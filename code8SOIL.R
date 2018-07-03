setwd("~/Documents/SOILMANUSCRIPTdrive-download-20180110T184357Z-001/SOILD_v4/data")

library(caret)
library(doMC)
library(doParallel)
library(reshape)
library(Metrics)
library(caretEnsemble)
library(snowfall)
library(raster)
library(automap)
library(ranger)
library('sp')
library('gstat')
library('parallel')
library(quantregForest)

dat <- read.csv('LAC.csv')
(lev <- levels (dat$country))

name <- c('ARGENTINA', 'BELIZE', 'BOLIVIA','BRAZIL','CHILE','COLOMBIA',
 'COSTA RICA','CUBA', 'DOMINICAN REPUBLIC','ECUADOR', 'GUATEMALA', 'HONDURAS', 'JAMAICA', 'MEXICO', 'NICARAGUA', 'PANAMA', 'PERU', 'SURINAME', 'URUGUAY', 'VENEZUELA')


(lev <- lev[c(2)])
(name <- name[c(2)])

covs <- stack('WG-TOPO-LAC.tif')
names(covs) <- readRDS("namesWG-TOPO-LAC.rds")

for (i in 1:length(lev)){
#i=16
cou <- getData("GADM", country=name[i], level=1)
cou <- spTransform(cou, CRS=projection (covs))
co <- crop (covs , cou)
print('masking covariates, done!')
d <- dat
d <- d[d$country== lev[i],]
d <- d[c(4:5, 9)]
d$OCSKGM <- log(d$OCSKGM)
dsp <- d
coordinates(dsp) <- ~X+Y
idx <- seq(1:dim(co)[3])
co <- setZ(co, idx)
s.list <- unstack(co)
names(s.list) <- names(co)

# Now, create a R cluster using all the machine cores minus one
sfInit(parallel=TRUE, cpus=parallel:::detectCores()-3)
# Load the required packages inside the cluster
sfLibrary(raster)
sfLibrary(sp)
# Run parallelized 'extract' function and stop cluster
e <- sfSapply(s.list, extract, y=dsp)
sfStop()

d <- cbind(d, data.frame(e))
d$ln2dms3a <- NULL
d$lnmdms3a <- NULL
d$sinks <- NULL  
d$land <- NULL
d$cntgad3a <- NULL

d <- na.omit(d)

inTrain <- createDataPartition(y = d$OCSKGM, p = .75, list = FALSE)
training <- d[ inTrain,]
testing <- d[-inTrain,]

set.seed(102)
ctrl <- trainControl(savePred=T, method="repeatedcv", number=5, repeats=5)

cl <- makeCluster(detectCores(), type='SOCK')
registerDoParallel(cl)
models <- caretList(training[-c(1,2,3)], training[,3], trControl=ctrl ,
methodList=c("glm", "rf", "pls","kknn", "svmLinear"))
ens <- caretEnsemble(models)
stopCluster(cl = cl)
#ens <- caretStack(models, method="bartMachine")

#ens <- readRDS('caretEnsemble-LAC.rds')
beginCluster()
(predEns <- clusterR(co[[names(d[-1])]], predict, args=list(model=ens, keepNA=FALSE)))
writeRaster(predEns, file=paste0(lev[i], 'V2-LACensembleSOC.tif'), 
overwrite=TRUE) 
#endCluster()

l <- list(models, ens, training, testing)
saveRDS(l, file=paste0(lev[i], 'V2-LACensembleSOC.rds'))
print (i)
print(paste0(lev[i], 'f1 done!'))

##
###error
#l <- readRDS("BrazilLACensembleSOC.rds")
#lis <- list.files(pattern='rds')
#lis <- lis[grep('ensembleSOC.rds', lis)]
#name <- name[-c(9, 12)]
#lev <- lev[-c(9, 12)]
#for (i in 6:length(lis)){
#covs <- stack('WG-TOPO-LAC.tif')
#names(covs) <- readRDS("namesWG-TOPO-LAC.rds")
#cou <- getData("GADM", country=name[i], level=1)
#cou <- spTransform(cou, CRS=projection (covs))
#covs <- crop (covs , cou)
#l <- readRDS(lis[i])
#testing <- l[[4]]
#ens <- l[[2]]

testing$res <- (testing$OCSKGM - predict(ens, testing[-c(1,2,3)], keepNA=FALSE))

model <- quantregForest(y=testing$res, x=testing[-c(1,2,3,132)], ntree=500, keep.inbag=TRUE)
#beginCluster(6,type="SOCK")
#Estimate model uncertainty
unc <- clusterR(co[[names(testing[-c(1,2,3,132)])]], predict, args=list(model=model,what=sd))
writeRaster(unc, file=paste0(lev[i], 'RESIDUALmapQRF-LACensembleSOC.tif'), 
overwrite=TRUE) 

print (i)
print(paste0(lev[i], 'f2 done!'))

d <- dat
d <- d[d$country== lev[i],]
print(i)
err  <- data.frame(d[c('X','Y','meaERROR')])
coordinates(err) <- ~X+Y

#c1 <- raster("BrazilRESIDUALmapQRF-LACensembleSOC.tif")
#c1 <-  aggregate(c1,2, mean)	

#c1 <- mask(c1, cou)
#c1 <- as(c1, 'SpatialPixelsDataFrame')

c1 <- as(aggregate(unc,2, mean), 'SpatialPixelsDataFrame')
proj4string(err) <- CRS(projection(c1))

grid.dist0 <- GSIF::buffer.dist(err["meaERROR"], c1[1], as.factor(1:nrow(err)))

#RFsp


dn0 <- paste(names(grid.dist0), collapse="+")
fm0 <- as.formula(paste("meaERROR ~ ", dn0))
fm0

ov.meaERROR <- over(err["meaERROR"], grid.dist0)
rm.meaERROR <- cbind(err@data["meaERROR"], ov.meaERROR)

m.meaERROR <- ranger(fm0, na.omit(rm.meaERROR), quantreg=TRUE, num.trees=150, seed=1)

m.meaERROR
quantiles = c((1-.682)/2, 0.5, 1-(1-.682)/2)
meaERROR.rfd <- predict(m.meaERROR, grid.dist0@data, type="quantiles", quantiles=quantiles)$predictions
str(meaERROR.rfd)

c1$meaERROR_rfd = meaERROR.rfd[,2]
c1$meaERROR_rfd_range = (meaERROR.rfd[,3]-meaERROR.rfd[,1])/2

saveRDS(c1, file=paste0(lev[i],'trendSPLINE-ERRORmapRFspAGG4.rds'))

print(lev[i])
print('done!')
endCluster()

}
#























#}
#l <- readRDS("BrazilLACensembleSOC.rds")
#x <- l[[2]][2][[1]][11]
#w <- (x[[1]][[1]])[-1]
#tr <- l[[4]]
#tr$pre <- rowWeightedMeans(predict(l[[1]], tr), na.rm=TRUE, w=w*w)
#tr$res <- tr$OCSKGM - tr$pre

