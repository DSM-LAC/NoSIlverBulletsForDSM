R 

setwd('~/Documents/SOILMANUSCRIPTdrive-download-20180110T184357Z-001/SOILD_v4/data')

library(raster)

socs<- stack('~/Downloads/gs-hw-sg-en.tif')

countryEnsemble <- exp(raster("~/Documents/SOILMANUSCRIPTdrive-download-20180110T184357Z-001/SOILD_v4/data/SOC-ensemble-country-specific.tif"))

socs <- stack(resample(countryEnsemble*10, socs), socs)

residualError <- exp(raster("~/Documents/SOILMANUSCRIPTdrive-download-20180110T184357Z-001/SOILD_v4/data/SOC-ensembleRESIDUALmap-country-specific.tif"))

socs <- stack(resample(residualError*10, socs), socs)

meaERROR <- raster("~/Documents/SOILMANUSCRIPTdrive-download-20180110T184357Z-001/SOILD_v4/data/SOC-MAEerrorMapRFsp-country-specific.tif")

socs <- stack(resample(meaERROR, socs), socs)

meaERRORrange <- raster("~/Documents/SOILMANUSCRIPTdrive-download-20180110T184357Z-001/SOILD_v4/data/SOC-MAEerrorRangeMapRFsp-country-specific.tif")

socs <- stack(resample(meaERRORrange, socs), socs)

names(socs) <- c('meaERRORrange', 'meaERROR', 
'residualError', 'countryEnsemble', 'gs', 'hw', 'sg', 'allEnsemble'
)

socsMask <- mask(socs, socs[[4]])
suCountryEns <- numeric()
suCountryAll <- numeric()
suSg <- numeric()
suGs <- numeric()
suHw <- numeric()


name <- c('ARGENTINA', 'BELIZE', 'BOLIVIA','BRAZIL','CHILE','COLOMBIA',
'COSTA RICA','CUBA', 'ECUADOR', 'GUATEMALA', 'JAMAICA', 'MEXICO', 'NICARAGUA', 'PANAMA', 'PERU', 'SURINAME', 'URUGUAY', 'VENEZUELA')

n <- c('ARG', 'BLZ', 'BOL','BRA','CHL','COL',
'CRI','CUB', 'ECU', 'GTM', 'JAM', 'MEX', 'NIC', 'PAN', 'PER', 'SUR', 'URY', 'VEN')


for (i in 1:length(name)){

cou <- getData("GADM", country=name[i], level=1)

cou <- spTransform(cou, CRS=projection (socs))

x1<- mask(socsMask[[4]], cou)
x2 <- mask(socsMask[[8]], cou)
x3 <- mask(socsMask[[7]], cou)
x4<- mask(socsMask[[5]], cou)
x5<- mask(socsMask[[6]], cou)

suCountryEns[i] <- (2500) * (cellStats(x1, sum)) * (1e-09)
suCountryAll[i] <- (2500) * (cellStats(x2, sum)) * (1e-09)
suSg[i] <- (2500) * (cellStats(x3, sum)) * (1e-09)
suGs[i] <- (2500) * (cellStats(x4, sum)) * (1e-09)
suHw[i] <- (2500) * (cellStats(x5, sum)) * (1e-09)

print(paste0(name[i], 'done!'))

}

dat <- data.frame(country = name,
ens = suCountryEns,
ensAll = suCountryAll,
sg = suSg,
gs = suGs,
hw = suHw)


library(plotrix)

par(mfrow=c(2,3))
for(i in 2:5){
slices <- dat[,i]
lbls <- dat$country
pie3D(slices,labels=lbls, labelcex = 0.8 ,start=0.5, explode=0.2,radius=.9,)
}
