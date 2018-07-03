R
setwd('~/Documents/SOILMANUSCRIPTdrive-download-20180110T184357Z-001/SOILD_v4/data/')

library(raster)

#name <- c('ARGENTINA', 'BELIZE', 'BOLIVIA','BRAZIL','CHILE','COLOMBIA',
 #'COSTA RICA','CUBA', 'ECUADOR', 'GUATEMALA', 'JAMAICA', 'MEXICO', #'NICARAGUA', 'PANAMA', 'PERU', 'SURINAME', 'URUGUAY', 'VENEZUELA')

 #lis <- list.files(pattern='tif')
 #lis <- lis[grep('V2-LACensembleSOC.tif', lis)]

#for(i in 1:length(lis)){
#x <- raster(lis[i])
#cou <- getData("GADM", country=name[i], level=1)
#cou <- spTransform(cou, CRS=projection (x))
#x <- mask (x , cou)
#writeRaster(x, file=paste0(name[i], 'maskedV2-LACensembleSOC.tif'), 
#overwrite=TRUE)
#print(name[i])}


ListRasters <- function(list_names) {
  raster_list <- list() # initialise the list of rasters
   for (i in 1:(length(list_names))){ 
    grd_name <- list_names[i] # list_names contains all the names of the images in .grd format
    raster_file <- stack(grd_name, bands=1)
   }
  raster_list <- append(raster_list, raster_file) # update raster_list at each iteration
}

wgs84.tif.list <- list.files(path=getwd(), pattern=glob2rx("*V2-LACensembleSOC.tif"), full.names=T,recursive=F)

list_names <- NULL
for (i in 1:length(wgs84.tif.list)) {
  list_names <- c(list_names, wgs84.tif.list[i])
}

raster.list <-sapply(list_names, FUN = ListRasters)

raster.list$fun <- mean

names(raster.list) <- NULL

raster.list$fun <- mean
mos <- do.call(mosaic, raster.list)

wgs84.tif.list <- list.files(path=getwd(), pattern=glob2rx("*maskedV2-LACensembleSOC.tif"), full.names=T,recursive=F)

list_names <- NULL
for (i in 1:length(wgs84.tif.list)) {
  list_names <- c(list_names, wgs84.tif.list[i])
}

raster.list <-sapply(list_names, FUN = ListRasters)

raster.list$fun <- mean

names(raster.list) <- NULL

raster.list$fun <- mean
mos2 <- do.call(mosaic, raster.list)



writeRaster(GSIF::merge(mos2, mos), file='SOC-ensemble-country-specific.tif')




