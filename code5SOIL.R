R
setwd('~/Documents/SOILMANUSCRIPTdrive-download-20180110T184357Z-001/SOILD_v4/data/')

library(raster)

name <- c('ARGENTINA', 'BELIZE', 'BOLIVIA','CHILE','COLOMBIA',
'COSTA RICA','CUBA', 'ECUADOR', 'GUATEMALA', 'JAMAICA', 'MEXICO', 'NICARAGUA', 'PANAMA', 'PERU', 'SURINAME', 'URUGUAY', 'VENEZUELA','BRAZIL')


lis <- list.files(pattern='rds')
lis <- lis[c(grep('trendSPLINE-ERRORmapRFsp.rds', lis),
grep("BraziltrendSPLINE-ERRORmapRFspAGG4.rds", lis))]

ref <- raster('SOC-ensembleRESIDUALmap-country-specific.tif')

for(i in 1:length(lis)){
x <- stack(readRDS(lis[i]))
cou <- getData("GADM", country=name[i], level=1)
cou <- spTransform(cou, CRS=projection (x))
x <- mask (x , cou)
x <- resample (x, ref)
writeRaster(x, file=paste0(name[i], 'trendSPLINE-ERRORmapRFsp.tif'), 
overwrite=TRUE)
print(name[i])}



ListRasters <- function(list_names) {
  raster_list <- list() # initialise the list of rasters
   for (i in 1:(length(list_names))){ 
    grd_name <- list_names[i] # list_names contains all the names of the images in .grd format
    raster_file <- stack(grd_name, bands=3)###REPEAT here band2 and 3
   }
  raster_list <- append(raster_list, raster_file) # update raster_list at each iteration
}

wgs84.tif.list <- list.files(path=getwd(), pattern=glob2rx("*trendSPLINE-ERRORmapRFsp.tif"), full.names=T,recursive=F)

list_names <- NULL
for (i in 1:length(wgs84.tif.list)) {
  list_names <- c(list_names, wgs84.tif.list[i])
}

raster.list <-sapply(list_names, FUN = ListRasters)

raster.list$fun <- mean

names(raster.list) <- NULL

raster.list$fun <- mean
mos <- do.call(mosaic, raster.list)


writeRaster(mos, file='SOC-MAEerrorMapRFsp-country-specific.tif')

#repeat with band 3 to get the error range

writeRaster(mos, file='SOC-MAEerrorRangeMapRFsp-country-specific.tif')










