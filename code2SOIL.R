library(raster)

gs <- raster('~/Documents/Global comparison/GLOBALcomp/gs.tif')
hw <- raster('~/Documents/Global comparison/GLOBALcomp/hw.tif')
sg <- raster('~/Documents/Global comparison/GLOBALcomp/sg.tif')
ens <- raster("~/Documents/SOILMANUSCRIPTdrive-download-20180110T184357Z-001/SOILD_v4/data/LACensembleSOC.tif")

#kg m to ton ha
ens <- exp(ens) * 10

s <- stack ( 
	resample(crop(gs, ens), ens),  
	resample(crop(hw, ens), ens),
	resample(crop(sg, ens), ens), ens )

writeRaster(s, file='gs-hw-sg-en.tif')
