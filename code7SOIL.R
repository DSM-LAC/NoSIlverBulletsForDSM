library(raster)
serena <- raster('la_classification_with_nodata_clump8_elim4_.tif')
serena <- projectRaster(serena, socs, method='ngb' )

socs <- stack (socs, serena)
names(socs[[9]]) <- 'lu'

table <- socs
table <- as.data.frame(table, xy=TRUE)
table$lu <- as.factor(table$lu)
lu <- read.csv('report_temp.csv')[2]

lev <- levels(table$lu)
res <- table[-c(1,2)][1,][-1,]

for (i in 1:length(lev)){
ta <- table[table$lu==lev[i],]
ap <- apply (na.omit(ta[-c(1,2,11)]), 2, sum) * 2500 * 1e-9
tot <- (data.frame(t(ap)))
tot$lu <- lu[i,1]
res <- rbind(res, tot)
print(lu[i,1])
}


table_sum <- table
table_sum$gs <- table_sum$gs * 2500
table_sum$hw <- table_sum$hw * 2500
table_sum$sg <- table_sum$sg * 2500
table_sum$layer <- table_sum$layer * 2500
table_sum$countryEnsemble <- table_sum$countryEnsemble *2500

lc_01 <-subset(table_sum, table_sum$la_classification_with_nodata_clump8_elim4_ == 1)
lc_soc_report[1,3] <- sum(na.omit(lc_01$gs))
lc_soc_report[1,4] <- sum(na.omit(lc_01$hw))
lc_soc_report[1,5] <- sum(na.omit(lc_01$sg))
lc_soc_report[1,6] <- sum(na.omit(lc_01$layer))
lc_soc_report[1,7] <- sum(na.omit(lc_01$filledSOILensCountryspecificSOC))

lc_02 <-subset(table_sum, table_sum$la_classification_with_nodata_clump8_elim4_ == 2)
lc_soc_report[2,3] <- sum(na.omit(lc_02$gs))
lc_soc_report[2,4] <- sum(na.omit(lc_02$hw))
lc_soc_report[2,5] <- sum(na.omit(lc_02$sg))
lc_soc_report[2,6] <- sum(na.omit(lc_02$layer))
lc_soc_report[2,7] <- sum(na.omit(lc_02$filledSOILensCountryspecificSOC))

lc_03 <-subset(table_sum, table_sum$la_classification_with_nodata_clump8_elim4_ == 3)
lc_soc_report[3,3] <- sum(na.omit(lc_03$gs))
lc_soc_report[3,4] <- sum(na.omit(lc_03$hw))
lc_soc_report[3,5] <- sum(na.omit(lc_03$sg))
lc_soc_report[3,6] <- sum(na.omit(lc_03$layer))
lc_soc_report[3,7] <- sum(na.omit(lc_03$filledSOILensCountryspecificSOC))

lc_04 <-subset(table_sum, table_sum$la_classification_with_nodata_clump8_elim4_ == 4)
lc_soc_report[4,3] <- sum(na.omit(lc_04$gs))
lc_soc_report[4,4] <- sum(na.omit(lc_04$hw))
lc_soc_report[4,5] <- sum(na.omit(lc_04$sg))
lc_soc_report[4,6] <- sum(na.omit(lc_04$layer))
lc_soc_report[4,7] <- sum(na.omit(lc_04$filledSOILensCountryspecificSOC))

lc_05 <-subset(table_sum, table_sum$la_classification_with_nodata_clump8_elim4_ == 5)
lc_soc_report[5,3] <- sum(na.omit(lc_05$gs))
lc_soc_report[5,4] <- sum(na.omit(lc_05$hw))
lc_soc_report[5,5] <- sum(na.omit(lc_05$sg))
lc_soc_report[5,6] <- sum(na.omit(lc_05$layer))
lc_soc_report[5,7] <- sum(na.omit(lc_05$filledSOILensCountryspecificSOC))

lc_06 <-subset(table_sum, table_sum$la_classification_with_nodata_clump8_elim4_ == 6)
lc_soc_report[6,3] <- sum(na.omit(lc_06$gs))
lc_soc_report[6,4] <- sum(na.omit(lc_06$hw))
lc_soc_report[6,5] <- sum(na.omit(lc_06$sg))
lc_soc_report[6,6] <- sum(na.omit(lc_06$layer))
lc_soc_report[6,7] <- sum(na.omit(lc_01$filledSOILensCountryspecificSOC))

lc_07 <-subset(table_sum, table_sum$la_classification_with_nodata_clump8_elim4_ == 7)
lc_soc_report[7,3] <- sum(na.omit(lc_07$gs))
lc_soc_report[7,4] <- sum(na.omit(lc_07$hw))
lc_soc_report[7,5] <- sum(na.omit(lc_07$sg))
lc_soc_report[7,6] <- sum(na.omit(lc_07$layer))
lc_soc_report[7,7] <- sum(na.omit(lc_07$filledSOILensCountryspecificSOC))

lc_08 <-subset(table_sum, table_sum$la_classification_with_nodata_clump8_elim4_ == 8)
lc_soc_report[8,3] <- sum(na.omit(lc_08$gs))
lc_soc_report[8,4] <- sum(na.omit(lc_08$hw))
lc_soc_report[8,5] <- sum(na.omit(lc_08$sg))
lc_soc_report[8,6] <- sum(na.omit(lc_08$layer))
lc_soc_report[8,7] <- sum(na.omit(lc_08$filledSOILensCountryspecificSOC))

lc_09 <-subset(table_sum, table_sum$la_classification_with_nodata_clump8_elim4_ == 9)
lc_soc_report[9,3] <- sum(na.omit(lc_09$gs))
lc_soc_report[9,4] <- sum(na.omit(lc_09$hw))
lc_soc_report[9,5] <- sum(na.omit(lc_09$sg))
lc_soc_report[9,6] <- sum(na.omit(lc_09$layer))
lc_soc_report[9,7] <- sum(na.omit(lc_09$filledSOILensCountryspecificSOC))

lc_10 <-subset(table_sum, table_sum$la_classification_with_nodata_clump8_elim4_ == 10)
lc_soc_report[10,3] <- sum(na.omit(lc_10$gs))
lc_soc_report[10,4] <- sum(na.omit(lc_10$hw))
lc_soc_report[10,5] <- sum(na.omit(lc_10$sg))
lc_soc_report[10,6] <- sum(na.omit(lc_10$layer))
lc_soc_report[10,7] <- sum(na.omit(lc_10$filledSOILensCountryspecificSOC))

lc_11 <-subset(table_sum, table_sum$la_classification_with_nodata_clump8_elim4_ == 11)
lc_soc_report[11,3] <- sum(na.omit(lc_11$gs))
lc_soc_report[11,4] <- sum(na.omit(lc_11$hw))
lc_soc_report[11,5] <- sum(na.omit(lc_11$sg))
lc_soc_report[11,6] <- sum(na.omit(lc_11$layer))
lc_soc_report[11,7] <- sum(na.omit(lc_11$filledSOILensCountryspecificSOC))

lc_12 <-subset(table_sum, table_sum$la_classification_with_nodata_clump8_elim4_ == 12)
lc_soc_report[12,3] <- sum(na.omit(lc_12$gs))
lc_soc_report[12,4] <- sum(na.omit(lc_12$hw))
lc_soc_report[12,5] <- sum(na.omit(lc_12$sg))
lc_soc_report[12,6] <- sum(na.omit(lc_12$layer))
lc_soc_report[12,7] <- sum(na.omit(lc_12$filledSOILensCountryspecificSOC))

lc_13 <-subset(table_sum, table_sum$la_classification_with_nodata_clump8_elim4_ == 13)
lc_soc_report[13,3] <- sum(na.omit(lc_13$gs))
lc_soc_report[13,4] <- sum(na.omit(lc_13$hw))
lc_soc_report[13,5] <- sum(na.omit(lc_13$sg))
lc_soc_report[13,6] <- sum(na.omit(lc_13$layer))
lc_soc_report[13,7] <- sum(na.omit(lc_13$filledSOILensCountryspecificSOC))

lc_14 <-subset(table_sum, table_sum$la_classification_with_nodata_clump8_elim4_ == 14)
lc_soc_report[14,3] <- sum(na.omit(lc_14$gs))
lc_soc_report[14,4] <- sum(na.omit(lc_14$hw))
lc_soc_report[14,5] <- sum(na.omit(lc_14$sg))
lc_soc_report[14,6] <- sum(na.omit(lc_14$layer))
lc_soc_report[14,7] <- sum(na.omit(lc_14$filledSOILensCountryspecificSOC))

lc_15 <-subset(table_sum, table_sum$la_classification_with_nodata_clump8_elim4_ == 15)
lc_soc_report[15,3] <- sum(na.omit(lc_15$gs))
lc_soc_report[15,4] <- sum(na.omit(lc_15$hw))
lc_soc_report[15,5] <- sum(na.omit(lc_15$sg))
lc_soc_report[15,6] <- sum(na.omit(lc_15$layer))
lc_soc_report[15,7] <- sum(na.omit(lc_15$filledSOILensCountryspecificSOC))

lc_16 <-subset(table_sum, table_sum$la_classification_with_nodata_clump8_elim4_ == 16)
lc_soc_report[16,3] <- sum(na.omit(lc_16$gs))
lc_soc_report[16,4] <- sum(na.omit(lc_16$hw))
lc_soc_report[16,5] <- sum(na.omit(lc_16$sg))
lc_soc_report[16,6] <- sum(na.omit(lc_16$layer))
lc_soc_report[16,7] <- sum(na.omit(lc_16$filledSOILensCountryspecificSOC))

lc_17 <-subset(table_sum, table_sum$la_classification_with_nodata_clump8_elim4_ == 17)
lc_soc_report[17,3] <- sum(na.omit(lc_17$gs))
lc_soc_report[17,4] <- sum(na.omit(lc_17$hw))
lc_soc_report[17,5] <- sum(na.omit(lc_17$sg))
lc_soc_report[17,6] <- sum(na.omit(lc_17$layer))
lc_soc_report[17,7] <- sum(na.omit(lc_17$filledSOILensCountryspecificSOC))

lc_18 <-subset(table_sum, table_sum$la_classification_with_nodata_clump8_elim4_ == 18)
lc_soc_report[18,3] <- sum(na.omit(lc_18$gs))
lc_soc_report[18,4] <- sum(na.omit(lc_18$hw))
lc_soc_report[18,5] <- sum(na.omit(lc_18$sg))
lc_soc_report[18,6] <- sum(na.omit(lc_18$layer))
lc_soc_report[18,7] <- sum(na.omit(lc_18$filledSOILensCountryspecificSOC))

lc_19 <-subset(table_sum, table_sum$la_classification_with_nodata_clump8_elim4_ == 19)
lc_soc_report[19,3] <- sum(na.omit(lc_19$gs))
lc_soc_report[19,4] <- sum(na.omit(lc_19$hw))
lc_soc_report[19,5] <- sum(na.omit(lc_19$sg))
lc_soc_report[19,6] <- sum(na.omit(lc_19$layer))
lc_soc_report[19,7] <- sum(na.omit(lc_19$filledSOILensCountryspecificSOC))

lc_20 <-subset(table_sum, table_sum$la_classification_with_nodata_clump8_elim4_ == 20)
lc_soc_report[20,3] <- sum(na.omit(lc_20$gs))
lc_soc_report[20,4] <- sum(na.omit(lc_20$hw))
lc_soc_report[20,5] <- sum(na.omit(lc_20$sg))
lc_soc_report[20,6] <- sum(na.omit(lc_20$layer))
lc_soc_report[20,7] <- sum(na.omit(lc_20$filledSOILensCountryspecificSOC))

lc_21 <-subset(table_sum, table_sum$la_classification_with_nodata_clump8_elim4_ == 21)
lc_soc_report[21,3] <- sum(na.omit(lc_21$gs))
lc_soc_report[21,4] <- sum(na.omit(lc_21$hw))
lc_soc_report[21,5] <- sum(na.omit(lc_21$sg))
lc_soc_report[21,6] <- sum(na.omit(lc_21$layer))
lc_soc_report[21,7] <- sum(na.omit(lc_21$filledSOILensCountryspecificSOC))

lc_22 <-subset(table_sum, table_sum$la_classification_with_nodata_clump8_elim4_ == 22)
lc_soc_report[22,3] <- sum(na.omit(lc_22$gs))
lc_soc_report[22,4] <- sum(na.omit(lc_22$hw))
lc_soc_report[22,5] <- sum(na.omit(lc_22$sg))
lc_soc_report[22,6] <- sum(na.omit(lc_22$layer))
lc_soc_report[22,7] <- sum(na.omit(lc_22$filledSOILensCountryspecificSOC))

lc_23 <-subset(table_sum, table_sum$la_classification_with_nodata_clump8_elim4_ == 23)
lc_soc_report[23,3] <- sum(na.omit(lc_23$gs))
lc_soc_report[23,4] <- sum(na.omit(lc_23$hw))
lc_soc_report[23,5] <- sum(na.omit(lc_23$sg))
lc_soc_report[23,6] <- sum(na.omit(lc_23$layer))
lc_soc_report[23,7] <- sum(na.omit(lc_23$filledSOILensCountryspecificSOC))

lc_24 <-subset(table_sum, table_sum$la_classification_with_nodata_clump8_elim4_ == 24)
lc_soc_report[24,3] <- sum(na.omit(lc_24$gs))
lc_soc_report[24,4] <- sum(na.omit(lc_24$hw))
lc_soc_report[24,5] <- sum(na.omit(lc_24$sg))
lc_soc_report[24,6] <- sum(na.omit(lc_24$layer))
lc_soc_report[24,7] <- sum(na.omit(lc_24$filledSOILensCountryspecificSOC))

lc_soc_report$gs <- lc_soc_report$gs *1e-9
lc_soc_report$hw <- lc_soc_report$hw *1e-9
lc_soc_report$sg <- lc_soc_report$sg *1e-9
lc_soc_report$ens <- lc_soc_report$ens *1e-9
lc_soc_report$country_spfc <- lc_soc_report$country_spfc *1e-9


lc_soc_report[25,3] <- sum(na.omit(lc_soc_report$gs))
lc_soc_report[25,4] <- sum(na.omit(lc_soc_report$hw))
lc_soc_report[25,5] <- sum(na.omit(lc_soc_report$sg))
lc_soc_report[25,6] <- sum(na.omit(lc_soc_report$ens))
lc_soc_report[25,7] <- sum(na.omit(lc_soc_report$country_spfc))
