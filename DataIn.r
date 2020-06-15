library(raster)
par(mfrow = c(1,1))
setwd("C:/Users/ellen/Documents/BA/Data_Ellen_Maria/KG_climate")
#KG_ <- raster("KG_ClimateMap.tif")
KG <- raster("KG_Climate_map_Aligned.tif")
mask_Bsk <- raster("Bsk_mask.tif")
mask_Cfb <- raster("Cfb_mask.tif")
mask_Csa <- raster("Csa_mask.tif")
mask_Csb <- raster("Csb_mask.tif")
mask_Bsk_vine <- raster("Bsk_Vineyard_MASK.tif")
mask_22 <- raster("22_mask.tif")
mask_31 <- raster("31_mask.tif")

#dim(KG)

setwd("C:/Users/ellen/Documents/BA/Data_Ellen_Maria")
mask <- raster( "Mask.TIF")
mask_LAI <- raster("mask_LAI.tif")
mask_LAI_KG <- raster("mask_LAI_KG.tif")

setwd("C:/Users/ellen/Documents/BA/Data_Ellen_Maria/Landcover Map")
landuse <- raster( "landuse_Spain_CLC_2012_1100m.TIF")
landuse_level1 <- raster("landuse_L1.TIF")
landuse_level2 <- raster("landuse_L2.TIF")
mask_landuse_C2 <- raster("Level2_mask.TIF")
mask_landuse_C3 <- raster("Level3_mask.TIF")
mask_landuse_C2_3 <- raster("Level2+3_mask.TIF")
mask_22_new <- raster("22_mask_NEW.tif")
mask_31_new <- raster("31_mask_NEW.tif")


setwd("C:/Users/ellen/Documents/BA/Data_Ellen_Maria/Year/climate/Trends")
names <- c("e_act","e_star","LAI","p","Rn","VPD")

dim_matrix <- matrix(ncol = length(names), nrow = 3)
colnames(dim_matrix) <- names
NaN_matrix <- matrix(ncol = length(names),nrow = 1)
colnames(NaN_matrix) <- names

for(i in 1:length(names)){
  imp_raster <- raster(paste(names[i], '.tif', sep=''))
  dim_matrix[,i] = dim(imp_raster)
  # dataframe from raster to name of data
  assign(  paste(names[i],'_year',sep=''), imp_raster )
  imp_raster[imp_raster == 0 ]=NaN
  NaN_nr <- sum(is.nan(getValues(imp_raster)))
  NaN_matrix[,i] <- NaN_nr
}



setwd("C:/Users/ellen/Documents/BA/Data_Ellen_Maria/Year/Outputs/trends")
ET_GF_year <- raster("ET_GF.tif")

print("Done")

