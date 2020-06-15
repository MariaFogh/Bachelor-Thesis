rm(list=ls())
library(raster)
setwd("~/Documents/DTU/6. Semester/Bachelorprojekt/Data_Ellen_Maria/LandcoverMap")
landuse <- raster("landuse_Spain_CLC_2012_1100m.tif")

n <- dim(landuse)[1] # number of rows
m <- dim(landuse)[2] # number of columns 

# matrix to vector w/ zero as NaN
vec <- as.vector(landuse)
vec[is.na(vec)]=0

col_idx <- rep(c(1:m), times = n)
row_idx <- numeric()
for (i in 1:n) {
  seq <- rep(c(i),times = m)
  row_idx <- cbind(row_idx, seq)
}
row_idx <- as.vector(row_idx)

# Value, row number and column number as matrix 
VRC <- matrix(c(vec,row_idx,col_idx), ncol =3, dimnames = list(NULL, c("val","rowNr","colNr")))

setwd("~/Documents/DTU/6. Semester/Bachelorprojekt/Data_Ellen_Maria")
mask <- raster("mask_LAI_KG.tif")

## Landuse as factor: Only extracting the vineyards
V1 <- VRC[VRC[,1]==311,]
V1 = V1[complete.cases(V1),]
landuse[cbind(V1[,2], V1[,3])]=1

V2 <- VRC[VRC[,1]==312,]
V2 = V2[complete.cases(V2),]
landuse[cbind(V2[,2], V2[,3])]=1

V3 <- VRC[VRC[,1]==313,]
V3 = V3[complete.cases(V3),]
landuse[cbind(V3[,2], V3[,3])]=1

V4 <- VRC[VRC[,1]<(310),]
V4 = V4[complete.cases(V4),]
landuse[cbind(V4[,2], V4[,3])]=NaN

V3 <- VRC[VRC[,1] >= 321,]
V3 = V3[complete.cases(V3),]
landuse[cbind(V3[,2], V3[,3])]=NaN

V4 <- VRC[VRC[,1]==999,]
V4 = V4[complete.cases(V4),]
landuse[cbind(V4[,2], V4[,3])]=NaN

landuse <- landuse*mask

writeRaster(landuse, "~/Documents/DTU/6. Semester/Bachelorprojekt/Data_Ellen_Maria/LandcoverMap/Extract_masks/31_mask_NEW.tif", overwrite = TRUE)

#### PLOT ####
rm(list=ls())
library(raster)
setwd("~/Documents/DTU/6. Semester/Bachelorprojekt/Data_Ellen_Maria/LandcoverMap/Extract_masks")
mask <- raster("31_mask.tif")

png(file = "~/Documents/DTU/6. Semester/Bachelorprojekt/Data_Ellen_Maria/LandcoverMap/Extract_masks/Visualise31.png")
plot(mask, main = "Visualization of Forests")
dev.off()




