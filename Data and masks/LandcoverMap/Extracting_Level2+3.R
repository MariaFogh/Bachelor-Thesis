## Extracting Level 2 to mask-file

rm(list=ls())
library(raster)
setwd("~/Documents/DTU/6. Semester/Bachelorprojekt/Data_Ellen_Maria/LandcoverMap")
L2_mask <- raster("landuse_L1.tif")

n <- dim(L2_mask)[1] # number of rows
m <- dim(L2_mask)[2] # number of columns 

# matrix to vector w/ zero as NaN
vec <- as.vector(L2_mask)

col_idx <- rep(c(1:m), times = n)
row_idx <- numeric()
for (i in 1:n) {
  seq <- rep(c(i),times = m)
  row_idx <- cbind(row_idx, seq)
}
row_idx <- as.vector(row_idx)

# Value, row number and column number as matrix 
VRC <- matrix(c(vec,row_idx,col_idx), ncol =3, dimnames = list(NULL, c("val","rowNr","colNr")))

## Level 2 and 3 = ones
V1 <- VRC[VRC[,1]==3,]
V1 = V1[complete.cases(V1),]
L2_mask[cbind(V1[,2], V1[,3])]=1

V2 <- VRC[VRC[,1]==2,] 
V2 = V2[complete.cases(V2),]
L2_mask[cbind(V2[,2], V2[,3])]=1

# Rest of levels is zero
V3 <- VRC[VRC[,1]==1,] 
V3 = V3[complete.cases(V3),]
L2_mask[cbind(V3[,2], V3[,3])]=NaN

V4 <- VRC[VRC[,1]==4,] 
V4 = V4[complete.cases(V4),]
L2_mask[cbind(V4[,2], V4[,3])]=NaN

V5 <- VRC[VRC[,1]==5,] 
V5 = V5[complete.cases(V5),]
L2_mask[cbind(V5[,2], V5[,3])]=NaN

V6 <- VRC[VRC[,1]==999,] 
V6 = V6[complete.cases(V6),]
L2_mask[cbind(V6[,2], V6[,3])]=NaN

setwd("~/Documents/DTU/6. Semester/Bachelorprojekt/Data_Ellen_Maria")
mask <- raster("mask_LAI.tif")

L2_mask <- L2_mask*mask

writeRaster(L2_mask, "~/Documents/DTU/6. Semester/Bachelorprojekt/Data_Ellen_Maria/LandcoverMap/Extract_masks/Level2+3_mask.tif", overwrite = TRUE)

#### Test plot ####
rm(list=ls())
library(raster)
setwd("~/Documents/DTU/6. Semester/Bachelorprojekt/Data_Ellen_Maria/LandcoverMap/Extract_masks")
Level2_3 <- raster("Level2+3_mask.tif")

# Correlation between ET_land and the explanatory variables 
png(file = "~/Documents/DTU/6. Semester/Bachelorprojekt/Data_Ellen_Maria/LandcoverMap/Extract_masks/VisualiseLevel2.png")
plot(Level2_3, main = "Visualisation of Level 2: Agricultural areas")
dev.off()






