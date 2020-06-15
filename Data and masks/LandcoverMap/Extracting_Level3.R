## Extracting Level 3 to mask-file

rm(list=ls())
library(raster)
setwd("~/Documents/DTU/6. Semester/Bachelorprojekt/Data_Ellen_Maria/LandcoverMap")
L3_mask <- raster("landuse_L1.tif")

n <- dim(L3_mask)[1] # number of rows
m <- dim(L3_mask)[2] # number of columns 

# matrix to vector w/ zero as NaN
vec <- as.vector(L3_mask)

col_idx <- rep(c(1:m), times = n)
row_idx <- numeric()
for (i in 1:n) {
  seq <- rep(c(i),times = m)
  row_idx <- cbind(row_idx, seq)
}
row_idx <- as.vector(row_idx)

# Value, row number and column number as matrix 
VRC <- matrix(c(vec,row_idx,col_idx), ncol =3, dimnames = list(NULL, c("val","rowNr","colNr")))

## Level 3 = ones
V1 <- VRC[VRC[,1]==3,]
V1 = V1[complete.cases(V1),]
L3_mask[cbind(V1[,2], V1[,3])]=1

# Rest of levels is zero
V2 <- VRC[VRC[,1]==1,] 
V2 = V2[complete.cases(V2),]
L3_mask[cbind(V2[,2], V2[,3])]=NaN

V3 <- VRC[VRC[,1]==2,] 
V3 = V3[complete.cases(V3),]
L3_mask[cbind(V3[,2], V3[,3])]=NaN

V4 <- VRC[VRC[,1]==4,] 
V4 = V4[complete.cases(V4),]
L3_mask[cbind(V4[,2], V4[,3])]=NaN

V5 <- VRC[VRC[,1]==5,] 
V5 = V5[complete.cases(V5),]
L3_mask[cbind(V5[,2], V5[,3])]=NaN

V6 <- VRC[VRC[,1]==999,] 
V6 = V6[complete.cases(V6),]
L3_mask[cbind(V6[,2], V6[,3])]=NaN


setwd("~/Documents/DTU/6. Semester/Bachelorprojekt/Data_Ellen_Maria")
mask <- raster("mask_LAI.tif")

L3_mask <- L3_mask*mask

writeRaster(L3_mask, "~/Documents/DTU/6. Semester/Bachelorprojekt/Data_Ellen_Maria/LandcoverMap/Extract_masks/Level3_mask.tif", overwrite = TRUE)

#### Test plot ####
rm(list=ls())
library(raster)
setwd("~/Documents/DTU/6. Semester/Bachelorprojekt/Data_Ellen_Maria/LandcoverMap/Extract_masks")
Level3 <- raster("Level3_mask.tif")

# Plot
png(file = "~/Documents/DTU/6. Semester/Bachelorprojekt/Data_Ellen_Maria/LandcoverMap/Extract_masks/VisualiseLevel3.png")
plot(Level3, main = "Visualisation of Level 3: Forest and semi natural areas")
dev.off()




