rm(list=ls())
library(raster)
setwd("~/Documents/DTU/6. Semester/Bachelorprojekt/Data_Ellen_Maria/KG_climate/Extract_masks")
Bsk_Vine <- raster("Bsk_Vineyard_mask.tif")

setwd("~/Documents/DTU/6. Semester/Bachelorprojekt/Data_Ellen_Maria")
mask <- raster("mask_LAI.tif")

#### Bsk_Vine ####
n <- dim(Bsk_Vine)[1] # number of rows
m <- dim(Bsk_Vine)[2] # number of columns 

# matrix to vector w/ zero as NaN
vec1 <- as.vector(Bsk_Vine)

col_idx <- rep(c(1:m), times = n)
row_idx <- numeric()
for (i in 1:n) {
  seq <- rep(c(i),times = m)
  row_idx <- cbind(row_idx, seq)
}
row_idx <- as.vector(row_idx)

# Value, row number and column number as matrix 
VRC_Bsk_Vine <- matrix(c(vec1,row_idx,col_idx), ncol =3, dimnames = list(NULL, c("val","rowNr","colNr")))
head(VRC_Bsk_Vine)

#### Mask ####
n <- dim(mask)[1] # number of rows
m <- dim(mask)[2] # number of columns 

# matrix to vector w/ zero as NaN
vec2 <- as.vector(mask)

col_idx <- rep(c(1:m), times = n)
row_idx <- numeric()
for (i in 1:n) {
  seq <- rep(c(i),times = m)
  row_idx <- cbind(row_idx, seq)
}
row_idx <- as.vector(row_idx)

# Value, row number and column number as matrix 
VRC_mask <- matrix(c(vec2,row_idx,col_idx), ncol =3, dimnames = list(NULL, c("val","rowNr","colNr")))
head(VRC_mask)

plot(mask)

#### Only extracting the vineyards og Bsk ####
VRC_Bsk_Vine[,1] <- VRC_Bsk_Vine[,1]*VRC_mask[,1]
#VRC_Bsk[VRC_Bsk[,1]==0]=NaN

mask[mask==1] = 0
plot(mask)

V1 <- VRC_Bsk_Vine[VRC_Bsk_Vine[,1]==1,]
V1 = V1[complete.cases(V1),]
mask[cbind(V1[,2], V1[,3])]=1

V2 <- VRC_Bsk_Vine[VRC_Bsk_Vine[,1]==0,]
V2 = V2[complete.cases(V2),]
mask[cbind(V2[,2], V2[,3])]=0

plot(mask)

writeRaster(Bsk_Vineyard, "~/Documents/DTU/6. Semester/Bachelorprojekt/Data_Ellen_Maria/KG_climate/Extract_masks/Bsk_Vineyard_mask.tif", overwrite = TRUE)

#### PLOT ####
rm(list=ls())
library(raster)
setwd("~/Documents/DTU/6. Semester/Bachelorprojekt/Data_Ellen_Maria/KG_climate/Extract_masks")
Bsk_Vineyard <- raster("Bsk_Vineyard_mask.tif")

png(file = "~/Documents/DTU/6. Semester/Bachelorprojekt/Data_Ellen_Maria/KG_climate/Extract_masks/VisualizeBsk_Vineyard.png")
plot(Bsk_Vineyard, main = "VIneyards in KG-climate: Bsk")
dev.off()





