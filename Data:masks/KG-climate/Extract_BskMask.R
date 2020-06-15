rm(list=ls())
library(raster)
setwd("~/Documents/DTU/6. Semester/Bachelorprojekt/Data_Ellen_Maria/KG_climate")
KG <- raster("KG_ClimateMap_Aligned.tif")

n <- dim(KG)[1] # number of rows
m <- dim(KG)[2] # number of columns 

# matrix to vector w/ zero as NaN
vec <- as.vector(KG)

col_idx <- rep(c(1:m), times = n)
row_idx <- numeric()
for (i in 1:n) {
  seq <- rep(c(i),times = m)
  row_idx <- cbind(row_idx, seq)
}
row_idx <- as.vector(row_idx)

# Value, row number and column number as matrix 
VRC <- matrix(c(vec,row_idx,col_idx), ncol =3, dimnames = list(NULL, c("val","rowNr","colNr")))


## KG_climate as factor: Values
V1 <- VRC[VRC[,1]==7,]
V1 = V1[complete.cases(V1),]
KG[cbind(V1[,2], V1[,3])]=1

# Rest of levels is zero
V2 <- VRC[VRC[,1]==4,] 
V2 = V2[complete.cases(V2),]
KG[cbind(V2[,2], V2[,3])]=NaN

V3 <- VRC[VRC[,1]==5,] 
V3 = V3[complete.cases(V3),]
KG[cbind(V3[,2], V3[,3])]=NaN

V4 <- VRC[VRC[,1]==6,] 
V4 = V4[complete.cases(V4),]
KG[cbind(V4[,2], V4[,3])]=NaN

V5 <- VRC[VRC[,1]==8,] 
V5 = V5[complete.cases(V5),]
KG[cbind(V5[,2], V5[,3])]=NaN

V6 <- VRC[VRC[,1]==9,] 
V6 = V6[complete.cases(V6),]
KG[cbind(V6[,2], V6[,3])]=NaN

V7 <- VRC[VRC[,1]==10,] 
V7 = V7[complete.cases(V7),]
KG[cbind(V7[,2], V7[,3])]=NaN

V8 <- VRC[VRC[,1]==14,] 
V8 = V8[complete.cases(V8),]
KG[cbind(V8[,2], V8[,3])]=NaN

V9 <- VRC[VRC[,1]==15,] 
V9 = V9[complete.cases(V9),]
KG[cbind(V9[,2], V9[,3])]=NaN

V10 <- VRC[VRC[,1]==16,] 
V10 = V10[complete.cases(V10),]
KG[cbind(V10[,2], V10[,3])]=NaN

V11 <- VRC[VRC[,1]==17,] 
KG[cbind(V11[2], V11[3])]=NaN

V12 <- VRC[VRC[,1]==18,] 
V12 = V12[complete.cases(V12),]
KG[cbind(V12[,2], V12[,3])]=NaN

V13 <- VRC[VRC[,1]==19,] 
V13 = V13[complete.cases(V13),]
KG[cbind(V13[,2], V13[,3])]=NaN

V14 <- VRC[VRC[,1]==26,] 
V14 = V14[complete.cases(V14),]
KG[cbind(V14[,2], V14[,3])]=NaN

V15 <- VRC[VRC[,1]==27,] 
V15 = V15[complete.cases(V15),]
KG[cbind(V15[,2], V15[,3])]=NaN

V16 <- VRC[VRC[,1]==29,] 
V16 = V16[complete.cases(V16),]
KG[cbind(V16[,2], V16[,3])]=NaN

V17 <- VRC[VRC[,1]==0,] 
V17 = V17[complete.cases(V17),]
KG[cbind(V17[,2], V17[,3])]=v

setwd("~/Documents/DTU/6. Semester/Bachelorprojekt/Data_Ellen_Maria")
mask <- raster("mask_LAI_KG.tif")

KG <- KG*mask

writeRaster(KG, "~/Documents/DTU/6. Semester/Bachelorprojekt/Data_Ellen_Maria/KG_climate/Extract_masks/Bsk_MASK.tif", overwrite = TRUE)

#### PLOT ####
rm(list=ls())
library(raster)
setwd("~/Documents/DTU/6. Semester/Bachelorprojekt/Data_Ellen_Maria/KG_climate/Extract_masks")
KG_Bsk <- raster("Bsk_MASK.tif")

png(file = "~/Documents/DTU/6. Semester/Bachelorprojekt/Data_Ellen_Maria/KG_climate/Extract_masks/VisualiseKG_Bsk.png")
plot(KG_Bsk, main = "KG_climate: Bsk (Arid, steppe, cold)")
dev.off()

n <- dim(mask)[1] # number of rows
m <- dim(mask)[2] # number of columns 

# matrix to vector w/ zero as NaN
vec <- as.vector(mask)

col_idx <- rep(c(1:m), times = n)
row_idx <- numeric()
for (i in 1:n) {
  seq <- rep(c(i),times = m)
  row_idx <- cbind(row_idx, seq)
}
row_idx <- as.vector(row_idx)

# Value, row number and column number as matrix 
VRC_mask <- matrix(c(vec,row_idx,col_idx), ncol =3, dimnames = list(NULL, c("val","rowNr","colNr")))
VRC_mask[is.na(VRC_mask)]=0

setwd("~/Documents/DTU/6. Semester/Bachelorprojekt/Data_Ellen_Maria")
mask_k <- raster("mask.tif")

#### Bsk_Vine ####
n <- dim(mask_k)[1] # number of rows
m <- dim(mask_k)[2] # number of columns 

# matrix to vector w/ zero as NaN
vec1 <- as.vector(mask_k)

col_idx <- rep(c(1:m), times = n)
row_idx <- numeric()
for (i in 1:n) {
  seq <- rep(c(i),times = m)
  row_idx <- cbind(row_idx, seq)
}
row_idx <- as.vector(row_idx)

# Value, row number and column number as matrix 
VRC_mask_k <- matrix(c(vec1,row_idx,col_idx), ncol =3, dimnames = list(NULL, c("val","rowNr","colNr")))
head(VRC_mask_k)

#### Only extracting the vineyards og Bsk ####
VRC_mask_k[,1] <- VRC_mask_k[,1]*VRC_mask[,1]
#VRC_Bsk[VRC_Bsk[,1]==0]=NaN

V1 <- VRC_mask_k[VRC_mask_k[,1]==1,]
#V1 = V1[complete.cases(V1),]
mask[cbind(V1[,2], V1[,3])]=1

V2 <- VRC_mask_k[VRC_mask_k[,1]==0,]
#V2 = V2[complete.cases(V2),]
mask[cbind(V2[,2], V2[,3])]=0

plot(mask)

png(file = "~/Documents/DTU/6. Semester/Bachelorprojekt/Data_Ellen_Maria/Area_plots/Bsk_Vineyard.png")
plot(mask, main = "KG-climate: Bsk (Arid, steppe, cold)")
dev.off()



