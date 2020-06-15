rm(list=ls())
library(raster)
setwd("~/Documents/DTU/6. Semester/Bachelorprojekt/Data_Ellen_Maria/LandcoverMap/Extract_masks")
Vine <- raster("22_mask.tif")

setwd("~/Documents/DTU/6. Semester/Bachelorprojekt/Data_Ellen_Maria/KG_climate/Extract_masks")
Bsk <- raster("Bsk_MASK.tif")

#### Vine ####
n <- dim(Vine)[1] # number of rows
m <- dim(Vine)[2] # number of columns 

# matrix to vector w/ zero as NaN
vec1 <- as.vector(Vine)

col_idx <- rep(c(1:m), times = n)
row_idx <- numeric()
for (i in 1:n) {
  seq <- rep(c(i),times = m)
  row_idx <- cbind(row_idx, seq)
}
row_idx <- as.vector(row_idx)

# Value, row number and column number as matrix 
VRC_vine <- matrix(c(vec1,row_idx,col_idx), ncol =3, dimnames = list(NULL, c("val","rowNr","colNr")))
head(VRC_vine)

#### Bsk ####
n <- dim(Bsk)[1] # number of rows
m <- dim(Bsk)[2] # number of columns 

# matrix to vector w/ zero as NaN
vec2 <- as.vector(Bsk)

col_idx <- rep(c(1:m), times = n)
row_idx <- numeric()
for (i in 1:n) {
  seq <- rep(c(i),times = m)
  row_idx <- cbind(row_idx, seq)
}
row_idx <- as.vector(row_idx)

# Value, row number and column number as matrix 
VRC_Bsk <- matrix(c(vec2,row_idx,col_idx), ncol =3, dimnames = list(NULL, c("val","rowNr","colNr")))
head(VRC_Bsk)

#### Only extracting the vineyards og Bsk ####
VRC_Bsk[,1] <- VRC_Bsk[,1]*VRC_vine[,1]
#VRC_Bsk[VRC_Bsk[,1]==0]=NaN

Bsk_Vineyard = raster(ncol=ncol(Bsk),
                      nrow=nrow(Bsk),
                      xmn=xmin(Bsk),
                      xmx=xmax(Bsk),
                      ymn=ymin(Bsk),
                      ymx=ymax(Bsk))
projection(Bsk_Vineyard) = projection(Bsk)


V1 <- VRC_Bsk[VRC_Bsk[,1]==1,]
#V1 = V1[complete.cases(V1),]
Bsk_Vineyard[cbind(V1[,2], V1[,3])]=1

V2 <- VRC_Bsk[VRC_Bsk[,1]==0,]
#V2 = V2[complete.cases(V2),]
Bsk_Vineyard[cbind(V2[,2], V2[,3])]=NaN

####
writeRaster(Bsk_Vineyard, "~/Documents/DTU/6. Semester/Bachelorprojekt/Data_Ellen_Maria/KG_climate/Extract_masks/Bsk_22_MASK.tif", overwrite = TRUE)

#### PLOT ####
rm(list=ls())
library(raster)
setwd("~/Documents/DTU/6. Semester/Bachelorprojekt/Data_Ellen_Maria/KG_climate/Extract_masks")
Bsk_Vineyard <- raster("Bsk_22_MASK.tif")

png(file = "~/Documents/DTU/6. Semester/Bachelorprojekt/Data_Ellen_Maria/KG_climate/Extract_masks/VisualizeBsk_PermanentCrops.png")
plot(Bsk_Vineyard, main = "Visualization of Bsk_PermanentCrops")
dev.off()





