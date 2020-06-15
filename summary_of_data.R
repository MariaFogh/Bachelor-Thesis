###### Data Preparation ####
rm(list=ls())

### LAI INCLUDING PORTUGAL, FRANCE ECT
setwd("~/Documents/DTU/6. Semester/Bachelorprojekt/Data_Ellen_Maria/Year/climate/Trends")
LAI_all <- raster("LAI.tif")

#### Omitting KG 0 from Spain-mask ####
n <- dim(LAI_all)[1] # number of rows
m <- dim(LAI_all)[2] # number of columns 

# matrix to vector w/ zero as NaN
vec <- as.vector(LAI_all)

col_idx <- rep(c(1:m), times = n)
row_idx <- numeric()
for (i in 1:n) {
  seq <- rep(c(i),times = m)
  row_idx <- cbind(row_idx, seq)
}
row_idx <- as.vector(row_idx)

# Value, row number and column number as matrix 
VRC_LAI <- matrix(c(vec,row_idx,col_idx), ncol =3, dimnames = list(NULL, c("val","rowNr","colNr")))

## Overwrite the outliers with the mean of LAI
ocean <- VRC_LAI[VRC_LAI[,1]==0,] 
ocean <- ocean[complete.cases(ocean),]
LAI_all[cbind(ocean[,2], ocean[,3])] = NaN

png(file = "~/Documents/DTU/6. Semester/Bachelorprojekt/Data_Ellen_Maria/LAI_ori.png")
plot(LAI_all, main = "LAI contained in Spain, Portugal, ect.")
dev.off()

##### SOLUTION: USE THE MASK CONTAINING ONLY SPAIN (Ocean is NaN)
setwd("~/Documents/DTU/6. Semester/Bachelorprojekt/Data_Ellen_Maria/Year/climate/Trends")
LAI <- raster("LAI.tif")

setwd("~/Documents/DTU/6. Semester/Bachelorprojekt/Data_Ellen_Maria")
mask <- raster("Mask.tif")

LAI <- LAI*mask

png(file = "~/Documents/DTU/6. Semester/Bachelorprojekt/Data_Ellen_Maria/LAI_SPAIN.png")
plot(LAI, main = "LAI only contained in Spain")
dev.off()


#### SUMMARY: ALL DATA
rm(list = ls())
library(raster)

setwd("~/Documents/DTU/6. Semester/Bachelorprojekt/Data_Ellen_Maria")
source("random_uni.R")

setwd("~/Documents/DTU/6. Semester/Bachelorprojekt/Data_Ellen_Maria")
mask <- raster( "Mask.TIF")
RASTER1 <- mask
n <- length(mask)
uni_ran <- random_uni(RASTER1,n,1)

setwd("~/Documents/DTU/6. Semester/Bachelorprojekt/Data_Ellen_Maria/Year/climate/Trends")
names <- c("e_act","e_star","LAI","p","Rn","VPD")

dim_matrix <- matrix(ncol = length(names), nrow = 3)
colnames(dim_matrix) <- names
NaN_matrix <- matrix(ncol = length(names),nrow = 1)
colnames(NaN_matrix) <- names

for(i in 1:length(names)){
  imp_raster <- raster(paste(names[i], '.tif', sep=''))
  dim_matrix[,i] = dim(imp_raster)
  # dataframe from raster to name of data
  assign(  paste(names[i],'',sep=''), imp_raster )
  imp_raster[imp_raster == 0 ]=NaN
  NaN_nr <- sum(is.nan(getValues(imp_raster)))
  NaN_matrix[,i] <- NaN_nr
}

setwd("~/Documents/DTU/6. Semester/Bachelorprojekt/Data_Ellen_Maria/Year/Outputs/trends")
ET <- raster("ET_GF.tif")

setwd("~/Documents/DTU/6. Semester/Bachelorprojekt/Data_Ellen_Maria/KG_climate")
KG <- raster("KG_ClimateMap_Aligned.tif")

setwd("~/Documents/DTU/6. Semester/Bachelorprojekt/Data_Ellen_Maria/LandcoverMap")
landuse_L3 <- raster( "landuse_Spain_CLC_2012_1100m.TIF")
landuse_L1 <- raster("landuse_L1.TIF")
landuse_L2 <- raster("landuse_L2.TIF")

names <- c("landuse_L1", "landuse_L2", "landuse_L3", "KG","LAI","p","Rn","e_act","e_star", "VPD", "ET")

##### sample 1 ####
sample_1 <- uni_ran$coor_matrix[,1:3]
sample_1 <- sample_1[complete.cases(sample_1),]
# koordinater 
coor_1 <- cbind(sample_1[,2],sample_1[,3])
# Udtag værdier fra data 
VarMatrix <- matrix(ncol = (length(names)+2), nrow = length(sample_1[,1]))
for(i in 1:length(names)){
  # file in from global Env
  imp <- get(names[i], .GlobalEnv)
  VarMatrix[,i] <- imp[coor_1]
}
VarMatrix[,(length(names)+1):(length(names)+2)] <- coor_1
total_val_land_1 <- as.data.frame(VarMatrix)
names(total_val_land_1)<- c("landuse_L1", "landuse_L2", "landuse_L3", "KG", "LAI","p","Rn","e","e*","VPD", "ET","row","col")
head(total_val_land_1)

df <- total_val_land_1[c(5:11)]
head(df)
print(xtable(summary(df), type = "latex"), file = "~/Documents/DTU/6. Semester/Bachelorprojekt/Data_Ellen_Maria/summary_fullData.tex")

png(file = "~/Documents/DTU/6. Semester/Bachelorprojekt/Data_Ellen_Maria/pairs_fullData.png", width = 1000, height = 500)
pairs(df)
dev.off()

total_val_land_1$KG <- factor(total_val_land_1$KG)
w_KG = table(total_val_land_1$KG)

t_KG = as.data.frame(w_KG)
colnames(t_KG) <- c("KG-climate factors", "Frequency")
print(xtable(t_KG, type = "latex"), file = "~/Documents/DTU/6. Semester/Bachelorprojekt/Data_Ellen_Maria/KG_countFullData.tex")

png(file = "~/Documents/DTU/6. Semester/Bachelorprojekt/Data_Ellen_Maria/hist_allDataKG.png", width = 1000, height = 560)
ggplot(data = total_val_land_1, aes(x=KG)) + geom_bar() + 
  geom_text(stat='count', aes(label=..count..), vjust=-1) + 
  ggtitle("Frequency distribution of KG-climate factors in Spain") +
  labs(y="Count", x = "KG-climate factors") +
  theme(text = element_text(size=15))
dev.off()

total_val_land_1$landuse_L1 <- factor(total_val_land_1$landuse_L1)
w_landuse_L1 = table(total_val_land_1$landuse_L1)

t_landuse_L1 = as.data.frame(w_landuse_L1)
colnames(t_landuse_L1) <- c("Land cover factors (Level 1)", "Frequency")

print(xtable(t_landuse_L1, type = "latex"), file = "~/Documents/DTU/6. Semester/Bachelorprojekt/Data_Ellen_Maria/landuse_L1_countFullData.tex")

png(file = "~/Documents/DTU/6. Semester/Bachelorprojekt/Data_Ellen_Maria/hist_allDataL1.png", width = 700, height = 500)
ggplot(data = total_val_land_1, aes(x=landuse_L1)) + geom_bar() + 
  geom_text(stat='count', aes(label=..count..), vjust=-1) + 
  ggtitle("Frequency distribution of landcover factors (Level 1) in Spain") +
  labs(y="Count", x = "Land cover factors (Level 1)") +
  theme(text = element_text(size=15))
dev.off()

total_val_land_1$landuse_L2 <- factor(total_val_land_1$landuse_L2)
w_landuse_L2 = table(total_val_land_1$landuse_L2)

t_landuse_L2 = as.data.frame(w_landuse_L2)
colnames(t_landuse_L2) <- c("Land cover factors (Level 2)", "Frequency")

print(xtable(t_landuse_L2, type = "latex"), file = "~/Documents/DTU/6. Semester/Bachelorprojekt/Data_Ellen_Maria/landuse_L2_countFullData.tex")

png(file = "~/Documents/DTU/6. Semester/Bachelorprojekt/Data_Ellen_Maria/hist_allDataL2.png", width = 1000, height = 450)
na.omit(total_val_land_1) %>% ggplot(aes(x=landuse_L2)) + geom_bar() + 
  geom_text(stat='count', aes(label=..count..), vjust=-1) + 
  ggtitle("Frequency distribution of landcover factors (Level 2) in Spain") +
  labs(y="Count", x = "Land cover factors (Level 2)") +
  theme(text = element_text(size=15)) + scale_colour_discrete(na.translate = F)
dev.off()

total_val_land_1$landuse_L3 <- factor(total_val_land_1$landuse_L3)
w_landuse_L3 = table(total_val_land_1$landuse_L3)

t_landuse_L3 = as.data.frame(w_landuse_L3)
colnames(t_landuse_L3) <- c("Land cover factors (Level 3)", "Frequency")

print(xtable(t_landuse_L3, type = "latex"), file = "~/Documents/DTU/6. Semester/Bachelorprojekt/Data_Ellen_Maria/landuse_L3_countFullData.tex")


##### SUMMARY: BSK VINE
# Load data
delim = ";"
dec = "," 
setwd("~/Documents/DTU/6. Semester/Bachelorprojekt/Data_Ellen_Maria/Cross-validation")
dat <- read.csv("Bsk_vineyard_2_int.csv", header = TRUE, sep=delim, dec=dec, stringsAsFactors=FALSE)
X <- as.data.frame(dat[,c(2:4,6:8)]) 
colnames(X) <- c("LAI", "p", "Rn", "e", "e*", "ET")

library(xtable)
print(xtable(summary(X), type = "latex"), file = "~/Documents/DTU/6. Semester/Bachelorprojekt/Data_Ellen_Maria/summary_BSk_Vine.tex")


##### SUMMARY: PERMANENT CROPS
# Load data
delim = ";"
dec = "," 
setwd("~/Documents/DTU/6. Semester/Bachelorprojekt/Data_Ellen_Maria/Cross-validation")
dat <- read.csv("22_AInt_uden_0.csv", header = TRUE, sep=delim, dec=dec, stringsAsFactors=FALSE)
X <- as.data.frame(dat[,c(2:6,70)]) 
colnames(X) <- c("LAI", "p", "Rn", "e", "e*", "ET")
head(X)

library(xtable)
print(xtable(summary(X), type = "latex"), file = "~/Documents/DTU/6. Semester/Bachelorprojekt/Data_Ellen_Maria/summary_22.tex")

KG <- as.data.frame(dat[,c(62:69)])
head(KG)
colSums(KG)
KG = as.data.frame(colSums(KG))

KG$colSum(KG) <- factor(KG$colSum(KG))
w_landuse_L1 = table(total_val_land_1$landuse_L1)

t_landuse_L1 = as.data.frame(w_landuse_L1)

t_22KG = as.data.frame(colSums(KG))
print(xtable(t_22KG, type = "latex"), file = "~/Documents/DTU/6. Semester/Bachelorprojekt/Data_Ellen_Maria/22_countKG.tex")

png(file = "~/Documents/DTU/6. Semester/Bachelorprojekt/Data_Ellen_Maria/hist_allDataKG.png", width = 1000, height = 560)
ggplot(data = t_22KG, aes(x=KG)) + geom_bar() + 
  geom_text(stat='count', aes(label=..count..), vjust=-1) + 
  ggtitle("Frequency distribution of KG-climate factors in Spain") +
  labs(y="Count", x = "KG-climate factors") +
  theme(text = element_text(size=15))
dev.off()

##### SUMMARY: Forests
# Load data
delim = ";"
dec = "," 
setwd("~/Documents/DTU/6. Semester/Bachelorprojekt/Data_Ellen_Maria/Cross-validation")
dat <- read.csv("31_AInt_uden_6_16_19_29.csv", header = TRUE, sep=delim, dec=dec, stringsAsFactors=FALSE)
X <- as.data.frame(dat[,c(2:6,82)]) 
colnames(X) <- c("LAI", "p", "Rn", "e", "e*", "ET")

library(xtable)
print(xtable(summary(X), type = "latex"), file = "~/Documents/DTU/6. Semester/Bachelorprojekt/Data_Ellen_Maria/summary_31.tex")

KG <- as.data.frame(dat[,c(72:81)])
head(KG)
colSums(KG)

t_31KG = as.data.frame(colSums(KG))
print(xtable(t_31KG, type = "latex"), file = "~/Documents/DTU/6. Semester/Bachelorprojekt/Data_Ellen_Maria/31_countKG.tex")

