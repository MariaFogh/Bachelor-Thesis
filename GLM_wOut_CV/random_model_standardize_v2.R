setwd("C:/Users/ellen/Documents/BA")
source("DataIn.R")

setwd("C:/Users/ellen/Documents/BA/kode")
source("random_uni.R")
#random_uni <- function(matrix,size_sample, n_sample)

RASTER1 <- mask

uni_ran <- random_uni(RASTER1,1000,10)

names <- c("e_act_year","e_star_year","LAI_year","p_year","Rn_year","VPD_year","ET_GF_year","landuse_level1","landuse")

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
names(total_val_land_1)<- c(names,'row','col')
head(total_val_land_1)
total_val_land_1$landuse_level1 <- as.factor(total_val_land_1$landuse_level1)
total_val_land_1$landuse <- as.factor(total_val_land_1$landuse)
###############################
###############################
###### Standardize data #######
total_land_1_scale <- data.frame(coor_1)
names(total_land_1_scale) = c('row','col')
total_land_1_scale$e <- scale(total_val_land_1$e_act_year)
total_land_1_scale$e_star <- scale(total_val_land_1$e_star_year)
total_land_1_scale$LAI <- scale(total_val_land_1$LAI_year)
total_land_1_scale$p <- scale(total_val_land_1$p_year)
total_land_1_scale$Rn <- scale(total_val_land_1$Rn_year)
#total_land_1_scale$VPD <- scale(total_val_land_1$VPD_year)
total_land_1_scale$ET<- scale(total_val_land_1$ET_GF_year)
total_land_1_scale$landuse <- as.factor(total_val_land_1$landuse_level1)
#total_land_1_scale$landuse <- as.factor(total_val_land_1$landuse)
#### Model factor 5 ####
lm1_scale <- lm(ET ~ .- row - col, data = total_land_1_scale)
lm1a_scale <- step(lm1_scale, test = "F")
drop1(lm1a_scale, test = "F")
summary(lm1a_scale)
library(jtools)
summ(lm1a_scale,digits = 4, model.info = FALSE)


