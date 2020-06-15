# sample 2 from random_uni 
setwd("C:/Users/ellen/Documents/BA")
source("DataIn.R")

setwd("C:/Users/ellen/Documents/BA/kode")
source("random_uni.R")
#random_uni <- function(matrix,size_sample, n_sample)

RASTER1 <- mask

uni_ran <- random_uni(RASTER1,1000,10)

names <- c("e_act_year","e_star_year","LAI_year","p_year","Rn_year","VPD_year","ET_GF_year","landuse_level1","landuse")

##### sample 2 ####
sample_2 <- uni_ran$coor_matrix[,4:6]
sample_2 <- sample_2[complete.cases(sample_2),]
# koordinater 
coor_2 <- cbind(sample_2[,2],sample_2[,3])
# Udtag værdier fra data 
VarMatrix <- matrix(ncol = (length(names)+2), nrow = length(sample_2[,1]))
for(i in 1:length(names)){
  # file in from global Env
  imp <- get(names[i], .GlobalEnv)
  VarMatrix[,i] <- imp[coor_2]
}
VarMatrix[,(length(names)+1):(length(names)+2)] <- coor_2
total_val_land_2 <- as.data.frame(VarMatrix)
names(total_val_land_2)<- c(names,'row','col')
total_val_land_2$landuse_level1 <- as.factor(total_val_land_2$landuse_level1)
total_val_land_2$landuse <- as.factor(total_val_land_2$landuse)
##############################
##############################
###### Standardize data #######
total_land_2_scale <- data.frame(coor_2)
names(total_land_2_scale) = c('row','col')
total_land_2_scale$e <- scale(total_val_land_2$e_act_year)
total_land_2_scale$e_star <- scale(total_val_land_2$e_star_year)
total_land_2_scale$LAI <- scale(total_val_land_2$LAI_year)
total_land_2_scale$p <- scale(total_val_land_2$p_year)
total_land_2_scale$Rn <- scale(total_val_land_2$Rn_year)
total_land_2_scale$ET<- scale(total_val_land_2$ET_GF_year)
total_land_2_scale$landuse <- as.factor(total_val_land_2$landuse_level1)
#### Make model 5 factor ####
lm2 <- lm(ET ~ .- row - col, data = total_land_2_scale)
lm2a <- step(lm2, test = "F")
drop1(lm2a, test = "F")
par(mfrow=c(2,2))
plot(lm2a)
library(jtools)
summ(lm2a,digits = 4, model.info = FALSE)
