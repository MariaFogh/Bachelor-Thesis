# sample 3 from random_uni 
setwd("C:/Users/ellen/Documents/BA")
source("DataIn.R")

setwd("C:/Users/ellen/Documents/BA/kode")
source("random_uni.R")
#random_uni <- function(matrix,size_sample, n_sample)

RASTER1 <- mask

uni_ran <- random_uni(RASTER1,1000,10)

names <- c("e_act_year","e_star_year","LAI_year","p_year","Rn_year","VPD_year","ET_GF_year","landuse_level1","landuse")

##### sample 3 ####
sample_3 <- uni_ran$coor_matrix[,7:9]
sample_3 <- sample_3[complete.cases(sample_3),]
# koordinater 
coor_3 <- cbind(sample_3[,2],sample_3[,3])
# Udtag værdier fra data 
VarMatrix <- matrix(ncol = (length(names)+2), nrow = length(sample_3[,1]))
for(i in 1:length(names)){
  # file in from global Env
  imp <- get(names[i], .GlobalEnv)
  VarMatrix[,i] <- imp[coor_3]
}
VarMatrix[,(length(names)+1):(length(names)+2)] <- coor_3
total_val_land_3 <- as.data.frame(VarMatrix)
names(total_val_land_3)<- c(names,'row','col')
total_val_land_3$landuse_level1 <- as.factor(total_val_land_3$landuse_level1)
total_val_land_3$landuse <- as.factor(total_val_land_3$landuse)
##############################
##############################
total_land_3_scale <- data.frame(coor_3)
names(total_land_3_scale) = c('row','col')
total_land_3_scale$e <- scale(total_val_land_3$e_act_year)
total_land_3_scale$e_star <- scale(total_val_land_3$e_star_year)
total_land_3_scale$LAI <- scale(total_val_land_3$LAI_year)
total_land_3_scale$p <- scale(total_val_land_3$p_year)
total_land_3_scale$Rn <- scale(total_val_land_3$Rn_year)
total_land_3_scale$ET<- scale(total_val_land_3$ET_GF_year)
total_land_3_scale$landuse <- as.factor(total_val_land_3$landuse_level1)
#### Make model ####
lm3 <- lm(ET ~ .- row - col, data = total_land_3_scale)
lm3a <- step(lm3, test = "F")
drop1(lm3a, test = "F")
par(mfrow=c(2,2))
plot(lm3a)
library(jtools)
summ(lm3a,digits = 4, model.info = FALSE)
