# sample 5 from random_uni 
setwd("C:/Users/ellen/Documents/BA")
source("DataIn.R")

setwd("C:/Users/ellen/Documents/BA/kode")
source("random_uni.R")
#random_uni <- function(matrix,size_sample, n_sample)

RASTER1 <- mask

uni_ran <- random_uni(RASTER1,1000,10)

names <- c("e_act_year","e_star_year","LAI_year","p_year","Rn_year","VPD_year","ET_GF_year","landuse_level1")

##### sample 5 ####
sample_5 <- uni_ran$coor_matrix[,13:15]
sample_5 <- sample_5[complete.cases(sample_5),]
# koordinater 
coor_5 <- cbind(sample_5[,2],sample_5[,3])
# Udtag værdier fra data 
VarMatrix <- matrix(ncol = (length(names)+2), nrow = length(sample_5[,1]))
for(i in 1:length(names)){
  # file in from global Env
  imp <- get(names[i], .GlobalEnv)
  VarMatrix[,i] <- imp[coor_5]
}
VarMatrix[,(length(names)+1):(length(names)+2)] <- coor_5
total_val_land_5 <- as.data.frame(VarMatrix)
names(total_val_land_5)<- c(names,'row','col')
total_val_land_5$landuse_level1 <- as.factor(total_val_land_5$landuse_level1)
##############################
##############################
###### Standardize data #######
total_land_5_scale <- data.frame(coor_5)
names(total_land_5_scale) = c('row','col')
total_land_5_scale$e <- scale(total_val_land_5$e_act_year)
total_land_5_scale$e_star <- scale(total_val_land_5$e_star_year)
total_land_5_scale$LAI <- scale(total_val_land_5$LAI_year)
total_land_5_scale$p <- scale(total_val_land_5$p_year)
total_land_5_scale$Rn <- scale(total_val_land_5$Rn_year)
total_land_5_scale$ET<- scale(total_val_land_5$ET_GF_year)
total_land_5_scale$landuse <- as.factor(total_val_land_5$landuse_level1)
#### Model ####
lm5 <- lm(ET ~ .- row - col, data = total_land_5_scale)
lm5a <- step(lm5, test = "F")
drop1(lm5a, test = "F")
lm5b <- update(lm5a,~.-e_star )
drop1(lm5b,test ="F")
summary(lm5b)
par(mfrow=c(2,2))
plot(lm5a)
library(jtools)
summ(lm5b,digits = 4, model.info = FALSE)
