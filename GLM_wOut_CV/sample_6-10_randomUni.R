# sample 6 from random_uni 
setwd("C:/Users/ellen/Documents/BA")
source("DataIn.R")
setwd("C:/Users/ellen/Documents/BA/kode")
source("random_uni.R")

RASTER1 <- mask
uni_ran <- random_uni(RASTER1,1000,10)
names <- c("e_act_year","e_star_year","LAI_year","p_year","Rn_year","VPD_year","ET_GF_year","landuse_level1")

##### sample 6 ####
sample_6 <- uni_ran$coor_matrix[,16:18]
sample_6 <- sample_6[complete.cases(sample_6),]
# koordinater 
coor_6 <- cbind(sample_6[,2],sample_6[,3])
# Udtag værdier fra data 
VarMatrix <- matrix(ncol = (length(names)+2), nrow = length(sample_6[,1]))
for(i in 1:length(names)){
  # file in from global Env
  imp <- get(names[i], .GlobalEnv)
  VarMatrix[,i] <- imp[coor_6]
}
VarMatrix[,(length(names)+1):(length(names)+2)] <- coor_6
total_val_land_6 <- as.data.frame(VarMatrix)
names(total_val_land_6)<- c(names,'row','col')
total_val_land_6$landuse_level1 <- as.factor(total_val_land_6$landuse_level1)
##############################
##############################
###### Standardize data #######
###### 6 ######
total_land_6_scale <- data.frame(coor_6)
names(total_land_6_scale) = c('row','col')
total_land_6_scale$e <- scale(total_val_land_6$e_act_year)
total_land_6_scale$e_star <- scale(total_val_land_6$e_star_year)
total_land_6_scale$LAI <- scale(total_val_land_6$LAI_year)
total_land_6_scale$p <- scale(total_val_land_6$p_year)
total_land_6_scale$Rn <- scale(total_val_land_6$Rn_year)
total_land_6_scale$ET<- scale(total_val_land_6$ET_GF_year)
total_land_6_scale$landuse <- as.factor(total_val_land_6$landuse_level1)
#### Make model ####
lm6 <- lm(ET ~ .- row - col, data = total_land_6_scale)
lm6a <- step(lm6, test = "F")
drop1(lm6a, test = "F")
summary(lm6a)
par(mfrow=c(2,2))
plot(lm6a)
library(jtools)
summ(lm6a,digits = 4, model.info = FALSE)


##### 7 ######
sample_7 <- uni_ran$coor_matrix[,19:21]
sample_7 <- sample_7[complete.cases(sample_7),]
# koordinater 
coor_7 <- cbind(sample_7[,2],sample_7[,3])
# Udtag værdier fra data 
VarMatrix <- matrix(ncol = (length(names)+2), nrow = length(sample_7[,1]))
for(i in 1:length(names)){
  # file in from global Env
  imp <- get(names[i], .GlobalEnv)
  VarMatrix[,i] <- imp[coor_7]
}
VarMatrix[,(length(names)+1):(length(names)+2)] <- coor_7
total_val_land_7 <- as.data.frame(VarMatrix)
names(total_val_land_7)<- c(names,'row','col')
total_val_land_7$landuse_level1 <- as.factor(total_val_land_7$landuse_level1)
#stand
total_land_7_scale <- data.frame(coor_7)
names(total_land_7_scale) = c('row','col')
total_land_7_scale$e <- scale(total_val_land_7$e_act_year)
total_land_7_scale$e_star <- scale(total_val_land_7$e_star_year)
total_land_7_scale$LAI <- scale(total_val_land_7$LAI_year)
total_land_7_scale$p <- scale(total_val_land_7$p_year)
total_land_7_scale$Rn <- scale(total_val_land_7$Rn_year)
total_land_7_scale$ET<- scale(total_val_land_7$ET_GF_year)
total_land_7_scale$landuse <- as.factor(total_val_land_7$landuse_level1)
#model
lm7 <- lm(ET ~ .- row - col, data = total_land_7_scale)
lm7a <- step(lm7, test = "F")
drop1(lm7a, test = "F")
par(mfrow=c(2,2))
plot(lm7a)
library(jtools)
summ(lm7a,digits = 4, model.info = FALSE)

##### 8 ######
sample_8 <- uni_ran$coor_matrix[,22:24]
sample_8 <- sample_8[complete.cases(sample_8),]
# koordinater 
coor_8 <- cbind(sample_8[,2],sample_8[,3])
# Udtag værdier fra data 
VarMatrix <- matrix(ncol = (length(names)+2), nrow = length(sample_8[,1]))
for(i in 1:length(names)){
  # file in from global Env
  imp <- get(names[i], .GlobalEnv)
  VarMatrix[,i] <- imp[coor_8]
}
VarMatrix[,(length(names)+1):(length(names)+2)] <- coor_8
total_val_land_8 <- as.data.frame(VarMatrix)
names(total_val_land_8)<- c(names,'row','col')
total_val_land_8$landuse_level1 <- as.factor(total_val_land_8$landuse_level1)
#stand
total_land_8_scale <- data.frame(coor_8)
names(total_land_8_scale) = c('row','col')
total_land_8_scale$e <- scale(total_val_land_8$e_act_year)
total_land_8_scale$e_star <- scale(total_val_land_8$e_star_year)
total_land_8_scale$LAI <- scale(total_val_land_8$LAI_year)
total_land_8_scale$p <- scale(total_val_land_8$p_year)
total_land_8_scale$Rn <- scale(total_val_land_8$Rn_year)
total_land_8_scale$ET<- scale(total_val_land_8$ET_GF_year)
total_land_8_scale$landuse <- as.factor(total_val_land_8$landuse_level1)
#model
lm8 <- lm(ET ~ .- row - col, data = total_land_8_scale)
lm8a <- step(lm8, test = "F")
drop1(lm8a, test = "F")
library(jtools)
summ(lm8a,digits = 4, model.info = FALSE)

##### 9 ######
sample_9 <- uni_ran$coor_matrix[,25:27]
sample_9 <- sample_9[complete.cases(sample_9),]
# koordinater 
coor_9 <- cbind(sample_9[,2],sample_9[,3])
# Udtag værdier fra data 
VarMatrix <- matrix(ncol = (length(names)+2), nrow = length(sample_9[,1]))
for(i in 1:length(names)){
  # file in from global Env
  imp <- get(names[i], .GlobalEnv)
  VarMatrix[,i] <- imp[coor_9]
}
VarMatrix[,(length(names)+1):(length(names)+2)] <- coor_9
total_val_land_9 <- as.data.frame(VarMatrix)
names(total_val_land_9)<- c(names,'row','col')
total_val_land_9$landuse_level1 <- as.factor(total_val_land_9$landuse_level1)
#stand
total_land_9_scale <- data.frame(coor_9)
names(total_land_9_scale) = c('row','col')
total_land_9_scale$e <- scale(total_val_land_9$e_act_year)
total_land_9_scale$e_star <- scale(total_val_land_9$e_star_year)
total_land_9_scale$LAI <- scale(total_val_land_9$LAI_year)
total_land_9_scale$p <- scale(total_val_land_9$p_year)
total_land_9_scale$Rn <- scale(total_val_land_9$Rn_year)
total_land_9_scale$ET<- scale(total_val_land_9$ET_GF_year)
total_land_9_scale$landuse <- as.factor(total_val_land_9$landuse_level1)
#model
lm9 <- lm(ET ~ .- row - col, data = total_land_9_scale)
lm9a <- step(lm9, test = "F")
drop1(lm9a, test = "F")
library(jtools)
summ(lm9a,digits = 4, model.info = FALSE)

##### 10 ######
sample_10 <- uni_ran$coor_matrix[,22:24]
sample_10 <- sample_10[complete.cases(sample_10),]
# koordinater 
coor_10 <- cbind(sample_10[,2],sample_10[,3])
# Udtag værdier fra data 
VarMatrix <- matrix(ncol = (length(names)+2), nrow = length(sample_10[,1]))
for(i in 1:length(names)){
  # file in from global Env
  imp <- get(names[i], .GlobalEnv)
  VarMatrix[,i] <- imp[coor_10]
}
VarMatrix[,(length(names)+1):(length(names)+2)] <- coor_10
total_val_land_10 <- as.data.frame(VarMatrix)
names(total_val_land_10)<- c(names,'row','col')
total_val_land_10$landuse_level1 <- as.factor(total_val_land_10$landuse_level1)
#stand
total_land_10_scale <- data.frame(coor_10)
names(total_land_10_scale) = c('row','col')
total_land_10_scale$e <- scale(total_val_land_10$e_act_year)
total_land_10_scale$e_star <- scale(total_val_land_10$e_star_year)
total_land_10_scale$LAI <- scale(total_val_land_10$LAI_year)
total_land_10_scale$p <- scale(total_val_land_10$p_year)
total_land_10_scale$Rn <- scale(total_val_land_10$Rn_year)
total_land_10_scale$ET<- scale(total_val_land_10$ET_GF_year)
total_land_10_scale$landuse <- as.factor(total_val_land_10$landuse_level1)
#model
lm10 <- lm(ET ~ .- row - col, data = total_land_10_scale)
lm10a <- step(lm10, test = "F")
drop1(lm10a, test = "F")
library(jtools)
summ(lm10a,digits = 4, model.info = FALSE)
