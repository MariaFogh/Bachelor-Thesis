# sample 4 from random_uni 
setwd("C:/Users/ellen/Documents/BA")
source("DataIn.R")

setwd("C:/Users/ellen/Documents/BA/kode")
source("random_uni.R")
#random_uni <- function(matrix,size_sample, n_sample)

RASTER1 <- mask

uni_ran <- random_uni(RASTER1,1000,10)

names <- c("e_act_year","e_star_year","LAI_year","p_year","Rn_year","VPD_year","ET_GF_year","landuse_level1")

##### sample 4 ####
sample_4 <- uni_ran$coor_matrix[,10:12]
sample_4 <- sample_4[complete.cases(sample_4),]
# koordinater 
coor_4 <- cbind(sample_4[,2],sample_4[,3])
# Udtag værdier fra data 
VarMatrix <- matrix(ncol = (length(names)+2), nrow = length(sample_4[,1]))
for(i in 1:length(names)){
  # file in from global Env
  imp <- get(names[i], .GlobalEnv)
  VarMatrix[,i] <- imp[coor_4]
}
VarMatrix[,(length(names)+1):(length(names)+2)] <- coor_4
total_val_land_4 <- as.data.frame(VarMatrix)
names(total_val_land_4)<- c(names,'row','col')
total_val_land_4$landuse_level1 <- as.factor(total_val_land_4$landuse_level1)
##############################
##############################
total_land_4_scale <- data.frame(coor_4)
names(total_land_4_scale) = c('row','col')
total_land_4_scale$e <- scale(total_val_land_4$e_act_year)
total_land_4_scale$e_star <- scale(total_val_land_4$e_star_year)
total_land_4_scale$LAI <- scale(total_val_land_4$LAI_year)
total_land_4_scale$p <- scale(total_val_land_4$p_year)
total_land_4_scale$Rn <- scale(total_val_land_4$Rn_year)
total_land_4_scale$ET<- scale(total_val_land_4$ET_GF_year)
total_land_4_scale$landuse <- as.factor(total_val_land_4$landuse_level1)
#### Make model ####
lm4 <- lm(ET ~ .- row - col, data = total_land_4_scale)
lm4a <- step(lm4, test = "F")
drop1(lm4a, test = "F")
summary(lm4a)
par(mfrow=c(2,2))
plot(lm4a)
library(jtools)
summ(lm4a,digits = 4, model.info = FALSE)
