rm(list = ls())

setwd("C:/Users/ellen/Documents/BA")
source("DataIn.R")

setwd("C:/Users/ellen/Documents/BA/kode")
source("random_uni.R")

RASTER1 <- mask_Bsk_vine
n <- length(mask_Bsk_vine)
uni_ran <- random_uni(RASTER1,n,1)
names <- c("LAI_year","p_year","Rn_year","e_act_year","e_star_year","ET_GF_year")

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
names <- c("LAI","p","Rn","e","e_star","ET")
names(total_val_land_1)<- c(names,'row','col')
head(total_val_land_1)
write.csv2(total_val_land_1,"C:/Users/ellen/Documents/BA/DataSubset/Bsk_vineyard_with_e.csv" )

##########################
#### standardixe data ####
Bsk_vine_Matrix <- matrix(ncol = (length(names)), nrow = length(total_val_land_1[,1]))
Bsk_vine_Matrix[,1] <- scale(total_val_land_1[,1])
Bsk_vine_Matrix[,2] <- scale(total_val_land_1[,2])
Bsk_vine_Matrix[,3] <- scale(total_val_land_1[,3])
Bsk_vine_Matrix[,4] <- scale(total_val_land_1[,4])
Bsk_vine_Matrix[,5] <- scale(total_val_land_1[,5])
Bsk_vine_Matrix[,6] <- total_val_land_1[,6]

df_Bsk_vine_scale <- as.data.frame(Bsk_vine_Matrix)
names(df_Bsk_vine_scale) <- names

lm_Bsk_vine_scale_1 <- lm(ET ~ .^2, data = df_Bsk_vine_scale)
lm_Bsk_vine_scale_1a <- update(lm_Bsk_vine_scale_1,~.+I(LAI^2)+I(p^2)+
                                 I(Rn^2)+I(e^2)+I(e_star^2),data = df_Bsk_vine_scale)
lm_Bsk_vine_scale_1b <- step(lm_Bsk_vine_scale_1a, test = "F", ,direction = "backward")
drop1(lm_Bsk_vine_scale_1b, test = "F")
lm_Bsk_vine_scale_1c <- update(lm_Bsk_vine_scale_1b,~.-LAI:e_star)
drop1(lm_Bsk_vine_scale_1c, test = "F")
summary(lm_Bsk_vine_scale_1c)
library(jtools)
summ(lm_Bsk_vine_scale_1c, scale = TRUE,digits = 4, model.info = FALSE)
summ(lm_Bsk_vine_scale_1c,digits = 4, model.info = FALSE)

effect_plot(lm_Bsk_vine_scale_1c, pred = Rn, interval = TRUE, plot.points = TRUE)
png("C:/Users/ellen/Documents/BA/Model presentation/Bsk_vine_GLM/Bsk_vine_all_diag.png", width = 1000, height = 500)
par(mfrow = c(2,2))
par(cex.lab = 1.5)
par(cex.axis = 1.5)
par(cex.sub = 5)
par(cex.main = 4)
plot(lm_Bsk_vine_scale_1c)
dev.off()
