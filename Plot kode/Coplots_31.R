##################################
##################################
#### Opsæt af data .csv filer ####
rm(list = ls())
library(xtable)
library(caret)

setwd("C:/Users/ellen/Documents/BA")
source("DataIn.R")

setwd("C:/Users/ellen/Documents/BA/kode")
source("random_uni.R")

RASTER1 <- mask_31_new
n <- length(mask_31_new)
uni_ran <- random_uni(RASTER1,n,1)

sample_1 <- uni_ran$coor_matrix[,1:3]
sample_1 <- sample_1[complete.cases(sample_1),]
# koordinater 
coor_1 <- cbind(sample_1[,2],sample_1[,3])

# udtage factor værdier og interactioner 
names_factor <- c("LAI_year","p_year","Rn_year","e_act_year","e_star_year","KG","ET_GF_year")
VarMatrix <- matrix(ncol = (length(names_factor)), nrow = length(sample_1[,1]))
for(i in 1:length(names_factor)){
  # file in from global Env
  imp <- get(names_factor[i], .GlobalEnv)
  VarMatrix[,i] <- imp[coor_1]
}
var_factor <- as.data.frame(VarMatrix)
names(var_factor) <- names_factor
head(var_factor)
var_factor$KG <- as.factor(var_factor$KG)
#var_factor <- var_factor[!(var_factor$KG == "6" | var_factor$KG == "16"|var_factor$KG == "19"|var_factor$KG == "29"),]
str(var_factor)
#lev <- c("4","5", "7", "8", "9", "14", "15", "18", "19", "26", "27" )
##### CO-PLOT #####
i=5
par(cex.lab = 10)
par(cex.axis = 10)
names <- c("LAI","p","Rn","e","e*","KG","Evapotranspiration")
for (i in 1:5) {
  png(file = paste("C:/Users/ellen/Documents/BA/DataSubset/31_coplot_",names_factor[i],"_KG.png", sep = ""), width = 800, height = 500)
  coplot(ET_GF_year ~ var_factor[,i]|var_factor[,6],data = var_factor,row =1,
         xlab = c(paste(names[i]), "Given: KG Map"),
         ylab = "Evapotranspiration")
  dev.off()
}
apply(var_factor[,1:5], 2, min)
apply(var_factor[,1:5], 2, max)
png(file = paste("C:/Users/ellen/Documents/BA/DataSubset/31_coplot_LAI_e.png", sep = ""), width = 800, height = 500)
coplot(ET_GF_year ~ LAI_year|e_act_year,data = var_factor,row =1,
       xlab = c("LAI","e"), ylab = "ET", panel = panel.smooth)  
dev.off()
png(file = paste("C:/Users/ellen/Documents/BA/DataSubset/31_coplot_e_LAI.png", sep = ""), width = 800, height = 500)
coplot(ET_GF_year ~ e_act_year|LAI_year,data = var_factor,row =1,
       xlab = c("e","LAI"), ylab = "ET", panel = panel.smooth)  
dev.off()

png(file = paste("C:/Users/ellen/Documents/BA/DataSubset/31_coplot_Rn_e.png", sep = ""), width = 800, height = 500)
coplot(ET_GF_year ~ Rn_year|e_act_year,data = var_factor,row =1,
       xlab = c("Rn","e"), ylab = "ET", panel = panel.smooth)  
dev.off()
png(file = paste("C:/Users/ellen/Documents/BA/DataSubset/31_coplot_e_Rn.png", sep = ""), width = 800, height = 500)
coplot(ET_GF_year ~ e_act_year|Rn_year,data = var_factor,row =1,
       xlab = c("e","Rn"), ylab = "ET", panel = panel.smooth)  
dev.off()
