#################################################
# Conditioning plots #
#################################################

rm(list=ls())

# Load data
delim = ";"
dec = "," 
setwd("~/Documents/DTU/6. Semester/Bachelorprojekt/Data_Ellen_Maria/Cross-validation")
dat <- read.csv("Bsk_vineyard_2_int.csv", header = TRUE, sep=delim, dec=dec, stringsAsFactors=FALSE)
head(dat)

# Selecting p and e_star 
df <- dat[,c(3,7,8)]
colnames(df) <- c("p", "e*", "ET")
head(df)

# Interaction if slope changes in different interval windows
library(lattice)
png(paste(file = "~/Documents/DTU/6. Semester/Bachelorprojekt/Data_Ellen_Maria/coplot/coplot_estar_given_p(VineBsk).png"), width = 900, height = 500)
par(cex.lab = 2.5)
par(cex.axis = 2.5)
coplot(ET ~ `e*` | p, df,
      number = 4, columns = 4, overlap=0)
dev.off()

png(paste(file = "~/Documents/DTU/6. Semester/Bachelorprojekt/Data_Ellen_Maria/coplot/coplot_p_given_estar(VineBsk).png"), width = 900, height = 500)
par(cex.lab = 2.5)
par(cex.axis = 2.5)
coplot(ET ~ p | `e*`, df,
       number = 4, columns = 4, overlap = 0)
dev.off()

# Selecting Rn and e_act
df <- dat[,c(4,6,8)]
colnames(df) <- c("Rn", "e", "ET")

# Interaction if slope changes in different interval windows
png(paste(file = "~/Documents/DTU/6. Semester/Bachelorprojekt/Data_Ellen_Maria/coplot/coplot_eact_given_Rn(VineBsk).png"), width = 900, height = 500)
par(cex.lab = 2.5)
par(cex.axis = 2.5)
coplot(ET ~ e | Rn, df,
      number = 4, columns = 4, overlap = 0, )
dev.off()

png(paste(file = "~/Documents/DTU/6. Semester/Bachelorprojekt/Data_Ellen_Maria/coplot/coplot_Rn_given_eact(VineBsk).png"), width = 900, height = 500)
par(cex.lab = 2.5)
par(cex.axis = 2.5)
coplot(ET ~ Rn | e, df,
      number = 4, columns = 4, overlap = 0)
dev.off()

# Selecting Rn and p 
df <- dat[,c(3,4,8)]
colnames(df) <- c("p", "Rn", "ET")

# Interaction if slope changes in different interval windows
png(paste(file = "~/Documents/DTU/6. Semester/Bachelorprojekt/Data_Ellen_Maria/coplot/coplot_p_given_Rn(VineBsk).png"), width = 900, height = 500)
par(cex.lab = 2.5)
par(cex.axis = 2.5)
coplot(ET ~ p | Rn, df,
       number = 4, columns = 4, overlap = 0, )
dev.off()

png(paste(file = "~/Documents/DTU/6. Semester/Bachelorprojekt/Data_Ellen_Maria/coplot/coplot_Rn_given_p(VineBsk).png"), width = 900, height = 500)
par(cex.lab = 2.5)
par(cex.axis = 2.5)
coplot(ET ~ Rn | p, df,
       number = 4, columns = 4, overlap = 0)
dev.off()




