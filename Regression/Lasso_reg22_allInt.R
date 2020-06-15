## Lasso-regression
rm(list=ls())

library(glmnet)  # for ridge regression
library(dplyr)   # for data cleaning
library(psych)   # for function tr() to compute trace of a matrix
library(cvTools)

packages <- c('coefplot', 'DT')
purrr::walk(packages, library, character.only=TRUE)

# Load data
delim = ";"
dec = "," 
setwd("C:/Users/ellen/Documents/BA/DataSubset")
dat <- read.csv("22_AInt_uden_0.csv", header = TRUE, sep=delim, dec=dec, stringsAsFactors=FALSE)
n <- dim(dat)[2]
X <- as.matrix(dat[,c(1:6,62:69)]) # w/out ET_GF
KK = 6
KG_nr = 8
KG_names <- c("KG.4","KG.5","KG.6","KG.7","KG.8","KG.9","KG.14","KG.15")

attributeNames <- c("Intercept","LAI","p","Rn","e","e*",
                    "LAI:p","LAI:Rn","LAI:e","LAI:e*","p:Rn","p:e","p:e*","Rn:e","Rn:e*","e:e*",
                    "LAI^2","p^2","Rn^2","e^2","e*^2",
                    "KG4:LAI","KG4:p","KG4:Rn","KG4:e","KG4:e*",
                    "KG5:LAI","KG5:p","KG5:Rn","KG5:e","KG5:e*",
                    "KG6:LAI","KG6:p","KG6:Rn","KG6:e","KG6:e*",
                    "KG7:LAI","KG7:p","KG7:Rn","KG7:e","KG7:e*",
                    "KG8:LAI","KG8:p","KG8:Rn","KG8:e","KG8:e*",
                    "KG9:LAI","KG9:p","KG9:Rn","KG9:e","KG9:e*",
                    "KG14:LAI","KG14:p","KG14:Rn","KG14:e","KG14:e*",
                    "KG15:LAI","KG15:p","KG15:Rn","KG15:e","KG15:e*",
                    "KG4","KG5","KG6","KG7","KG8","KG9","KG14","KG15")
y <- dat[,n]

P <- length(attributeNames)
N <- dim(X)[1]
M <- dim(X)[2]
# Regularized Linear regression 
set.seed(144)    # seef for reproducibility
# Crossvalidation
# Create crossvalidation partition for evaluation of performance of optimal model
K <- 10
CV <- cvFolds(N, K=K)

# set up vectors that will store sizes of training and test sizes
CV$TrainSize <- c()
CV$TestSize <- c()

# Initialise variables
Error_train_rlr <- rep(NA,K)
Error_test_rlr <- rep(NA,K)
lambda_cv <- rep(NA,K)
coef <- matrix(rep(NA, times=(P)*K), nrow=P)
mu2 <- matrix(rep(NA, times=(5)*K), nrow=5)
sigma2 <- matrix(rep(NA, times=(5)*K), nrow=5)

for(k in 1:K){
  set.seed(144) 
  paste('Crossvalidation fold ', k, '/', K, sep='')
  
  # Extract the training and test set
  X_train <- X[CV$subsets[CV$which!=k], ];
  y_train <- y[CV$subsets[CV$which!=k]];
  X_test <- X[CV$subsets[CV$which==k], ];
  y_test <- y[CV$subsets[CV$which==k]];
  CV$TrainSize[k] <- length(y_train)
  CV$TestSize[k] <- length(y_test)

  mu2[,k]<- colMeans(X_train[, 2:KK])
  sigma2[,k] <- apply(X_train[, 2:KK], 2, sd)
  
  X_train[, 2:KK] <- scale(X_train[, 2:KK], mu2[,k], sigma2[,k])
  X_test[, 2:KK] <- scale(X_test[, 2:KK], mu2[,k], sigma2[,k])
  
  #obtain interactions
  var_2_train <- X_train[,2:6]^2
  var_2_test <- X_test[,2:6]^2
  
  X_train_int <- model.matrix(~.^2,as.data.frame(X_train[,c(2:6)]))[,-1]
  X_test_int <- model.matrix(~.^2,as.data.frame(X_test[,c(2:6)]))[,-1]
  
  X_train_KG <- model.matrix(~(KG.4+LAI_year+p_year+Rn_year+e_act_year+e_star_year)^2 -1,as.data.frame(X_train))[,c(7:11)]
  X_test_KG <- model.matrix(~(KG.4+LAI_year+p_year+Rn_year+e_act_year+e_star_year)^2 -1,as.data.frame(X_test))[,c(7:11)]
  for (p in 1:(KG_nr-1)) {
    KG_int <- model.matrix(~(X_train[,p+7]+LAI_year+p_year+Rn_year+e_act_year+e_star_year)^2 -1,as.data.frame(X_train))[,c(7:11)]
    KG_int_test <- model.matrix(~(X_test[,p+7]+LAI_year+p_year+Rn_year+e_act_year+e_star_year)^2 -1,as.data.frame(X_test))[,c(7:11)]
    X_train_KG <- cbind(X_train_KG,KG_int)
    X_test_KG <- cbind(X_test_KG,KG_int_test)
  }
  #dataset for CV 
  X_train <- (cbind(X_train_int,var_2_train,X_train_KG,X_train[,7:14]))
  colnames(X_train) <- attributeNames[-1]
  X_test <- (cbind(X_test_int,var_2_test,X_test_KG,X_test[,7:14]))
  colnames(X_test) <- attributeNames[-1]
  
  # Perform 10-fold cross-validation to select lambda ---------------------------
  lambdas_to_try <- 10^seq(-3, 3, length.out = 30)
  # Setting alpha = 1 implements lasso regression
  lasso_cv <- cv.glmnet(X_train, y_train, alpha = 1, lambda = lambdas_to_try,
                        standardize = TRUE, nfolds = 10)
  #plot(lasso_cv)
  
  # Best cross-validated lambda
  lambda_cv[k] <- lasso_cv$lambda.1se
  coef[,k] <- coef(lasso_cv, s = lasso_cv$lambda.1se)[,1]
  
  # evaluate training and test error performance for optimal selected value of lambda
  Error_train_rlr[k] = sum( (y_train - cbind(1,X_train) %*% coef[,k])^2 )
  Error_test_rlr[k] = sum( (y_test - cbind(1,X_test) %*% coef[,k])^2 )
}
mu2
sigma2
#### Selecting optimal model ####
lambda_mode = as.numeric(names(which.max(table(lambda_cv))))

########################################
###### Bootstrap #######################
########################################
library(boot)
library(caret)
set.seed(1234)
fold_test <- createFolds(y, k = 10, list = TRUE, returnTrain = FALSE)

lambda_mode

y_train <- y[-c(fold_test$Fold10)];

X_train <- X[-c(fold_test$Fold10), ];

X_train_mean <- apply(X_train[,2:6], 2, mean)
X_train_sd <- apply(X_train[,2:6], 2, sd)

X_train_stand <- X_train
X_train_stand[,2:6] <- scale(X_train[,2:6])

head(X_train_stand)
#obtain interactions
var_2_train <- X_train_stand[,2:6]^2
X_train_int <- model.matrix(~.^2,as.data.frame(X_train_stand[,c(2:6)]))[,-1]
X_train_KG <- model.matrix(~(KG.4+LAI_year+p_year+Rn_year+e_act_year+e_star_year)^2 -1,as.data.frame(X_train_stand))[,c(7:11)]
for (p in 1:(KG_nr-1)) {
  KG_int <- model.matrix(~(X_train[,p+7]+LAI_year+p_year+Rn_year+e_act_year+e_star_year)^2 -1,as.data.frame(X_train_stand))[,c(7:11)]
  X_train_KG <- cbind(X_train_KG,KG_int)
}
#dataset for bootstrap
X_train_stand <- (cbind(X_train_int,var_2_train,X_train_KG,X_train_stand[,7:14]))
colnames(X_train_stand) <- attributeNames[-1]

### model 
glm_boot <- glmnet(X_train_stand,y_train,alpha=1, lambda = lambdas_to_try, standardize = TRUE )
l_opt <- which(round(glm_boot$lambda,8) ==round(lambda_mode,8))
# output 
glm_boot$beta[,l_opt]
glm_boot$a0[l_opt]

# combine X and y
comb <- cbind(X_train_stand,y_train)
names(comb) <- c(attributeNames,y)
comb <- as.data.frame(comb)

m <- dim(comb)[2]
# function obtain parameters
par_glmnet <- function(data, indices){
  print(length(unique(indices)))
  m <- dim(data)[2]
  d <- data[indices,]
  X_ <- d[,-m]
  X_ <- as.matrix(X_)
  y_ <- as.vector(d$y)
  fit <- glmnet(X_,y_,alpha=1,lambda= lambdas_to_try, standardize = TRUE)
  ret <- c(as.vector(fit$a0[l_opt]),as.vector(fit$beta[,l_opt]))
  return(ret)
}

# bootstrapping with 1000 replications
results <- boot(data=comb, statistic=par_glmnet,
                R=500)
plot(results, index = 60)
#
#### MEDIAN ####
boot_median <- apply(results$t, 2, median)
n <- dim(results$t)[1]
df_median <- matrix(rep(boot_median,n),ncol = P, byrow = TRUE)
boot_delta <- df_median - results$t
delta_quantile <- apply(boot_delta,2, quantile,  c(.025, .975))
boot_CIp <- boot_median - delta_quantile[1,]
boot_CIn <- boot_median - delta_quantile[2,]
df_boot <- cbind(boot_median,boot_CIn,boot_CIp)
df_boot <- as.data.frame(df_boot, row.names = attributeNames)
names(df_boot) <- c("Estimate", "Lower CI", "Upper CI")

df_boot$names <- attributeNames
#df_boot$cv_est <- esti

#### Plot ####
library(reshape2)
df_boot_new <- melt(df_boot)
df_boot_new$names <- factor(df_boot$names, levels = df_boot$names)
head(df_boot_new)
nn <- length(attributeNames)
df_line <- as.data.frame(df_boot_new[1:(nn*2),c(1,3)])
df_line$value <- c(rep(-2.5,nn),rep(2.5,nn))
png(paste("C:/Users/ellen/Documents/BA/Machinelearning/Bootstrap/ParEsti_22_allInt_boot_",Sys.Date(),".png",sep=""), width = 1000, height = 500)
ggplot(data = df_boot_new, mapping = aes(x = names,y=value)) + 
  geom_line(data = df_line, linetype = "dashed", aes(color = names))+
  geom_point(data = df_boot_new[df_boot_new$variable == "Estimate",], size = 3 , aes(color = names))+
  geom_line(data = df_boot_new, size=0.8)+
  geom_hline(yintercept=0, linetype="dashed", color = "grey20")+
  labs(title = "Parameter Estimates with CI" , 
       x = "Parameters",
       y = "Estimate")+
  theme(axis.text.x = element_text(colour = "grey20", size = 16, angle = 90,
                                   hjust = 0.5, vjust = 0.5),
        axis.text.y=element_text(colour = "grey20", size = 16),
        text = element_text(size = 16))+
  theme(legend.position = "none")
dev.off()
df_line$value <- c(rep(-0.5,nn),rep(2.5,nn))
png(paste("C:/Users/ellen/Documents/BA/Machinelearning/Bootstrap/ParEsti_22_allInt_boot_var_",Sys.Date(),".png",sep=""), width = 1000, height = 500)
ggplot(data = df_boot_new[c(1:21),], mapping = aes(x = names,y=value)) + 
  geom_line(data = df_line[c(1:21,70:90),], linetype = "dashed", aes(color = names))+
  geom_point(data = df_boot_new[df_boot_new$variable == "Estimate",][c(1:21),], size = 3 , aes(color = names))+
  geom_line(data = df_boot_new[c(1:21,70:90,139:159),], size=0.8)+
  geom_hline(yintercept=0, linetype="dashed", color = "grey20")+
  labs(title = "Parameter Estimates with CI" , 
       x = "Parameters",
       y = "Estimate")+
  theme(axis.text.x = element_text(colour = "grey20", size = 16, angle = 90,
                                   hjust = 0.5, vjust = 0.5),
        axis.text.y=element_text(colour = "grey20", size = 16),
        text = element_text(size = 16))+
  theme(legend.position = "none")
dev.off()
df_line$value <- c(rep(-2.5,nn),rep(2.5,nn))
png(paste("C:/Users/ellen/Documents/BA/Machinelearning/Bootstrap/ParEsti_22_allInt_boot_KG_",Sys.Date(),".png",sep=""), width = 1000, height = 500)
ggplot(data = df_boot_new[-c(1:21),], mapping = aes(x = names,y=value)) + 
  geom_line(data = df_line[-c(1:21,70:90),], linetype = "dashed", aes(color = names))+
  geom_point(data = df_boot_new[df_boot_new$variable == "Estimate",][-c(1:21),], size = 3 , aes(color = names))+
  geom_line(data = df_boot_new[-c(1:21,70:90,139:159),], size=0.8)+
  geom_hline(yintercept=0, linetype="dashed", color = "grey20")+
  labs(title = "Parameter Estimates with CI" , 
       x = "Parameters",
       y = "Estimate")+
  theme(axis.text.x = element_text(colour = "grey20", size = 16, angle = 90,
                                   hjust = 0.5, vjust = 0.5),
        axis.text.y=element_text(colour = "grey20", size = 16),
        text = element_text(size = 16))+
  theme(legend.position = "none")
dev.off()

# to data frame latex table
df_boot_table <- cbind(boot_median,boot_CIn,boot_CIp)
df_boot_table <- as.data.frame(df_boot_table, row.names = attributeNames)
names(df_boot_table) <- c("Estimate", "Lower CI", "Upper CI")
library(xtable)
head(df_boot_table)
print(xtable(df_boot_table, type = "latex"), 
      file = paste("C:/Users/ellen/Documents/BA/Machinelearning/Bootstrap/ParEsti_22_allInt_median_",Sys.Date(),".tex",sep=""))
write.csv2(df_boot_table,file = paste("C:/Users/ellen/Documents/BA/Machinelearning/Bootstrap/ParEsti_22_allInt_median_",Sys.Date(),".csv",sep=""))

#####
par_not_zero <- as.data.frame(cbind(0,0))
names(par_not_zero) <- c("Parameter", "Estimate")
par_not_zero$Parameter <- as.character(par_not_zero$Parameter)

for (i in 1:nn) {
  if(min(abs(df_boot_table[i,c(2,3)]))!= 0 ){
    par_not_zero[i,1] <- row.names(df_boot_table)[i]
    par_not_zero[i,2] <- df_boot_table[i,1]
  }else{
    par_not_zero[i,1] <- row.names(df_boot_table)[i]
    par_not_zero[i,2] <- 0
  }
}
#### Natural to working parameters #####
B_int <- as.data.frame(cbind(0,0))
names(B_int) <- c("Parameter", "Estimate")
B_int$Parameter <- as.character(B_int$Parameter)
p = 7
for (j in 1:4) {
  q = 5-j
  for (k in 1:q) {
    B_int[p-6,2] <- par_not_zero[p,2]/(X_train_sd[j]*X_train_sd[5-q+k])
    B_int[p-6,1] <- par_not_zero[p,1]
    p = p+1
  }
}
B_intKG <- as.data.frame(cbind(0,0))
names(B_intKG) <- c("Parameter", "Estimate")
B_intKG$Parameter <- as.character(B_int$Parameter)
p = 22
for (j in 1:KG_nr) {
  for (k in 1:5) {
    B_intKG[p-21,2] <- par_not_zero[p,2]/(X_train_sd[k])
    B_intKG[p-21,1] <- par_not_zero[p,1]
    p = p+1
  }
}
B_KG <- as.data.frame(cbind(0,0))
names(B_KG) <- c("Parameter", "Estimate")
B_KG$Parameter <- as.character(B_int$Parameter)
p = 22
for (i in 1:KG_nr) {
  val <- par_not_zero[61+i,2]
  for (j in 1:5) {
    val <- val - par_not_zero[p,2]*X_train_mean[j]/X_train_sd[j] 
    p = p + 1
  }
  B_KG[i,2] <- val
  B_KG[i,1] <- par_not_zero[61+i,1]
}
B_2 <- as.data.frame(cbind(0,0))
names(B_2) <- c("Parameter", "Estimate")
B_2$Parameter <- as.character(B_2$Parameter)
p = 17 
for (i in 1:5) {
  B_2[p-16,2] <- par_not_zero[p,2]/(X_train_sd[i]^2)
  B_2[p-16,1] <- par_not_zero[p,1]
  p = p+1
}
B_ <- as.data.frame(cbind(0,0))
names(B_) <- c("Parameter", "Estimate")
B_$Parameter <- as.character(B_$Parameter)
val <- par_not_zero[2,2]/X_train_sd[1]
val <- val - par_not_zero[17,2]*2*X_train_mean[1]/(X_train_sd[1]^2)
for (j in 1:4) {
  val = val - par_not_zero[j+6,2]*X_train_mean[j+1]/(X_train_sd[1]*X_train_sd[j+1])
}
B_[1,2] <- val
B_[1,1] <- "LAI"

val <- par_not_zero[3,2]/X_train_sd[2]
val <- val - par_not_zero[18,2]*2*X_train_mean[2]/(X_train_sd[2]^2)
val = val - par_not_zero[7,2]*X_train_mean[1]/(X_train_sd[2]*X_train_sd[1])
for (j in 1:3) {
  val = val - par_not_zero[j+10,2]*X_train_mean[j+2]/(X_train_sd[2]*X_train_sd[j+2])
}
B_[2,2] <- val
B_[2,1] <- "p"

val <- par_not_zero[4,2]/X_train_sd[3]
val <- val - par_not_zero[19,2]*2*X_train_mean[3]/(X_train_sd[3]*X_train_sd[3])
val = val - par_not_zero[8,2]*X_train_mean[1]/(X_train_sd[3]*X_train_sd[1])
val = val - par_not_zero[11,2]*X_train_mean[2]/(X_train_sd[3]*X_train_sd[2])
for (j in 1:2) {
  val = val - par_not_zero[j+13,2]*X_train_mean[j+3]/(X_train_sd[3]*X_train_sd[j+3])
}
B_[3,2] <- val
B_[3,1] <- "Rn"

val <- par_not_zero[5,2]/X_train_sd[4]
val <- val - par_not_zero[20,2]*2*X_train_mean[4]/(X_train_sd[4]^2)
val = val - par_not_zero[9,2]*X_train_mean[1]/(X_train_sd[4]*X_train_sd[1])
val = val - par_not_zero[12,2]*X_train_mean[2]/(X_train_sd[4]*X_train_sd[2])
val = val - par_not_zero[14,2]*X_train_mean[3]/(X_train_sd[4]*X_train_sd[3])
val = val - par_not_zero[16,2]*X_train_mean[5]/(X_train_sd[4]*X_train_sd[5])
B_[4,2] <- val
B_[4,1] <- "e"

val <- par_not_zero[6,2]/X_train_sd[5]
val <- val - par_not_zero[21,2]*2*X_train_mean[5]/(X_train_sd[5]^2)
val = val - par_not_zero[10,2]*X_train_mean[1]/(X_train_sd[5]*X_train_sd[1])
val = val - par_not_zero[13,2]*X_train_mean[2]/(X_train_sd[5]*X_train_sd[2])
val = val - par_not_zero[15,2]*X_train_mean[3]/(X_train_sd[5]*X_train_sd[3])
val = val - par_not_zero[16,2]*X_train_mean[4]/(X_train_sd[5]*X_train_sd[4])
B_[5,2] <- val
B_[5,1] <- "e*"

B_0 <- as.data.frame(cbind(0,0))
names(B_0) <- c("Parameter", "Estimate")
B_0$Parameter <- as.character(B_$Parameter)
val <- par_not_zero[1,2]
for (i in 1:5) {
  val <- val - ( par_not_zero[i+1,2]*X_train_mean[i] ) / ( X_train_sd[i]) +
    ( par_not_zero[i+16,2]*X_train_mean[i]^2 ) / ( X_train_sd[i]^2 ) 
}
p <- 7
for (j in 1:4) {
  q = 5-j
  for (k in 1:q) {
    val  <- val + ( par_not_zero[p,2] * X_train_mean[j] *X_train_mean[5-q+k] ) /
      ( X_train_sd[j] * X_train_sd[5-q+k] )
    p = p+1
  }
}
B_0[1,2] <- val
B_0[1,1] <- "Intercept"

## all parameters 
par <- c(B_0[,2],B_[,2],B_int[,2],B_2[,2],B_intKG[,2],B_KG[,2])
names(par) <- attributeNames

par2 <- par
par2[par2==0] <- NA
par2 <- par2[complete.cases(par2)]

print(xtable(as.data.frame(t(par2)),type="latex",digits = 3),
      file = paste("C:/Users/ellen/Documents/BA/Model presentation/22/ParEsti_22_natural_allInt_zero_",Sys.Date(),".tex",sep=""))
write.csv2(as.data.frame(par),file = paste("C:/Users/ellen/Documents/BA/Model presentation/22/ParEsti_22_natural_allInt",Sys.Date(),".csv",sep=""))

df_par <- as.data.frame(par2)
df_par$names <- row.names(as.data.frame(df_par))
ggplot(data = df_par, aes(x = par2, y =names ))+
  geom_point(aes(color = names))+
  labs(title = "Parameter Estimates in Natural Domain" , 
       x = "Estimates",
       y = "Non-zero parameters")+
  theme(axis.text.x = element_text(colour = "grey20", size = 10),
        axis.text.y=element_text(colour = "grey20", size = 10),
        text = element_text(size = 16))+
  theme(legend.position = "none")

##### Predict plots ##### 
par_test <- read.csv("C:/Users/ellen/Documents/BA/Model presentation/22/ParEsti_22_natural_allInt2020-06-09.csv",dec=",",sep=";")
par <- par_test$par
y_test <- y[c(fold_test$Fold10)]

X_test <- X[c(fold_test$Fold10), ]

X_test_ <- cbind(model.matrix(~.^2,as.data.frame(X_test[,2:6])) , X_test[,-1]^2 )
X_test_KG <- model.matrix(~(KG.4+LAI_year+p_year+Rn_year+e_act_year+e_star_year)^2 -1,as.data.frame(X_test))[,c(7:11)]
for (p in 1:(KG_nr-1)) {
  KG_int <- model.matrix(~(X_test[,p+7]+LAI_year+p_year+Rn_year+e_act_year+e_star_year)^2 -1,as.data.frame(X_test))[,c(7:11)]
  X_test_KG <- cbind(X_test_KG,KG_int)
}
X_test_ <- cbind(X_test_[,1:21],X_test_KG,X_test_[,22:29])
X_test <- X_test_
colnames(X_test) <- attributeNames
y_pred <- X_test%*%par

df_test <- cbind(y_test,y_pred)
df_test <- as.data.frame(df_test)
### Compute nrmse
squared_sums <- sum((y_test - y_pred)^2)
mse <- squared_sums/length(y_test)
rmse  <- sqrt(mse)
nrmse = rmse/(quantile(y_test)[4]-quantile(y_test)[2])
NRMSE = round(nrmse,3)

### Compute R^2 
rss <- sum((y_pred - y_test) ^ 2)  ## residual sum of squares
tss <- sum((y_test - mean(y_test)) ^ 2)  ## total sum of squares
rsq <- 1 - rss/tss
Rsq <- round(rsq,3)
#### Nice plot 
library(ggplot2)
max_plot <- max(max(y_test),max(y_pred))
min_plot <- min(min(y_test),min(y_pred))

one_hun <- (max_plot-min_plot)/100
one_25 <- (max_plot-min_plot)/25

y_max <- max_plot - one_hun
y_min <- max_plot-(max_plot-min_plot)/4 - one_hun

x_max <- one_25 + min_plot+(max_plot-min_plot)/4
x_min <- min_plot+ one_25

title_ <- "Performance of Model on Permanent Crops with KG"
#png(paste("C:/Users/ellen/Documents/BA/Predictions/Pred_22_allInt_",Sys.Date(),".png",sep=""), width = 1000, height = 500)
png(paste("C:/Users/ellen/Documents/BA/Model presentation/22/Pred_22_allInt_",Sys.Date(),".png",sep=""), width = 1000, height = 500)
ggplot(data=df_test, aes(x = y_test, y = y_pred)) + geom_point(alpha = 4/10)+
  labs(x="Observed values", y="Predicted values", title = title_)+
  geom_abline(intercept = 0, slope = 1)+
  annotate("text", x = mean(c(x_min,x_max)), y = y_max - 1.5*one_25,
           label = paste0("NRMSE==", NRMSE,sep=""), parse = TRUE, size = 6.5)+
  annotate("text", x =  mean(c(x_min,x_max)), y = y_max - 4*one_25,
           label = paste0("R^2==", Rsq,sep=""), parse = TRUE, size = 6.5)+
  annotate("rect", xmin =x_min , xmax = x_max, ymin = y_min, ymax = y_max,
           alpha = .2)+
  theme(text = element_text(size = 20)) + 
  coord_cartesian(xlim = c(min_plot, max_plot), ylim = c(min_plot, max_plot))
 dev.off()
err <- y_test-y_pred
df_res <- cbind(y_pred,err)
df_res <- as.data.frame(df_res)
png(paste("C:/Users/ellen/Documents/BA/Model presentation/22/ResVsFit_22_",Sys.Date(),".png",sep=""), width = 1000, height = 500)
ggplot(data = df_res, aes(x=y_pred, y = err))+
  geom_hline(yintercept=0, linetype="dashed", color = 2)+
  geom_point(alpha = 4/10) + 
  labs(title = "Residuals vs Fitted" , 
       x = "Fitted values",
       y = "Residuals")+
  theme(axis.text.x = element_text(colour = "grey20", size = 20),
        axis.text.y=element_text(colour = "grey20", size = 20),
        text = element_text(size = 25))
dev.off()
png(paste("C:/Users/ellen/Documents/BA/Model presentation/22/qq_22_",Sys.Date(),".png",sep=""), width = 1000, height = 500)
qqnorm(err, pch = 1, frame = FALSE)
qqline(err, col = "steelblue", lwd = 2)
dev.off()
err <- as.data.frame(err)
library(devtools)
library(qqplotr)
png(paste("C:/Users/ellen/Documents/BA/Model presentation/22/qq_22_gg_",Sys.Date(),".png",sep=""), width = 1000, height = 500)
ggplot(data = err, mapping = aes(sample = V1)) +
  stat_qq_band() +
  stat_qq_line() +
  stat_qq_point() +
  labs(x = "Theoretical Quantiles", y = "Sample Quantiles", title = "Normal Q-Q plot")+
  theme(axis.text.x = element_text(colour = "grey20", size = 20),
        axis.text.y=element_text(colour = "grey20", size = 20),
        text = element_text(size = 25))

dev.off()
################ Interpret interactions in model ################
X_mean <- apply(X[,1:6],2, mean)
X_0.1 <- apply(X[,1:6],2, quantile,0.1)
X_0.9 <- apply(X[,1:6],2,quantile,0.9)
X_0.5 <- apply(X[,1:6],2, quantile, 0.5)
#par_csv <- read.csv("C:/Users/ellen/Documents/BA/Model presentation/22/ParEsti_natural_22_allInt2020-06-09.csv",header = TRUE, sep=delim, dec=dec, stringsAsFactors=FALSE)
#par <- par_csv
#read.csv("C:/Users/ellen/Documents/BA/Machinelearning/Bootstrap/ParEsti_22_allInt_median.csv", 
#         header = TRUE, sep=delim, dec=dec, stringsAsFactors=FALSE)
#### Interaction LAI KG4,5,6,7,8,9,14,15 #####
LAI_seq <- seq(min(dat$LAI_year), max(dat$LAI_year), by = 0.5)
fun_LAI_KG <- function(x,kg) {
  sum(X_0.5[-2]*par[c(1,3:6)])+
    par[2]*x+ par[17]*(x^2)+
    sum(X_0.5[-c(1,2)]*par[c(7:10)])*x+
    sum( par[c(11,13,14,15,18,19,21)] *X_0.5[c(3,3,4,4,3,4,6)]*X_0.5[c(4,6,5,6,3,4,6)])+
    sum(par[c((23+(kg-1)*5):(26+(kg-1)*5))]*X_0.5[c(3:6)]) +
    par[62+(kg-1)]+
    par[22+(kg-1)*5]*x
}
m <- length(LAI_seq)
df_LAI_KG <- as.data.frame(cbind(rep(LAI_seq,8),c(
  fun_LAI_KG(LAI_seq,1),
  fun_LAI_KG(LAI_seq,2),
  fun_LAI_KG(LAI_seq,3),
  fun_LAI_KG(LAI_seq,4),
  fun_LAI_KG(LAI_seq,5),
  fun_LAI_KG(LAI_seq,6),
  fun_LAI_KG(LAI_seq,7),
  fun_LAI_KG(LAI_seq,8))))
df_LAI_KG$`KG Map` <-  c(rep("KG4",m),rep("KG5",m),rep("KG6",m),
                         rep("KG7",m),rep("KG8",m),rep("KG9",m),
                         rep("KG14",m),rep("KG15",m)) 
names = c("KG4","KG5","KG6","KG7","KG8","KG9","KG14","KG15")
df_LAI_KG$`KG Map` <- factor(df_LAI_KG$`KG Map`, levels = names)
png(paste("C:/Users/ellen/Documents/BA/Model presentation/22/par_22_LAI_KG_",Sys.Date(),".png",sep=""), width = 1000, height = 500)
ggplot(df_LAI_KG,aes(x=V1,y=V2))+
  geom_line(aes(color = `KG Map`),size = 1)+
  labs(x="Leaf Area Index Trend",
       y="Evapotranspiration",
       title = "Function of LAI with Fixed Values for Remaining Parameters")+
  theme(axis.text.x = element_text(colour = "grey20", size =24),
        axis.text.y=element_text(colour = "grey20", size = 24),
        text = element_text(size = 25))
dev.off()

#### Interactions p KG4,5,6,7,8,9,14,15 #####
p_seq <- seq(min(dat$p_year), max(dat$p_year), by = 0.1)
fun_p_KG <- function(x,kg) {
  sum(X_0.5[-3]*par[c(1,2,4:6)])+
    par[3]*x+ par[18]*x^2+
    sum(X_0.5[-c(1,3)]*par[c(7,11:13)])*x+
    sum( par[c(8:10,14:17,19:21)] *X_0.5[c(2,2,2,4,4,5,2,4,5,6)]*X_0.5[c(4,5,6,5,6,6,2,4,5,6)])+
    sum(par[c((22+(kg-1)*5),(24+(kg-1)*5):(26+(kg-1)*5))]*X_0.5[c(2,4:6)]) +
    par[62+(kg-1)]+
    par[23+(kg-1)*5]*x
}
m <- length(p_seq)
df_p_KG <- as.data.frame(cbind(rep(p_seq,8),c(
  fun_p_KG(p_seq,1),
  fun_p_KG(p_seq,2),
  fun_p_KG(p_seq,3),
  fun_p_KG(p_seq,4),
  fun_p_KG(p_seq,5),
  fun_p_KG(p_seq,6),
  fun_p_KG(p_seq,7),
  fun_p_KG(p_seq,8))))
df_p_KG$`KG Map` <-  c(rep("KG4",m),rep("KG5",m),rep("KG6",m),
                         rep("KG7",m),rep("KG8",m),rep("KG9",m),
                         rep("KG14",m),rep("KG15",m)) 
names = c("KG4","KG5","KG6","KG7","KG8","KG9","KG14","KG15")
df_p_KG$`KG Map` <- factor(df_p_KG$`KG Map`, levels = names)
png(paste("C:/Users/ellen/Documents/BA/Model presentation/22/par_22_p_KG_",Sys.Date(),".png",sep=""), width = 1000, height = 500)
ggplot(df_p_KG,aes(x=V1,y=V2))+
  geom_line(aes(color = `KG Map`),size = 1)+
  labs(x="Precipitation Trend",
       y="Evapotranspiration",
       title = "Function of p with Fixed Values for Remaining Parameters")+
  theme(axis.text.x = element_text(colour = "grey20", size =24),
        axis.text.y=element_text(colour = "grey20", size = 24),
        text = element_text(size = 25))
dev.off()


#### Interaction Rn KG4,5,6,7,8,9,14,15 ######
Rn_seq <- seq(min(dat$Rn_year), max(dat$Rn_year), by = 1)
fun_Rn_KG <- function(x,kg) {
  sum(X_0.5[-4]*par[c(1:3,5,6)])+
    par[4]*x+par[19]*(x^2)+
    sum(X_0.5[-c(1,4)]*par[c(8,11,14,15)])*x+
    sum( par[c(7,9,10,12,13,16:18,20,21)] *X_0.5[c(2,2,2,4,4,5,2,3,5,6)]*X_0.5[c(3,5,6,5,6,6,2,3,5,6)])+
    sum(par[c((22+(kg-1)*5),(23+(kg-1)*5),(25+(kg-1)*5),(26+(kg-1)*5))]*X_0.5[c(2,3,5,6)]) +
    par[62+(kg-1)]+
    par[24+(kg-1)*5]*x
}
m <- length(Rn_seq)
df_Rn_KG <- as.data.frame(cbind(rep(Rn_seq,8),c(
  fun_Rn_KG(Rn_seq,1),
  fun_Rn_KG(Rn_seq,2),
  fun_Rn_KG(Rn_seq,3),
  fun_Rn_KG(Rn_seq,4),
  fun_Rn_KG(Rn_seq,5),
  fun_Rn_KG(Rn_seq,6),
  fun_Rn_KG(Rn_seq,7),
  fun_Rn_KG(Rn_seq,8))))
df_Rn_KG$`KG Map` <-  c(rep("KG4",m),rep("KG5",m),rep("KG6",m),
                        rep("KG7",m),rep("KG8",m),rep("KG9",m),
                        rep("KG14",m),rep("KG15",m)) 
names = c("KG4","KG5","KG6","KG7","KG8","KG9","KG14","KG15")
df_Rn_KG$`KG Map` <- factor(df_Rn_KG$`KG Map`, levels = names)
png(paste("C:/Users/ellen/Documents/BA/Model presentation/22/par_22_Rn_KG_",Sys.Date(),".png",sep=""), width = 1000, height = 500)
ggplot(df_Rn_KG,aes(x=V1,y=V2))+
  geom_line(aes(color = `KG Map`),size = 1)+
  labs(x="Net Radiation Trend",
       y="Evapotranspiration",
       title = "Function of Rn with Fixed Values for Remaining Parameters")+
  theme(axis.text.x = element_text(colour = "grey20", size =24),
        axis.text.y=element_text(colour = "grey20", size = 24),
        text = element_text(size = 25))
dev.off()

#### Interaction e KG4,5,6,7,8,9,14,15 ######
e_seq <- seq(min(dat$e_act_year), max(dat$e_act_year), by = 0.01)
fun_e_KG <- function(x,kg) {
  sum(X_0.5[-5]*par[c(1:4,6)])+
    par[5]*x+ par[20]*(x^2) +
    sum(X_0.5[-c(1,5)]*par[c(9,12,14,16)])*x+
    sum( par[c(7,8,10,11,13,15,17:19,21)] *X_0.5[c(2,2,2,3,3,4,2,3,4,6)]*X_0.5[c(3,4,6,4,6,6,2,3,4,6)])+
    sum(par[c((22+(kg-1)*5):(24+(kg-1)*5),(26+(kg-1)*5))]*X_0.5[c(2:4,6)]) +
    par[62+(kg-1)]+
    par[25+(kg-1)*5]*x
}
m <- length(e_seq)
df_e_KG <- as.data.frame(cbind(rep(e_seq,8),c(
  fun_e_KG(e_seq,1),
  fun_e_KG(e_seq,2),
  fun_e_KG(e_seq,3),
  fun_e_KG(e_seq,4),
  fun_e_KG(e_seq,5),
  fun_e_KG(e_seq,6),
  fun_e_KG(e_seq,7),
  fun_e_KG(e_seq,8))))
df_e_KG$`KG Map` <-  c(rep("KG4",m),rep("KG5",m),rep("KG6",m),
                       rep("KG7",m),rep("KG8",m),rep("KG9",m),
                       rep("KG14",m),rep("KG15",m)) 
names = c("KG4","KG5","KG6","KG7","KG8","KG9","KG14","KG15")
df_e_KG$`KG Map` <- factor(df_e_KG$`KG Map`, levels = names)
png(paste("C:/Users/ellen/Documents/BA/Model presentation/22/par_22_e_KG_",Sys.Date(),".png",sep=""), width = 1000, height = 500)
ggplot(df_e_KG,aes(x=V1,y=V2))+
  geom_line(aes(color = `KG Map`),size = 1)+
  labs(x="Actual Water Vapor Trend",
       y="Evapotranspiration",
       title = "Function of e with Fixed Values for Remaining Parameters")+
  theme(axis.text.x = element_text(colour = "grey20", size =24),
        axis.text.y=element_text(colour = "grey20", size = 24),
        text = element_text(size = 25))
dev.off()

#### Interaction e* KG4,5,6,7,8,9,14,15 ######
e_star_seq <- seq(min(dat$e_star_year), max(dat$e_star_year), by = 0.01)
fun_e_star_KG <- function(x,kg) {
  sum(X_0.5[-6]*par[c(1:5)])+
    par[6]*x+ par[21]*(x^2) +
    sum(X_0.5[-c(1,6)]*par[c(10,13,15,16)])*x+
    sum( par[c(7,8,9,11,12,14,17:20)] *X_0.5[c(2,2,2,3,3,4,2,3,4,5)]*X_0.5[c(3,4,5,4,5,5,2,3,4,5)])+
    sum(par[c((22+(kg-1)*5):(25+(kg-1)*5))]*X_0.5[c(2:5)]) +
    par[62+(kg-1)]+
    par[26+(kg-1)*5]*x
}
m <- length(e_star_seq)
df_e_star_KG <- as.data.frame(cbind(rep(e_star_seq,8),c(
  fun_e_star_KG(e_star_seq,1),
  fun_e_star_KG(e_star_seq,2),
  fun_e_star_KG(e_star_seq,3),
  fun_e_star_KG(e_star_seq,4),
  fun_e_star_KG(e_star_seq,5),
  fun_e_star_KG(e_star_seq,6),
  fun_e_star_KG(e_star_seq,7),
  fun_e_star_KG(e_star_seq,8))))
df_e_star_KG$`KG Map` <-  c(rep("KG4",m),rep("KG5",m),rep("KG6",m),
                            rep("KG7",m),rep("KG8",m),rep("KG9",m),
                            rep("KG14",m),rep("KG15",m)) 
names = c("KG4","KG5","KG6","KG7","KG8","KG9","KG14","KG15")
df_e_star_KG$`KG Map` <- factor(df_e_star_KG$`KG Map`, levels = names)
png(paste("C:/Users/ellen/Documents/BA/Model presentation/22/par_22_e_star_KG_",Sys.Date(),".png",sep=""), width = 1000, height = 500)
ggplot(df_e_star_KG,aes(x=V1,y=V2))+
  geom_line(aes(color = `KG Map`),size = 1)+
  labs(x="Saturated Water Vapor Trend",
       y="Evapotranspiration",
       title = "Function of e* with Fixed Values for Remaining Parameters")+
  theme(axis.text.x = element_text(colour = "grey20", size =24),
        axis.text.y=element_text(colour = "grey20", size = 24),
        text = element_text(size = 25))
dev.off()
