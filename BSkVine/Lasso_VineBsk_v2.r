## Lasso-regression
rm(list=ls())

library(glmnet)  # for ridge regression
library(dplyr)   # for data cleaning
library(psych)   # for function tr() to compute trace of a matrix
library(cvTools)

packages <- c('coefplot', 'DT')
purrr::walk(packages, library, character.only=TRUE)

setwd("~/Documents/DTU/5. Semester/02450 Introduktion til machine learning og data mining/02450Toolbox_R")
source("setup.R")

# Load data
delim = ";"
dec = "," 
setwd("~/Documents/DTU/6. Semester/Bachelorprojekt/Data_Ellen_Maria/Cross-validation")
dat <- read.csv("Bsk_vineyard_2_int.csv", header = TRUE, sep=delim, dec=dec, stringsAsFactors=FALSE)
#X <- as.matrix(dat[,-c(1,8,30,31)])
#X <- as.matrix(dat[,-c(6,7,8,13,14,18,19,22,23,25,26,27,28,29,30,31)]) #w/out e_act + e_star
#X <- as.matrix(dat[,-c(5,8,12,17,21,24,27,28,30,31)]) # w/out VPD
X <- as.matrix(dat[,c(1:4,6,7)])
head(X)
attributeNames <- colnames(X)
#attributeNames  <- c("Intercept","LAI","p","Rn","e_act","e_star",
#  "LAI^2","p^2","Rn^2","e_act^2","e_star^2",
#  "LAI:p","LAI:Rn","LAI:e_act","LAI:e_star","p:Rn","p:e_act","p:e_star","Rn:e_act","Rn:e_star","e_act:e_star")

attributeNames  <- c("Intercept","LAI","p","Rn","e","e*",
                     "LAI:p","LAI:Rn","LAI:e","LAI:e*","p:Rn","p:e","p:e*","Rn:e","Rn:e*","e:e*",
                     "LAI^2","p^2","Rn^2","e^2","e*^2")
P <- length(attributeNames)

y <- dat[,8]

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
mu2 <- matrix(rep(NA, times=(dim(X)-1)[2]*K), nrow=(dim(X)-1)[2])
sigma2 <- matrix(rep(NA, times=(dim(X)-1)[2]*K), nrow=(dim(X)-1)[2])

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
  
  mu2[,k]<- colMeans(X_train[, 2:(dim(X))[2]])
  sigma2[,k] <- apply(X_train[, 2:(dim(X))[2]], 2, sd)
  
  X_train[, 2:dim(X)[2]] <- scale(X_train[, 2:dim(X)[2]], mu2[,k], sigma2[,k])
  X_test[, 2:dim(X)[2]] <- scale(X_test[, 2:dim(X)[2]], mu2[,k], sigma2[,k])
  
  #obtain interactions
  var_2_train <- X_train[,-1]^2
  var_2_test <- X_test[,-1]^2
  
  X_train <- model.matrix(~.^2,as.data.frame(X_train[,-1]))[,-1]
  X_train <- (cbind(X_train,var_2_train))
  colnames(X_train) <- attributeNames[-1]
  
  X_test <- model.matrix(~.^2,as.data.frame(X_test[,-1]))[,-1]
  X_test <- (cbind(X_test,var_2_test))
  colnames(X_test) <- attributeNames[-1]
  
  # Perform 10-fold cross-validation to select lambda ---------------------------
  lambdas_to_try <- 10^seq(-3, 3, length.out = 30)
  # Setting alpha = 1 implements lasso regression
  lasso_cv <- cv.glmnet(X_train, y_train, alpha = 1, lambda = lambdas_to_try,
                        standardize = TRUE, nfolds = 10)
  plot(lasso_cv)
  plot(lasso_cv$glmnet.fit, "lambda")
  # Best cross-validated lambda
  lambda_cv[k] <- lasso_cv$lambda.1se
  coef[,k] <- coef(lasso_cv, s = lasso_cv$lambda.1se)[,1]
  
  # evaluate training and test error performance for optimal selected value of lambda
  Error_train_rlr[k] = sum( (y_train - cbind(1,X_train) %*% coef[,k])^2 )
  Error_test_rlr[k] = sum( (y_test - cbind(1,X_test) %*% coef[,k])^2 )
}

png(paste(file = "~/Documents/DTU/6. Semester/Bachelorprojekt/Data_Ellen_Maria/Cross-validation/plots/PLOT_Lasso_BSkVine.png"), width = 500, height = 500)
coefplot <- coefplot(lasso_cv, lambda = 'lambda.1se', pointSize = 4)
coefplot +  theme (axis.text.x  = element_text(size=16)) +
  theme(axis.text.y = element_text(size=16)) + theme(title = element_text(size=16))
dev.off()

#### Selecting optimal model ###
# choosing the lambda that occurs the most
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

X_train_mean <- apply(X_train[,2:M], 2, mean)
X_train_sd <- apply(X_train[,2:M], 2, sd)

X_train_stand <- X_train
X_train_stand[,2:(dim(X_train))[2]] <- scale(X_train[,2:(dim(X_train))[2]])
#obtain interactions
var_2_train <- X_train_stand[,-1]^2
X_train_stand <- model.matrix(~.^2,as.data.frame(X_train_stand[,-1]))[,-1]
X_train_stand <- (cbind(X_train_stand,var_2_train))
colnames(X_train_stand) <- attributeNames[-1]

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
plot(results, index = 1)
#
#### skal have mean og sd ####
#boot_sd <- apply(results$t, 2, sd)
boot_mean <- apply(results$t, 2, mean)
df_mean <- matrix(rep(boot_mean,n),ncol = 21, byrow = TRUE)
boot_delta_m <- df_mean - results$t
delta_quantile_m <- apply(boot_delta_m,2, quantile,  c(.05, .95))
boot_CIp_m <- boot_mean - delta_quantile_m[1,]
boot_CIn_m <- boot_mean - delta_quantile_m[2,]

df_boot_m <- cbind(boot_mean,boot_CIn,boot_CIp)
df_boot_m <- as.data.frame(df_boot_m, row.names = attributeNames)
names(df_boot_m) <- c("Estimate", "Lower CI", "Upper CI")

df_boot$names <- attributeNames

#### MEDIAN ####
boot_median <- apply(results$t, 2, median)
n <- dim(results$t)[1]
df_median <- matrix(rep(boot_median,n),ncol = 21, byrow = TRUE)

boot_delta <- df_median - results$t
#boot_delta2 <- sweep(results$t,2,results$t0)
#boot_median + apply(sweep(results$t,2,boot_median),2,quantile,  c(.025, .975))
delta_quantile <- apply(boot_delta,2, quantile,  c(.025, .975))
#delta_quantile2 <- apply(boot_delta2,2, quantile,  c(.025, .975))

apply(results$t,2, quantile,  c(.025, .975))
boot_CIp <- boot_median - delta_quantile[1,]
boot_CIn <- boot_median - delta_quantile[2,]
#boot_CIp <- boot_mean + 2.576*(boot_sd/sqrt(n))
#boot_CIn <- boot_mean - 2.576*(boot_sd/sqrt(n))
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
df_line$value <- c(rep(-3,nn),rep(1.5,nn))
png("C:/Users/ellen/Documents/BA/Machinelearning/Bootstrap/ParEsti_BskVine_boot.png", width = 1000, height = 500)
ggplot(data = df_boot_new, mapping = aes(x = names,y=value)) + 
  geom_line(data = df_line, linetype = "dashed", aes(color = names))+
  geom_line(data = df_boot_new, size=0.8)+
  geom_point(data = df_boot_new[df_boot_new$variable == "Estimate",], size = 3 , aes(color = names))+
  geom_hline(yintercept=0, linetype="dashed", color = "grey40")+
  labs(title = "Parameter Estimates with CI" , 
       x = "Parameters",
       y = "Estimates")+
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
print(xtable(df_boot_table, type = "latex", digits = 5), 
      file = paste("C:/Users/ellen/Documents/BA/Machinelearning/Bootstrap/ParEsti_BskVine_median_",Sys.Date(),".tex",sep=""))
write.csv2(df_boot_table,file = paste("C:/Users/ellen/Documents/BA/Machinelearning/Bootstrap/ParEsti_VineBsk_median_",Sys.Date(),".csv",sep=""))

##### Natural parameters #####
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
B_2 <- as.data.frame(cbind(0,0))
names(B_2) <- c("Parameter", "Estimate")
B_2$Parameter <- as.character(B_2$Parameter)
p = 17 
for (i in 1:5) {
  B_2[p-16,2] <- par_not_zero[p,2]/(X_train_sd[i]*X_train_sd[i])
  B_2[p-16,1] <- par_not_zero[p,1]
  p = p+1
}
B_ <- as.data.frame(cbind(0,0))
names(B_) <- c("Parameter", "Estimate")
B_$Parameter <- as.character(B_$Parameter)

val <- par_not_zero[2,2]/X_train_sd[1]
val <- val - par_not_zero[17,2]*2*X_train_mean[1]/(X_train_sd[1]*X_train_sd[1])
for (j in 1:4) {
  val = val -par_not_zero[j+6,2]*X_train_mean[j+1]/(X_train_sd[1]*X_train_sd[j+1])
}
B_[1,2] <- val
B_[1,1] <- "LAI"

val <- par_not_zero[3,2]/X_train_sd[2]
val <- val - par_not_zero[18,2]*2*X_train_mean[2]/(X_train_sd[2]*X_train_sd[2])
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
par <- c(B_0[,2],B_[,2],B_int[,2],B_2[,2])
names(par) <- attributeNames

par2 <- par
par2[par2==0] <- NA
par2 <- par2[complete.cases(par2)]

print(xtable(as.data.frame(t(par2)),type="latex",digits = 3),
      file = paste("C:/Users/ellen/Documents/BA/Model presentation/Bsk_vine/ParEsti_BskVine_natural_zero_",Sys.Date(),".tex",sep=""))
write.csv2(as.data.frame(par),file = paste("C:/Users/ellen/Documents/BA/Model presentation/Bsk_vine/ParEsti_BskVine_natural_",Sys.Date(),".csv",sep=""))
print(xtable(as.data.frame(par), type = "latex"), 
      file = paste("C:/Users/ellen/Documents/BA/Model presentation/Bsk_vine/ParEsti_natural_BskVine_",Sys.Date(),".tex",sep=""))

df_par <- as.data.frame(par2)
df_par$names <- row.names(as.data.frame(df_par))
ggplot(data = df_par, aes(x = par2, y =names ))+
  geom_point(aes(color = names))+
  labs(title = "Parameter Estimates in Natural Domain" , 
       x = "Estimates",
       y = "Non-zero parameters")+
  theme(axis.text.x = element_text(colour = "grey20", size = 16),
        axis.text.y=element_text(colour = "grey20", size = 16),
        text = element_text(size = 16))+
  theme(legend.position = "none")

##### Performance of model #######
y_test <- y[c(fold_test$Fold10)];

X_test <- X[c(fold_test$Fold10), ];
X_test <- cbind(model.matrix(~.^2,as.data.frame(X_test[,-1])) , X_test[,-1]^2 )
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

title_ <- "Performance of Model on Vineyards in BSk-area"
png("C:/Users/ellen/Documents/BA/Model presentation/Bsk_vine/Pred_BskVine.png", width = 1000, height = 500)
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
png("C:/Users/ellen/Documents/BA/Model presentation/Bsk_vine/ResVsFit_BskVine.png", width = 1000, height = 500)
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
qqnorm(err, pch = 1, frame = FALSE)
qqline(err, col = "steelblue", lwd = 2)
library(car)
qqPlot(err)
err <- as.data.frame(err)
library(devtools)
library(qqplotr)
png("C:/Users/ellen/Documents/BA/Model presentation/Bsk_vine/QQ_BskVine.png", width = 1000, height = 500)
ggplot(data = err, mapping = aes(sample = V1)) +
  stat_qq_band() +
  stat_qq_line() +
  stat_qq_point(size=1.5) +
  labs(x = "Theoretical Quantiles", y = "Sample Quantiles", title = "Q-Q plot")+
  theme(axis.text.x = element_text(colour = "grey20", size = 20),
        axis.text.y=element_text(colour = "grey20", size = 20),
        text = element_text(size = 25))
dev.off()
################ Interpret interactions in model ################
X_mean <- apply(X,2, mean)
X_0.1 <- apply(X,2, quantile,0.1)
X_0.9 <- apply(X,2,quantile,0.9)
X_0.5 <- apply(X,2, quantile, 0.5)
#### Interactions p and e_star #####
p_seq <- seq(min(dat$p_year), max(dat$p_year), by = 0.1)
e_star_seq <- quantile(X[,6], c(0.01,0.1,0.5,0.9 ,0.99))
fun_pestar_0.1 <- function(x,e_star) {
  par[3]*x + par[6]*e_star +
    par[21]*e_star^2 +
    par[13]*x*e_star +
    sum(par[c(1,2,4,5)]*X_0.1[-c(3,6)])+
    sum(par[c(14,19)]*X_0.1[c(4,4)]*X_0.1[c(5,4)])
}
fun_pestar_0.5 <- function(x,e_star) {
  par[3]*x + par[6]*e_star +
    par[21]*(e_star^2) +
    par[13]*x*e_star +
    sum(par[c(1,2,4,5)]*X_0.5[-c(3,6)])+
    sum(par[c(14,19)]*X_0.5[c(4,4)]*X_0.5[c(5,4)])
}
fun_pestar_0.9 <- function(x,e_star) {
  par[3]*x + par[6]*e_star +
    par[21]*e_star^2 +
    par[13]*x*e_star +
    sum(par[c(1,2,4,5)]*X_0.9[-c(3,6)])+
    sum(par[c(14,19)]*X_0.9[c(4,4)]*X_0.9[c(5,4)])
}
m <- length(p_seq)
df_pestar <- as.data.frame(cbind(rep(p_seq,5),c(
                            fun_pestar_0.5(p_seq,e_star_seq[1]),
                            fun_pestar_0.5(p_seq,e_star_seq[2]),
                            fun_pestar_0.5(p_seq,e_star_seq[3]),
                            fun_pestar_0.5(p_seq,e_star_seq[4]),
                            fun_pestar_0.5(p_seq,e_star_seq[5]))))

df_pestar$`Fixed Value` <- c(rep("e*: 0.0030",m),rep("e*: 0.0284",m) ,rep("e*: 0.0991",m),rep("e*: 0.1267",m),rep("e*: 0.1513",m))
png("C:/Users/ellen/Documents/BA/Model presentation/Bsk_vine/BskVine_p_estar_.png", width = 1000, height = 500)
ggplot(df_pestar, aes(x = V1, y =V2)) + 
  geom_line(aes(color = `Fixed Value`), size = 1) +
  labs(x="Precipitation",
       y="Evapotranspiration",
       title = "Interaction between p and e*")+
  theme(axis.text.x = element_text(colour = "grey20", size = 24),
        axis.text.y=element_text(colour = "grey20", size = 24),
        text = element_text(size = 25))
dev.off()

#### Interactions Rn and e_act ####
Rn_seq <- seq(min(dat$Rn_year), max(dat$Rn_year), by = 1)
e_act_seq <- quantile(X[,5], c(0.01,0.1,0.5,0.9 ,0.99))
fun_Rne_0.5 <- function(x,e) {
  par[4]*x + par[5]*e +
    par[19]*(x^2) +
    par[14]*x*e +
    sum(par[c(1:3,6)]*X_0.5[-c(4,5)])+
    sum(par[c(13,21)]*X_0.5[c(3,6)]*X_0.5[c(6,6)])
}
m <- length(Rn_seq)
df_Rne <- as.data.frame(cbind(rep(Rn_seq,5),c(
  fun_Rne_0.5(Rn_seq,e_act_seq[1]),
  fun_Rne_0.5(Rn_seq,e_act_seq[2]),
  fun_Rne_0.5(Rn_seq,e_act_seq[3]),
  fun_Rne_0.5(Rn_seq,e_act_seq[4]),
  fun_Rne_0.5(Rn_seq,e_act_seq[5]))))
library(ggplot2)
df_Rne$Value <- c(rep("e: -0.147",m),rep("e: -0.124",m) ,rep("e: -0.054",m),rep("e: -0.013",m),rep("e: 0.019",m))
names = c("e: -0.147","e: -0.124","e: -0.054","e: -0.013","e: 0.019")
df_Rne$`Fixed Value` <- factor(df_Rne$Value, levels = names)

png("C:/Users/ellen/Documents/BA/Model presentation/Bsk_vine/BskVine_Rn_eact.png", width = 1000, height = 500)
ggplot(df_Rne, aes(x = V1, y =V2)) + 
  geom_line(aes(color = `Fixed Value`), size = 1) +
  labs(x="Net Radiation", 
       y="Evapotranspiration",
       title = "Interaction between Rn and e")+
  theme(axis.text.x = element_text(colour = "grey20", size = 24),
        axis.text.y=element_text(colour = "grey20", size = 24),
        text = element_text(size = 25))
dev.off()

###### Rn plot ####
Rn_seq <- seq(min(dat$Rn_year), max(dat$Rn_year), by = 1)
fun_Rn_0.1 <- function(x) {
  sum(X_0.1[-4]*par[c(1:3,5,6)])+
    sum(par[c(13,21)]*X_0.1[c(3,6)]*X_0.1[c(6,6)])+
    par[2]*x+
    par[c(14)]*x*X_0.1[6]+
    par[19]*x^2
    #knowing remaining interactions are zero
}
fun_Rn_0.5 <- function(x) {
  sum(X_0.5[-4]*par[c(1:3,5,6)])+
    sum(par[c(13,21)]*X_0.5[c(3,6)]*X_0.5[c(6,6)])+
    par[2]*x+
    par[c(14)]*x*X_0.1[6]+
    par[19]*x^2
  #knowing remaining interactions are zero
}
fun_Rn_0.9 <- function(x) {
  sum(X_0.9[-4]*par[c(1:3,5,6)])+
    sum(par[c(13,21)]*X_0.9[c(3,6)]*X_0.9[c(6,6)])+
    par[2]*x+
    par[c(14)]*x*X_0.1[6]+
    par[19]*x^2
  #knowing remaining interactions are zero
}
m <- length(Rn_seq)
df_Rn <- as.data.frame(cbind(rep(Rn_seq,3),c(
  fun_Rn_0.1(Rn_seq),
  fun_Rn_0.5(Rn_seq),
  fun_Rn_0.9(Rn_seq))))
df_Rn$`Fixed Value` <- c(rep("10th Q",m),rep("50th Q",m),rep("90th Q",m))
head(df_Rn)
png("C:/Users/ellen/Documents/BA/Model presentation/Bsk_vine/BskVine_Rn_Q.png", width = 1000, height = 500)
ggplot(df_Rn,aes(x=V1,y=V2))+
  geom_line(aes(color = `Fixed Value`), size = 1)+
  labs(x="Net Radiation",
       y="Evapotranspiration",
       title = "Function of Rn with Fixed Values for Remaining Parameters")+
  theme(axis.text.x = element_text(colour = "grey20", size = 24),
        axis.text.y=element_text(colour = "grey20", size = 24),
        text = element_text(size = 25))
dev.off()
##### LAI #####
LAI_seq <- seq(min(dat$LAI_year), max(dat$LAI_year), by = 0.5)
fun_LAI_0.1 <- function(x) {
  sum(X_0.1[-2]*par[c(1,3:6)])+
    sum(par[c(13,14,19,21)]*X_0.1[c(3,4,4,6)]*X_0.1[c(6,5,4,6)])+
    par[2]*x + par[17]*x^2
  #knowing remaining interactions are zero
}
fun_LAI_0.5 <- function(x) {
  sum(X_0.5[-2]*par[c(1,3:6)])+
    sum(par[c(13,14,19,21)]*X_0.5[c(3,4,4,6)]*X_0.5[c(6,5,4,6)])+
    par[2]*x + par[17]*x^2
  #knowing remaining interactions are zero
}
fun_LAI_0.9 <- function(x) {
  sum(X_0.9[-2]*par[c(1,3:6)])+
    sum(par[c(13,14,19,21)]*X_0.9[c(3,4,4,6)]*X_0.9[c(6,5,4,6)])+
    par[2]*x + par[17]*x^2
  #knowing remaining interactions are zero
}
m <- length(LAI_seq)
df_LAI <- as.data.frame(cbind(rep(LAI_seq,3),c(
  fun_LAI_0.1(LAI_seq),
  fun_LAI_0.5(LAI_seq),
  fun_LAI_0.9(LAI_seq))))
df_LAI$`Fixed Value` <-  c(rep("10th Q",m),rep("50th Q",m),rep("90th Q",m)) 
png("C:/Users/ellen/Documents/BA/Model presentation/Bsk_vine/BskVine_LAI_Q.png", width = 1000, height = 500)
ggplot(df_LAI,aes(x=V1,y=V2))+
  geom_line(aes(color = `Fixed Value`),size = 1)+
  labs(x="Leaf Area Index",
       y="Evapotranspiration",
       title = "Function of LAI with Fixed Values for Remaining Parameters")+
  theme(axis.text.x = element_text(colour = "grey20", size =24),
        axis.text.y=element_text(colour = "grey20", size = 24),
        text = element_text(size = 25))
dev.off()

##### p ########
p_seq <- seq(min(dat$p_year), max(dat$p_year), by = 0.1)
fun_p_0.1 <- function(x,e_star) {
  par[3]*x + par[18]*(x^2)+
    sum(par[c(1,2,4:6)]*X_0.1[-c(3)])+
    sum(par[c(7,11:13)]*X_0.1[c(2,4:6)])*x+
    sum(par[c(8:10,14:17,19:21)]*X_0.1[c(2,2,2,4,4,5,2,4,5,6)]*X_0.1[c(4,5,6,5,6,6,2,4,5,6)])
}
fun_p_0.5 <- function(x,e_star) {
  par[3]*x + par[18]*(x^2)+
    sum(par[c(1,2,4:6)]*X_0.5[-c(3)])+
    sum(par[c(7,11:13)]*X_0.5[c(2,4:6)])*x+
    sum(par[c(8:10,14:17,19:21)]*X_0.5[c(2,2,2,4,4,5,2,4,5,6)]*X_0.5[c(4,5,6,5,6,6,2,4,5,6)])
}
fun_p_0.9 <- function(x,e_star) {
  par[3]*x + par[18]*(x^2)+
    sum(par[c(1,2,4:6)]*X_0.9[-c(3)])+
    sum(par[c(7,11:13)]*X_0.9[c(2,4:6)])*x+
    sum(par[c(8:10,14:17,19:21)]*X_0.9[c(2,2,2,4,4,5,2,4,5,6)]*X_0.9[c(4,5,6,5,6,6,2,4,5,6)])
}
m <- length(p_seq)
df_p <- as.data.frame(cbind(rep(p_seq,3),c(
  fun_p_0.1(p_seq),
  fun_p_0.5(p_seq),
  fun_p_0.9(p_seq))))
df_p$`Fixed Value` <-  c(rep("10th Q",m),rep("50th Q",m),rep("90th Q",m)) 
png("C:/Users/ellen/Documents/BA/Model presentation/Bsk_vine/BskVine_p_Q.png", width = 1000, height = 500)
ggplot(df_p,aes(x=V1,y=V2))+
  geom_line(aes(color = `Fixed Value`),size = 1)+
  labs(x="Precipitation",
       y="Evapotranspiration",
       title = "Function of p with Fixed Values for Remaining Parameters")+
  theme(axis.text.x = element_text(colour = "grey20", size =24),
        axis.text.y=element_text(colour = "grey20", size = 24),
        text = element_text(size = 25))
dev.off()

####### e ########
e_seq <- seq(min(dat$e_act_year), max(dat$e_act_year), by = 0.01)
fun_e_0.1 <- function(x) {
  par[5]*x + par[20]*(x^2)+
    sum(par[c(1:4,6)]*X_0.1[-c(5)])+
    sum(par[c(9,12,14,16)]*X_0.1[c(2:4,6)])*x+
    sum(par[c(7,8,10,11,13,15,17:19,21)]*X_0.1[c(2,2,2,3,3,4,2,3,4,6)]*X_0.1[c(3,4,6,4,6,6,2,3,4,6)])
}
fun_e_0.5 <- function(x) {
  par[5]*x + par[20]*(x^2)+
    sum(par[c(1:4,6)]*X_0.5[-c(5)])+
    sum(par[c(9,12,14,16)]*X_0.5[c(2:4,6)])*x+
    sum(par[c(7,8,10,11,13,15,17:19,21)]*X_0.5[c(2,2,2,3,3,4,2,3,4,6)]*X_0.5[c(3,4,6,4,6,6,2,3,4,6)])
}
fun_e_0.9 <- function(x) {
  par[5]*x + par[20]*(x^2)+
    sum(par[c(1:4,6)]*X_0.9[-c(5)])+
    sum(par[c(9,12,14,16)]*X_0.9[c(2:4,6)])*x+
    sum(par[c(7,8,10,11,13,15,17:19,21)]*X_0.9[c(2,2,2,3,3,4,2,3,4,6)]*X_0.9[c(3,4,6,4,6,6,2,3,4,6)])
}
m <- length(e_seq)
df_e <- as.data.frame(cbind(rep(e_seq,3),c(
  fun_e_0.1(e_seq),
  fun_e_0.5(e_seq),
  fun_e_0.9(e_seq))))
df_e$`Fixed Value` <-  c(rep("10th Q",m),rep("50th Q",m),rep("90th Q",m)) 
png("C:/Users/ellen/Documents/BA/Model presentation/Bsk_vine/BskVine_e_Q.png", width = 1000, height = 500)
ggplot(df_e,aes(x=V1,y=V2))+
  geom_line(aes(color = `Fixed Value`),size = 1)+
  labs(x="Actual Water Vapor",
       y="Evaeotranseiration",
       title = "Function of e with Fixed Values for Remaining Parameters")+
  theme(axis.text.x = element_text(colour = "grey20", size =24),
        axis.text.y=element_text(colour = "grey20", size = 24),
        text = element_text(size = 25))
dev.off()

####### e* ########
e_star_seq <- seq(min(dat$e_star_year), max(dat$e_star_year), by = 0.01)
fun_estar_0.1 <- function(x) {
  par[6]*x + par[21]*(x^2)+
    sum(par[c(1:5)]*X_0.1[-c(6)])+
    sum(par[c(10,13,15,16)]*X_0.1[c(2:5)])*x+
    sum(par[c(7:9,11,12,14,17:20)]*X_0.1[c(2,2,2,3,3,4,2,3,4,5)]*X_0.1[c(3,4,5,4,5,5,2,3,4,5)])
}
fun_estar_0.5 <- function(x) {
  par[6]*x + par[21]*(x^2)+
    sum(par[c(1:5)]*X_0.5[-c(6)])+
    sum(par[c(10,13,15,16)]*X_0.5[c(2:5)])*x+
    sum(par[c(7:9,11,12,14,17:20)]*X_0.5[c(2,2,2,3,3,4,2,3,4,5)]*X_0.5[c(3,4,5,4,5,5,2,3,4,5)])
}
fun_estar_0.9 <- function(x) {
  par[6]*x + par[21]*(x^2)+
    sum(par[c(1:5)]*X_0.9[-c(6)])+
    sum(par[c(10,13,15,16)]*X_0.9[c(2:5)])*x+
    sum(par[c(7:9,11,12,14,17:20)]*X_0.9[c(2,2,2,3,3,4,2,3,4,5)]*X_0.9[c(3,4,5,4,5,5,2,3,4,5)])
}
m <- length(e_star_seq)
df_estar <- as.data.frame(cbind(rep(e_star_seq,3),c(
  fun_estar_0.1(e_star_seq),
  fun_estar_0.5(e_star_seq),
  fun_estar_0.9(e_star_seq))))
df_estar$`Fixed Value` <-  c(rep("10th Q",m),rep("50th Q",m),rep("90th Q",m)) 
png("C:/Users/ellen/Documents/BA/Model presentation/Bsk_vine/BskVine_estar_Q.png", width = 1000, height = 500)
ggplot(df_estar,aes(x=V1,y=V2))+
  geom_line(aes(color = `Fixed Value`),size = 1)+
  labs(x="Saturated Water Vapor",
       y="Evaeotranseiration",
       title = "Function of e* with Fixed Values for Remaining Parameters")+
  theme(axis.text.x = element_text(colour = "grey20", size =24),
        axis.text.y=element_text(colour = "grey20", size = 24),
        text = element_text(size = 25))
dev.off()
