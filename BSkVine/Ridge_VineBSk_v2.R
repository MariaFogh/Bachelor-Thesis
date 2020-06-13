## Ridge-regression
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
  ridge_cv <- cv.glmnet(X_train, y_train, alpha = 0, lambda = lambdas_to_try,
                        standardize = TRUE, nfolds = 10)
  plot(ridge_cv)
  plot(ridge_cv$glmnet.fit, "lambda")
  # Best cross-validated lambda
  lambda_cv[k] <- ridge_cv$lambda.1se
  coef[,k] <- coef(ridge_cv, s = ridge_cv$lambda.1se)[,1]
  
  # evaluate training and test error performance for optimal selected value of lambda
  Error_train_rlr[k] = sum( (y_train - cbind(1,X_train) %*% coef[,k])^2 )
  Error_test_rlr[k] = sum( (y_test - cbind(1,X_test) %*% coef[,k])^2 )
}
png(paste(file = "~/Documents/DTU/6. Semester/Bachelorprojekt/Data_Ellen_Maria/Cross-validation/plots/PLOT_Ridge_BSkVine.png"), width = 500, height = 500)
coefplot <- coefplot(ridge_cv, lambda = 'lambda.1se', pointSize = 4)
coefplot +  theme (axis.text.x  = element_text(size=16)) +
  theme(axis.text.y = element_text(size=16)) + theme(title = element_text(size=16))
dev.off()

#### Selecting optimal model ###
# choosing the lambda that occurs the most
lambda_mode = as.numeric(names(which.max(table(lambda_cv))))