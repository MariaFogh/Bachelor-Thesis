
random_uni <- function(matrix,size_sample, n_sample){
  set.seed(5)
  size <- size_sample # sample size 
  N <- n_sample #number of samples
  UniSample <- matrix(0L,ncol = N, nrow = size) # for sample values
  comp <- vector(length = N) # to compare sample and if they are alike
  coor_matrix <- matrix(0L,ncol = N*3, nrow = size) # coordinates for each sample
  
  for (i in c(1:N)) {
    set.seed(i)
    sample1 <- sample(1:length(matrix), size, replace=FALSE)
    UniSample[,i] = sample1
    compare <- sum(colSums(UniSample==rep(UniSample[,i], length = length(UniSample[,1]))) > 0)
    comp[i] <- compare
  }
  
  # to take coordinates from map 
  n <- dim(matrix)[1] # number of rows
  m <- dim(matrix)[2] # number of columns 
  # matrix to vector
  vec <- as.vector(matrix)
  col_idx <- rep(c(1:m), times = n)
  row_idx <- numeric()
  for (i in 1:n) {
    seq <- rep(c(i),times = m)
    row_idx <- cbind(row_idx, seq)
  }
  row_idx <- as.vector(row_idx)
  
  # Value, row number and column number as matrix 
  VRC <- matrix(c(vec,row_idx,col_idx), ncol =3, dimnames = list(NULL, c("val","rowNr","colNr")))
  
  #
  #VRC[UniSample[,1],]
  #coor_matrix[,]
  #
  for (j in c(1:N)) {
    coor_matrix[,(j*3-2):(j*3)] <- VRC[UniSample[,j],]
  }
  return_list <- list("coor_matrix" = coor_matrix, "comp"=comp, "Uni_sample"=UniSample)
  return(return_list)
}