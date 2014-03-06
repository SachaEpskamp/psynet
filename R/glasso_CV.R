
wi2net <- function(x)
{
  x <- -cov2cor(x)
  diag(x) <- 0
  return(x)
}

# Prediction function:
predictNet <- function(net, data)
{
  # center data:
  data <- scale(data)
  
  # Remove diagonal:
  diag(net) <- 0
  
  # Number of observations:
  n <- nrow(data)
  k <- ncol(data)
  
  # Check:
  if (k != ncol(net))
  {
    stop("data does not have same number of variables as network")
  }
  
  # predicted values:
  Res <- matrix(0,n,k)
  
  # Start loop:
  for (i in seq_len(n))
  {
    for (j in seq_len(k))
    {
      Res[i,j] <-  net[,j] %*% data[i,]
    }
  }
  
  # Return:
  return(Res)
}



# Estimation function:
CVglasso <- function(data, K = 10, nonparanormal = TRUE, cost = rmspe, ...)
{
  # Nonparanormal:
  if (nonparanormal) data <- huge.npn(data, verbose = FALSE)
  
  # standardize:
  data <- scale(data)
  
  # Parameters:
  Np <- nrow(data)
  Ni <- ncol(data)
  
  # Create folds:
  Folds <- cvFolds(Np, K)

  ## Prediction cost function (INNER FUNCTION):
  predictionCost <- function(lambda)
  {
    # Full prediction matrix:
    Prediction <- matrix(,Np,Ni)
    
    # For all blocks:
    for (k in 1:K)
    {
      # Which in block:
      inBlock <- sort(Folds$subsets[Folds$which==k,1])
      
      # Covariance of folds without fold k:
      CovMat <- cov(data[-inBlock,], use = "pairwise.complete.obs")
      
      # Rune glasso:
      capture.output( Res <- glasso(CovMat, lambda, ...) )
      
      # Predict in block k:
      Prediction[inBlock,] <- predictNet(wi2net(Res$wi), data[inBlock,])
    }
    
    # Compute cost:
    cost(data, Prediction)
  }  
  
  # Find optimal lambda:
#   optimLambda <- optimizationFunction(predictionCost)  
  optimLambda <- optimize(predictionCost, c(0, 1))$minimum
  
  message(paste("Optimal lambda set to:",round(optimLambda,3)))
  
  # Compute and return glasso under optimal lambda:
  CovMat <- cov(data, use = "pairwise.complete.obs")
  Res <- glasso(CovMat, optimLambda, ...)
  net <- -cov2cor(Res$wi)
  diag(net) <- 1
  net <- as.matrix(forceSymmetric(net))
  return(net)
}
