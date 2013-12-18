## Correlations:
graph_cor <- function(
  x, # Data or cormat
  scale, 
  corMat, #Logical indicating if x is correlation matrix
  title = TRUE,
  ... # Args sent to qgraph
)
{
  if (missing(corMat))
  {
    corMat <- nrow(x)==ncol(x) && all(diag(x)==1) && isSymmetric(x)
  }
  
  if (!corMat)
  {
    x <- getCors(x, scale)
  }
  
  Res <- list(
    graph = x,
    output = NULL)
  
  Res$qgraph <- qgraph(x, ...)
  
  if (title) addTitle("Correlations")
  
  return(Res)
}


## Partial correlations:
graph_pcor <- function(
  x, # Data or cormat
  scale, 
  corMat, #Logical indicating if x is correlation matrix
  title = TRUE,
  ... # Args sent to qgraph
)
{
  if (missing(corMat))
  {
    corMat <- nrow(x)==ncol(x) && all(diag(x)==1) && isSymmetric(x)
  }
  
  if (!corMat)
  {
    x <- getCors(x, scale)
  }
  
  x <- cor2pcor(x)
  
  Res <- list(
    graph = x,
    output = NULL)
  
  Res$qgraph <- qgraph(x, ...)
  
  if (title) addTitle("Partial Correlations")
  
  return(Res)
}

## parcor: Lasso
graph_alpcor <- function(
  x, # Data or cormat
  scale, 
  title = TRUE,
  ... # Args sent to qgraph
)
{
  if (missing(scale)) scale <- autoScale(x)
  
  if (scale != "continuous")
  {
    warning("Data treated as continous for adaptive LASSO partial correlations")
  }
  
  x <- round(adalasso.net(x)$pcor.adalasso,10)
  
  Res <- list(
    graph = x,
    output = NULL)
  
  Res$qgraph <- qgraph(x, ...)
  
  if (title) addTitle("Partial Correlations (adaptive LASSO)")
  
  return(Res)
}

# Partial least squares pcor:
graph_plspcor <- function(
  x, # Data or cormat
  scale, 
  title = TRUE,
  ... # Args sent to qgraph
)
{
  if (missing(scale)) scale <- autoScale(x)
  
  if (scale != "continuous")
  {
    warning("Data treated as continous for adaptive LASSO partial correlations")
  }
  
  x <- round(pls.net(x)$pcor,10)
  
  Res <- list(
    graph = x,
    output = NULL)
  
  Res$qgraph <- qgraph(x, ...)
  
  if (title) addTitle("Partial Correlations (partial least squares)")
  
  return(Res)
}

# pcalg
graph_pc <- function(
  x, # Data
  scale, 
  corMat,
  title = TRUE,
  pcAlpha = 0.05,
  n, # Number of observations
  ... # Args sent to qgraph
)
{
  if (missing(corMat))
  {
    corMat <- nrow(x)==ncol(x) && all(diag(x)==1) && isSymmetric(x)
  }
  
  if (missing(n))
  {
    if (!corMat)
    {
      n <- nrow(x)
    } else stop("n not supplied")
  }
  
  if (!corMat)
  {
    cors <- getCors(x, scale)
  } else cors <- x
  
  pc <- pc(
    suffStat = list(C = cors, n = n),
    indepTest = gaussCItest, 
    p = ncol(x), 
    alpha = pcAlpha)
  
  Res <- list(
    graph = NULL,
    output = pc)
  
  Res$qgraph <- qgraph(pc, ...)
  
  if (title) addTitle("PC-algorithm")
  
  return(Res)
}

# pcalg (skeleton)
graph_pcskel <- function(
  x, # Data
  scale, 
  corMat,
  title = TRUE,
  pcAlpha = 0.05,
  n, # Number of observations
  ... # Args sent to qgraph
)
{
  if (missing(corMat))
  {
    corMat <- nrow(x)==ncol(x) && all(diag(x)==1) && isSymmetric(x)
  }
  
  if (missing(n))
  {
    if (!corMat)
    {
      n <- nrow(x)
    } else stop("n not supplied")
  }
  
  if (!corMat)
  {
    cors <- getCors(x, scale)
  } else cors <- x
  
  pc <- skeleton(
    suffStat = list(C = cors, n = n),
    indepTest = gaussCItest, 
    p = ncol(x), 
    alpha = pcAlpha)
  
  Res <- list(
    graph = NULL,
    output = pc)
  
  Res$qgraph <- qgraph(pc, ...)
  
  if (title) addTitle("PC-algorithm (skeleton)")
  
  return(Res)
}

# BDgraph: best
graph_BDbest <- function(
  BDobject,
  title = TRUE,
  ... # Args sent to qgraph
)
{
 Adj <- select(BDobject)
  
  Res <- list(
    graph = Adj,
    output = BDobject)
  
  Res$qgraph <- qgraph(Adj, ...)
  
  if (title) addTitle("BDgraph (graph with highest probability)")
  
  return(Res)
}

# BDgraph: phat
graph_BDpost <- function(
  BDobject,
  title = TRUE,
  ... # Args sent to qgraph
)
{
  Adj <- phat(BDobject)
  
  Res <- list(
    graph = Adj,
    output = BDobject)
  
  Res$qgraph <- qgraph(BDobject, BDgraph = "phat", BDtitles = FALSE, ...)
  
  if (title) addTitle("BDgraph (Posterior probabilities)")
  
  return(Res)
}

# BDgraph: Khat
graph_BDavg <- function(
  BDobject,
  title = TRUE,
  ... # Args sent to qgraph
)
{
  Adj <- BDobject$Khat
  diag(Adj) <- -1*diag(Adj)
  Adj <-  - Adj / sqrt(diag(Adj)%o%diag(Adj))
  
  Res <- list(
    graph = Adj,
    output = BDobject)
  
  Res$qgraph <- qgraph(Adj, ...)
  
  if (title) addTitle("BDgraph (Average posterior partial correlations)")
  
  return(Res)
}
