## Correlations:
graph_cor <- function(
  x, # Data or cormat
  scale, 
  corMat, #Logical indicating if x is correlation matrix
  title = TRUE,
  verbose = FALSE,
  ... # Args sent to qgraph
)
{
  if (verbose) message("psynet: Constructing correlation graph")
  
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
  verbose = FALSE,
  ... # Args sent to qgraph
)
{
  if (verbose) message("psynet: Constructing partial correlation graph")
  
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
  verbose = FALSE,
  ... # Args sent to qgraph
)
{
  if (verbose) message("psynet: Constructing correlation graph (adaptive lasso)")
  
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
  verbose = FALSE,
  ... # Args sent to qgraph
)
{
  if (verbose) message("psynet: Constructing correlation graph (PLS)")
  
  if (missing(scale)) scale <- autoScale(x)
  
  if (scale != "continuous")
  {
    warning("Data treated as continous for PLS partial correlations")
  }
  
  x <- round(pls.net(as.matrix(x))$pcor,10)
  
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
  verbose = FALSE,
  ... # Args sent to qgraph
)
{
  if (verbose) message("psynet: Constructing pc-algorithm graph")
  
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
  verbose = FALSE,
  ... # Args sent to qgraph
)
{
  if (verbose) message("psynet: Constructing pc-algorithm graph (skeleton)")
  
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
  verbose = FALSE,
  ... # Args sent to qgraph
)
{
  if (verbose) message("psynet: Constructing best posterior BDgraph graph")
  
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
  verbose = FALSE,
  ... # Args sent to qgraph
)
{
  if (verbose) message("psynet: Constructing BDgraph posterior probability graph")
  
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
  verbose = FALSE,
  ... # Args sent to qgraph
)
{
  if (verbose) message("psynet: Constructing BDgraph average partial correlation graph")
  
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


# BNlearn:
graph_bnlearn <- function(
  x, # Data
  scale, 
  title = TRUE,
  bnlearnFun,
  bnlearnArgs = list(),
  verbose = FALSE,
  ... # Args sent to qgraph
)
{
  if (verbose) message(paste0("psynet: Constructing bnlearn graph (",bnlearnFun,")"))
  
  stopifnot(!missing(bnlearnFun))
  
  x <- as.data.frame(x)
  
  if (missing(scale)) scale <- autoScale(x)
  
  if (scale=="dichotomous")
  {
    for (i in 1:ncol(x)) x[,i] <- factor(x[,i])
  }
  
  if (scale=="ordinal")
  {
    for (i in 1:ncol(x)) x[,i] <- ordered(x[,i])
  }
  
  bn <- do.call(get(bnlearnFun), c(list(x),bnlearnArgs))
    
  Res <- list(
    graph = NULL,
    output = bn)
  
  Res$qgraph <- qgraph(bn, ...)
  
  if (title) addTitle(paste0("bnlearn (",bnlearnFun,")"))
  
  return(Res)
}



# BNlearn boot:
graph_bnboot <- function(
  x, # Data
  scale, 
  title = TRUE,
  bnlearnFun, # character
  bnlearnArgs = list(),
  bnbootArgs = list(),
  verbose = FALSE,
  ... # Args sent to qgraph
)
{
  if (verbose) message(paste0("psynet: Constructing bnlearn bootstrapped strength graph (",bnlearnFun,")"))
  
  stopifnot(!missing(bnlearnFun))
  
  x <- as.data.frame(x)
  
  if (missing(scale)) scale <- autoScale(x)
  
  if (scale=="dichotomous")
  {
    for (i in 1:ncol(x)) x[,i] <- factor(x[,i])
  }
  
  if (scale=="ordinal")
  {
    for (i in 1:ncol(x)) x[,i] <- ordered(x[,i])
  }
  
  bn <- do.call(boot.strength, c(list(data=x,algorithm = bnlearnFun, algorithm.args=bnlearnArgs),bnbootArgs))
  
  Res <- list(
    graph = NULL,
    output = bn)
  
  Res$qgraph <- qgraph(bn, probabilityEdges = TRUE, ...)
  
  if (title) addTitle(paste0("bnlearn bootstrapped posterior probabilities (",bnlearnFun,")"))
  
  return(Res)
}