## Correlations:
graph_cor <- function(
  x, # Data or cormat
  scale, 
  corMat, #Logical indicating if x is correlation matrix
  title = FALSE, 
  citation = FALSE,
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
    graph = as.matrix(forceSymmetric(x)),
    output = NULL,
    method = "cor")
  
  
  if (title) 
  {
    ann <- "Correlations"
  } else ann <- NULL

  if (citation)
  {
    cit <- CitationExpr("qgraph")
  } else cit <- NULL
  
  Res$qgraph <- qgraph(x, title = ann, postExpression = cit, ...)
  class(Res) <- "psynetGraph"
  return(Res)
}


## Partial correlations:
graph_pcor <- function(
  x, # Data or cormat
  scale, 
  corMat, #Logical indicating if x is correlation matrix
  unweighted = FALSE, # Should the graph be made unweighted using GeneNet?
  title = FALSE, citation = FALSE,
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
  
  g <-  as.matrix(forceSymmetric(cor2pcor(x)))
  
  ### UNWEIGHTALIZE ###
  if (unweighted)
  {
    net <- extract.network(ggm.test.edges(g, plot = FALSE))
    g <- 1*as.matrix(sparseMatrix(net$node1,net$node2, dims=c(ncol(x),ncol(x))))
    g <- g + t(g)
  }

  colnames(g) <- rownames(g) <- colnames(x)

  
  Res <- list(
    graph = g,
    output = NULL,
    method = "pcor")
  
  
  if (title) 
  {
      ann <- "Partial Correlations (unweighted)"
  } else ann <- NULL

  if (citation)
  {
    cit <- CitationExpr("corpcor")
  } else cit <- NULL
  
  
  Res$qgraph <- qgraph(g, title = ann, postExpression = cit, ...)
  class(Res) <- "psynetGraph"
  return(Res)
}




## pcor.shrink:
graph_pcor.shrink <- function(
  x, # Data or cormat
  scale, 
  unweighted = FALSE,
  title = FALSE, citation = FALSE,
  verbose = FALSE,
  ... # Args sent to qgraph
)
{
  if (verbose) message("psynet: Constructing  shrinkage partial correlation graph")
  
  if (missing(scale)) scale <- autoScale(x)
  
  if (scale != "continuous")
  {
    warning("Data treated as continous for adaptive LASSO partial correlations")
  }
  
  Out <- corpcor:::pcor.shrink(x)
  g <- Out
  class(g) <- "matrix"
  colnames(g) <- rownames(g) <- colnames(x)
  g <- as.matrix(forceSymmetric(g))
  
  ### UNWEIGHTALIZE ###
  if (unweighted)
  {
    net <- extract.network(ggm.test.edges(g, plot = FALSE))
    g <- 1*as.matrix(sparseMatrix(net$node1,net$node2, dims=c(ncol(x),ncol(x))))
    g <- g + t(g)
  }
  
  Res <- list(
    graph = g,
    output = Out,
    method = "pcor")
  
  
  if (title) 
  {
    ann <- "Partial Correlations (shrinkage)"
  } else ann <- NULL
  
  if (citation)
  {
    cit <- CitationExpr("corpcor")
  } else cit <- NULL
  
  Res$qgraph <- qgraph(g, title = ann, postExpression = cit, ...)
  class(Res) <- "psynetGraph"
  return(Res)
}

## parcor: Lasso
graph_alpcor <- function(
  x, # Data or cormat
  scale, 
  title = FALSE, citation = FALSE,
  verbose = FALSE,
  unweighted = FALSE,
  ... # Args sent to qgraph
)
{
  if (verbose) message("psynet: Constructing correlation graph (adaptive lasso)")
  
  if (missing(scale)) scale <- autoScale(x)
  
  if (scale != "continuous")
  {
    warning("Data treated as continous for adaptive LASSO partial correlations")
  }

  Out <- adalasso.net(x)
  g <- round(Out$pcor.adalasso,14)
  g <- as.matrix(forceSymmetric(g))
  
  ### UNWEIGHTALIZE ###
  if (unweighted)
  {
    g <- 1*(g!=0)
  }
  
  colnames(g) <- rownames(g) <- colnames(x)
  
  Res <- list(
    graph = g,
    output = Out,
    method = "alpcor")

  if (title) 
  {
    ann <- "Partial Correlations (adaptive LASSO)"
  } else ann <- NULL

  if (citation)
  {
    cit <- CitationExpr("parcor")
  } else cit <- NULL
  
  
  Res$qgraph <- qgraph(g, title = ann, postExpression = cit, ...)
  class(Res) <- "psynetGraph"
  return(Res)
}

# Partial least squares pcor:
graph_plspcor <- function(
  x, # Data or cormat
  scale, 
  unweighted = FALSE,
  title = FALSE, citation = FALSE,
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
  
  Out <- pls.net(x)
  g <- round(Out$pcor,14)
  g <- as.matrix(forceSymmetric(g))
  
  ### UNWEIGHTALIZE ###
  if (unweighted)
  {
    net <- extract.network(ggm.test.edges(g, plot = FALSE))
    g <- 1*as.matrix(sparseMatrix(net$node1,net$node2, dims=c(ncol(x),ncol(x))))
    g <- g + t(g)
  }
  
  colnames(g) <- rownames(g) <- colnames(x)
  
  Res <- list(
    graph = g,
    output = Out,
    method = "plspcor")
  

  if (title) 
  {
    ann <- "Partial Correlations (partial least squares)"
  } else ann <- NULL

  if (citation)
  {
    cit <- CitationExpr("parcor")
  } else cit <- NULL
  
  Res$qgraph <- qgraph(g, title = ann, postExpression = cit, ...)
  class(Res) <- "psynetGraph"
  return(Res)
}

# pcalg
graph_pc <- function(
  x, # Data
  scale, 
  corMat, # correlat matr
  title = FALSE, citation = FALSE,
  pcAlpha = 0.05,
  n, # Number of observations
  verbose = FALSE,
  pcNoDicho = FALSE, # Base pc on tetra?
  skeleton = FALSE,
  adaptDF = TRUE,
  ... # Args sent to qgraph
)
{
  if (verbose) message("psynet: Constructing pc-algorithm graph")
  
  if (missing(n))
  {
    if (!missing(x))
    {
      n <- nrow(x)
    } else stop("n not supplied")
  }
  
  if (missing(scale)) 
  {
   if (missing(x)) stop("'x' or 'scale' needs to be assigned")
    
    scale <- autoScale(x)
    
  }
  
  if (scale == "dichotomous" &  !pcNoDicho)
  { 
    if (missing(x)) stop("Data needed for binary pcalg")
    
    suffStat = list(dm = x, adaptDF = adaptDF)
    indepTest = binCItest
  } else
  {
    if (missing(corMat))
    {
      corMat <- getCors(x, scale)
    } 
    suffStat = list(C = corMat, n = n)
    indepTest = gaussCItest
    
  }
  
  if (skeleton)
  {
    pc <- skeleton(
      suffStat = suffStat,
      indepTest = indepTest,
      p = ncol(x), 
      alpha = pcAlpha)
    
  } else 
  {
    pc <- pc(
      suffStat = suffStat,
      indepTest = indepTest,
      p = ncol(x), 
      alpha = pcAlpha)
    
  }
  
  Res <- list(
    graph = NULL,
    output = pc,
    method = ifelse(skeleton,"pcskel","pc"))

  
  if (title) 
  {
    if (skeleton)
    {
      ann <- "PC-algorithm (skeleton)"
    } else ann <- "PC-algorithm"
  } else ann <- NULL


  if (citation)
  {
    cit <- CitationExpr("pcalg")
  } else cit <- NULL
  
  Res$qgraph <- qgraph(pc, title = ann, postExpression = cit, ...)
  
  Res$graph <- getWmat(Res$qgraph)
  colnames(Res$graph) <- rownames(Res$graph) <- colnames(x)
  
  class(Res) <- "psynetGraph"
  return(Res)
}
# 
# # pcalg (skeleton)
# graph_pcskel <- function(
#   x, # Data
#   scale, 
#   corMat,
#   title = FALSE, citation = FALSE,
#   pcAlpha = 0.05,
#   n, # Number of observations
#   verbose = FALSE,
#   ... # Args sent to qgraph
# )
# {
#   if (verbose) message("psynet: Constructing pc-algorithm graph (skeleton)")
#   
#   if (missing(corMat))
#   {
#     corMat <- nrow(x)==ncol(x) && all(diag(x)==1) && isSymmetric(x)
#   }
#   
#   if (missing(n))
#   {
#     if (!corMat)
#     {
#       n <- nrow(x)
#     } else stop("n not supplied")
#   }
#   
#   if (!corMat)
#   {
#     cors <- getCors(x, scale)
#   } else cors <- x
#   
#   pc <- skeleton(
#     suffStat = list(C = cors, n = n),
#     indepTest = gaussCItest, 
#     p = ncol(x), 
#     alpha = pcAlpha)
#   
#   Res <- list(
#     graph = NULL,
#     output = pc)
#   
#   Res$qgraph <- qgraph(pc, ...)
#   
#   if (title) addTitle("PC-algorithm (skeleton)")
#   
#   return(Res)
# }

# BDgraph: best
graph_BDbest <- function(
  BDobject,
  title = FALSE, citation = FALSE,
  verbose = FALSE,
  ... # Args sent to qgraph
)
{
  if (verbose) message("psynet: Constructing best posterior BDgraph graph")
  
 Adj <- select(BDobject)
  
  Res <- list(
    graph = Adj,
    output = BDobject,
    method = "BDbest")
  
  
  
  if (title) 
  {
    ann <- "BDgraph (graph with highest probability)"
  } else ann <- NULL
  
  if (citation)
  {
    cit <- CitationExpr("BDgraph")
  } else cit <- NULL
  
  Res$qgraph <- qgraph(Adj, title = ann, postExpression = cit, ...)
 class(Res) <- "psynetGraph"
  return(Res)
}

# BDgraph: phat
graph_BDpost <- function(
  BDobject,
  title = FALSE, citation = FALSE,
  verbose = FALSE,
  ... # Args sent to qgraph
)
{
  if (verbose) message("psynet: Constructing BDgraph posterior probability graph")
  
  Adj <- phat(BDobject)
  
  Res <- list(
    graph = Adj,
    output = BDobject,
    method = "BDpost")

  if (title) 
  {
    ann <- "BDgraph (Posterior probabilities)"
  } else ann <- NULL

  
  if (citation)
  {
    cit <- CitationExpr("BDgraph")
  } else cit <- NULL
  
  Res$qgraph <- qgraph(BDobject, BDgraph = "phat", BDtitles = FALSE, title = ann, postExpression = cit, ...)
  class(Res) <- "psynetGraph"
  return(Res)
}

# BDgraph: Khat
graph_BDavg <- function(
  BDobject,
  title = FALSE, citation = FALSE,
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
    output = BDobject,
    method = "BDavg")
  
  if (title) 
  {
    ann <- "BDgraph (Average posterior partial correlations)"
  } else ann <- NULL
  
  
  if (citation)
  {
    cit <- CitationExpr("BDgraph")
  } else cit <- NULL
  
  Res$qgraph <- qgraph(Adj, title = ann, postExpression = cit, ...)
  class(Res) <- "psynetGraph"
  return(Res)
}


# BNlearn:
graph_bnlearn <- function(
  x, # Data
  scale, 
  title = FALSE, citation = FALSE,
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
    output = bn,
    method = paste0("bn.",bnlearnFun))
  
  if (title) 
  {
    ann <- paste0("bnlearn (",bnlearnFun,")")
  } else ann <- NULL
  
  
  if (citation)
  {
    cit <- CitationExpr(ref = "  Radhakrishnan Nagarajan, Marco Scutari, Sophie Lebre. (2013)
  Bayesian Networks in R with Applications in Systems Biology.
  Springer, New York. ISBN 978-1461464457.")
  } else cit <- NULL  
  
  Res$qgraph <- qgraph(bn, title = ann, postExpression = cit, ...)
  Res$graph <- getWmat(Res$qgraph)
  colnames(Res$graph) <- rownames(Res$graph) <- colnames(x)
  class(Res) <- "psynetGraph"
  return(Res)
}



# BNlearn boot:
graph_bnboot <- function(
  x, # Data
  scale, 
  title = FALSE, citation = FALSE,
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
    output = bn,
    method = paste0("bnboot.",bnlearnFun))
  
  
  if (title) 
  {
    ann <- paste0("bnlearn bootstrapped posterior probabilities (",bnlearnFun,")")
  } else ann <- NULL

  
  if (citation)
  {
    cit <- CitationExpr(ref = "  Radhakrishnan Nagarajan, Marco Scutari, Sophie Lebre. (2013)
  Bayesian Networks in R with Applications in Systems Biology.
  Springer, New York. ISBN 978-1461464457.")
  } else cit <- NULL  
  
  
  Res$qgraph <- qgraph(bn, probabilityEdges = TRUE, title = ann, postExpression = cit, ...)
  Res$graph <- getWmat(Res$qgraph)
  colnames(Res$graph) <- rownames(Res$graph) <- colnames(x)
  class(Res) <- "psynetGraph"
  return(Res)
}


### IsingFit
graph_IsingFit <- function(
  x, # Data or cormat
  scale, 
  unweighted = FALSE,
  title = FALSE, citation = FALSE,
  verbose = FALSE,
  IsingFitArgs = list(),
  ... # Args sent to qgraph
)
{
  if (verbose) message("psynet: Constructing IsingFit graph")
  
  if (missing(scale)) scale <- autoScale(x)
  
  if (scale != "dichotomous")
  {
    stop("IsingFit routine only supported for dichotomous data")
  }
  
  Res <- do.call(IsingFit,c(list(x=x, family = ifelse(scale=="dichotomous","binomial","gaussian"), plot = FALSE, 
                                 progressbar = FALSE), IsingFitArgs))
  
  g <- Res$weiadj
  if (unweighted)
  {
    g <- 1*(g!=0)
  }
  
  rownames(g) <- colnames(g) <- colnames(x)
  
  Res <- list(
    graph = Res$weiadj,
    output = Res,
    method = "IsingFit")
  
  if (title) 
  {
    ann <- "IsingFit"
  } else ann <- NULL
  
  if (citation)
  {
    cit <- CitationExpr("IsingFit")
  } else cit <- NULL
  
  Res$qgraph <- qgraph(Res$graph, title = ann, postExpression = cit, ...)
  colnames(Res$graph) <- rownames(Res$graph) <- colnames(x)
  class(Res) <- "psynetGraph"
  return(Res)
}








######## glasso ############33

graph_CVglasso <- function(
  x, # Data
  scale, 
  unweighted = FALSE,
  title = FALSE, citation = FALSE,
  verbose = FALSE,
  nonparanormal = TRUE,
  cost = rmspe,
  K = 10,
  ... # Args sent to qgraph
)
{
  if (verbose) message("psynet: Constructing k-fold CV glasso graph")
  
  if (missing(scale)) scale <- autoScale(x)
  
  if (scale != "continuous")
  {
    warning("k-fold CV glasso routine best suited for continuous data")
  }
  
  g <- CVglasso(x, K=K, cost = cost, nonparanormal = nonparanormal)

  if (unweighted)
  {
    g <- 1*(g!=0)
  }
  
  rownames(g) <- colnames(g) <- colnames(x)
  
  Res <- list(
    graph = g,
    output = g,
    method = "CVglasso")
  
  if (title) 
  {
    ann <- "k-fold CV glasso"
  } else ann <- NULL
  
  if (citation)
  {
    cit <- CitationExpr("glasso")
  } else cit <- NULL
  
  Res$qgraph <- qgraph(Res$graph, title = ann, postExpression = cit, ...)
  colnames(Res$graph) <- rownames(Res$graph) <- colnames(x)
  class(Res) <- "psynetGraph"
  return(Res)
}
