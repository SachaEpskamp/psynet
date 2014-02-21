# Main function. Works for dataset x, which can be dichotomous, ordinal or continuous
# Supported methods:
# - cor: correlation
# - pcor: concentration
# - alpcor: adaptive lasso concentration
# - pc: pcalg
# - pcskel: pcalg skeleton


networkAnalysis <- function(
  x, # dataset, a matrix
#   methods = c("cor","pcor","lpcor","alpcor","pc","pcskel"),
  methods = c("cor","pcor", "alpcor","plspcor","pc","pcskel","BDbest","BDpost","BDavg","bn.gs","bnboot.gs","bn.iamb","bnboot.iamb","bn.hc","bnboot.hc","bn.tabu","bnboot.tabu","bn.mmhc","bnboot.mmhc","bn.rsmax2","bnboot.rsmax2","bn.mmpc","bnboot.mmpc","bn.si.hiton.pc","bnboot.si.hiton.pc","bn.chow.liu","bnboot.chow.liu","bn.aracne","bnboot.aracne","IsingFit"),
  scale, # "dichotomous", "ordinal" or "continuous". Is otherwise detected
  nonparanormal = FALSE, #nonparanormal transformation?
  ask = FALSE, # Ask to go to next plot?
  titles = TRUE, # Add titles?
  citations = TRUE, # add citations?
  layout = "spring", # layout to be used in all graphs
  layoutToFirst = TRUE, # equate layout to the layout of the first plot?
  pcAlpha = 0.05, # alpha used in pcalg functions
  BDargs = list(),
  bnlearnArgs = list(),
  bnbootArgs = list(R = 200),
  parallelEdge = TRUE, # Same as in qgraph
  labels = TRUE, # Same as in qgraph
  verbose = TRUE, # Print messages such as RUNNOING BDGRAPH
  pcNoDicho = FALSE, # pc on tetra
  adaptDF = TRUE,
  graphArgs = list(), # named list with qgraph args
  IsingFitArgs = list(), # Arguments for IsingFit
  ... # general qgraph arguments
  )
{
  # Set ask par:
  askOrig <- par("ask")
  par(ask = ask)
  
  if (nonparanormal)
  {
    message("Computing nonparanormal transformation")
    x <- huge.npn(x)
    scale <- "continuous"
  }
  
  # set scale of data:
  if (missing(scale))
  {
    scale <- autoScale(x)
  }
  
  # set labels:
  if (isTRUE(labels))
  {
    if (!is.null(colnames(x)))labels <- colnames(x) else labels <- seq_len(ncol(x))
  }
  # Compute correlations:
  CorMat <- getCors(x,scale)
  
  # Results list:
  ### Each contains elements graph, output and qgraph
  Results <- list()
  
  # Compute BDgraph:
  if (any(grepl("BD",methods)))
  {
    if (scale != "continuous")
    {
      warning("Data treated as continous for BDgraph")
    }
    
    if (verbose) message("Computing BDgraph output")
    BDobject <- do.call(bdgraph, c(list(data=x), BDargs))
  }
    
  for (m in seq_along(methods))
  {
    meth <- methods[m]
    
    if (grepl("^bn\\.",methods[m]))
    {
      bnFunName <- gsub("bn.","",methods[m])
      meth <- "bn"
    }
    
    if (grepl("^bnboot\\.",methods[m]))
    {
      bnFunName <- gsub("bnboot.","",methods[m])
      meth <- "bnboot"
    }

    Results[[methods[m]]] <- try(switch(meth,
                         cor = do.call(graph_cor,c(list(CorMat, corMat = TRUE, title = titles, citation = citations, layout = layout, parallelEdge = parallelEdge, verbose = verbose, labels = labels,  ...),graphArgs[[methods[m]]])),
                         pcor = do.call(graph_pcor,c(list(CorMat, corMat = TRUE, title = titles, citation = citations, layout = layout, parallelEdge = parallelEdge, verbose = verbose, labels = labels, ...),graphArgs[[methods[m]]])),
                         alpcor = do.call(graph_alpcor,c(list(x, scale = scale, title = titles, citation = citations, layout = layout, parallelEdge = parallelEdge, verbose = verbose,labels = labels, ...),graphArgs[[methods[m]]])),
                         plspcor = do.call(graph_plspcor,c(list(x, scale = scale, title = titles, citation = citations, layout = layout, parallelEdge = parallelEdge,verbose = verbose,  labels = labels, ...),graphArgs[[methods[m]]])),
                         pc = do.call(graph_pc,c(list(x, corMat = CorMat, scale = scale, title = titles, citation = citations, layout = layout, n = nrow(x), pcAlpha = pcAlpha, parallelEdge = parallelEdge, verbose = verbose, labels = labels, pcNoDicho = pcNoDicho, skeleton = FALSE, adaptDF = adaptDF, ...),graphArgs[[methods[m]]])),
                         pcskel = do.call(graph_pc,c(list(x, corMat = CorMat, scale = scale, title = titles, citation = citations, layout = layout, n = nrow(x), pcAlpha = pcAlpha, parallelEdge = parallelEdge, verbose = verbose, labels = labels, pcNoDicho = pcNoDicho, skeleton =TRUE, adaptDF = adaptDF, ...),graphArgs[[methods[m]]])),
                         BDbest = do.call(graph_BDbest,c(list(BDobject, title = titles, citation = citations, layout = layout, parallelEdge = parallelEdge, verbose = verbose, labels = labels,...),graphArgs[[methods[m]]])),
                         BDpost = do.call(graph_BDpost,c(list(BDobject, title = titles, citation = citations, layout = layout, parallelEdge = parallelEdge, verbose = verbose, labels = labels,maximum = 1,...),graphArgs[[methods[m]]])),
                         BDavg = do.call(graph_BDavg,c(list(BDobject, title = titles, citation = citations, layout = layout, parallelEdge = parallelEdge, verbose = verbose, labels = labels,...),graphArgs[[methods[m]]])),
                         bn = do.call(graph_bnlearn,c(list(x,   scale = scale, title = titles, citation = citations,  bnlearnFun = bnFunName,  bnlearnArgs = bnlearnArgs, layout = layout, parallelEdge = parallelEdge, verbose = verbose, labels = labels,...),graphArgs[[methods[m]]])),
                         bnboot = do.call(graph_bnboot,c(list(x,   scale = scale, title = titles, citation = citations,  bnlearnFun = bnFunName,  bnlearnArgs = bnlearnArgs,  bnbootArgs = bnbootArgs, layout = layout, parallelEdge = parallelEdge, verbose = verbose, labels = labels,maximum = 1,...),graphArgs[[methods[m]]])),
                         IsingFit = do.call(graph_IsingFit,c(list(x, title = titles, citation = citations, layout = layout, parallelEdge = parallelEdge, verbose = verbose, labels = labels, IsingFitArgs = IsingFitArgs, ...),graphArgs[[methods[m]]]))
                         ))
    
    if (m == 1 && layoutToFirst)
    {
      layout <- Results[[methods[m]]]$qgraph$layout
    }
    
  }
  
  
  par(ask = askOrig)
  
  class(Results) <- "psynet"
  
  return(Results)
}

