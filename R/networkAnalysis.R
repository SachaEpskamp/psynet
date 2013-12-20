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
  methods = c("cor","pcor", "alpcor","plspcor","pc","pcskel","BDbest","BDpost","BDavg","bn.gs","bnboot.gs","bn.iamb","bnboot.iamb","bn.hc","bnboot.hc","bn.tabu","bnboot.tabu","bn.mmhc","bnboot.mmhc","bn.rsmax2","bnboot.rsmax2","bn.mmpc","bnboot.mmpc","bn.si.hiton.pc","bnboot.si.hiton.pc","bn.chow.liu","bnboot.chow.liu","bn.aracne","bnboot.aracne"),
  scale, # "dichotomous", "ordinal" or "continuous". Is otherwise detected
  ask = FALSE, # Ask to go to next plot?
  titles = TRUE, # Add titles?
  layout = "spring", # layout to be used in all graphs
  layoutToFirst = TRUE, # equate layout to the layout of the first plot?
  pcAlpha = 0.05, # alpha used in pcalg functions
  BDargs = list(),
  bnlearnArgs = list(),
  bnbootArgs = list(R = 200),
  parallelEdge = TRUE, # Same as in qgraph
  verbose = TRUE, # Print messages such as RUNNOING BDGRAPH
  ... # qgraph arguments
  )
{
  # Set ask par:
  par(ask = ask)
  
  # set scale of data:
  if (missing(scale))
  {
    scale <- autoScale(x)
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
    if (grepl("^bn\\.",methods[m]))
    {
      bnFunName <- gsub("bn.","",methods[m])
      methods[m] <- "bn"
    }
    
    if (grepl("^bnboot\\.",methods[m]))
    {
      bnFunName <- gsub("bnboot.","",methods[m])
      methods[m] <- "bnboot"
    }

    Results[[methods[m]]] <- try(switch(methods[m],
                         cor = graph_cor(CorMat, corMat = TRUE, title = titles, layout = layout, parallelEdge = parallelEdge, verbose = verbose, ...),
                         pcor = graph_pcor(CorMat, corMat = TRUE, title = titles, layout = layout, parallelEdge = parallelEdge, verbose = verbose, ...),
                         alpcor = graph_alpcor(x, scale = scale, title = titles, layout = layout, parallelEdge = parallelEdge, verbose = verbose, ...),
                         plspcor = graph_plspcor(x, scale = scale, title = titles, layout = layout, parallelEdge = parallelEdge,verbose = verbose,  ...),
                         pc = graph_pc(CorMat, scale = scale, title = titles, layout = layout, n = nrow(x), pcAlpha = pcAlpha, parallelEdge = parallelEdge, verbose = verbose, ...),
                         pcskel = graph_pcskel(CorMat, scale = scale, title = titles, layout = layout, n = nrow(x), pcAlpha = pcAlpha, parallelEdge = parallelEdge, verbose = verbose, ...),
                         BDbest = graph_BDbest(BDobject, title = titles, layout = layout, parallelEdge = parallelEdge, verbose = verbose, ...),
                         BDpost = graph_BDpost(BDobject, title = titles, layout = layout, parallelEdge = parallelEdge, verbose = verbose, ...),
                         BDavg = graph_BDavg(BDobject, title = titles, layout = layout, parallelEdge = parallelEdge, verbose = verbose, ...),
                         bn = graph_bnlearn(x,   scale = scale, title = titles,  bnlearnFun = bnFunName,  bnlearnArgs = bnlearnArgs, layout = layout, parallelEdge = parallelEdge, verbose = verbose, ...),
                         bnboot = graph_bnboot(x,   scale = scale, title = titles,  bnlearnFun = bnFunName,  bnlearnArgs = bnlearnArgs,  bnbootArgs = bnbootArgs, layout = layout, parallelEdge = parallelEdge, verbose = verbose, ...)
                         ))
    
    if (m == 1 && layoutToFirst)
    {
      layout <- Results[[methods[m]]]$qgraph$layout
    }
    
  }
  
  return(Results)
}

