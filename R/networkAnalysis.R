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
  methods = c("cor","pcor", "alpcor","plspcor","pc","pcskel","BDbest","BDpost","BDavg"),
  scale, # "dichotomous", "ordinal" or "continuous". Is otherwise detected
  ask = FALSE, # Ask to go to next plot?
  titles = TRUE, # Add titles?
  layout = "spring", # layout to be used in all graphs
  layoutToFirst = TRUE, # equate layout to the layout of the first plot?
  pcAlpha = 0.05, # alpha used in pcalg functions
  BDargs = list(),
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
    
    message("Computing BDgraph output")
    BDobject <- do.call(bdgraph, c(list(data=x), BDargs))
  }
    
  pb <- txtProgressBar(0,length(methods),style=3)
  for (m in seq_along(methods))
  {
    Results[[methods[m]]] <- switch(methods[m],
                         cor = graph_cor(CorMat, corMat = TRUE, title = titles, layout = layout, ...),
                         pcor = graph_pcor(CorMat, corMat = TRUE, title = titles, layout = layout, ...),
                         alpcor = graph_alpcor(x, scale = scale, title = titles, layout = layout, ...),
                         plspcor = graph_plspcor(x, scale = scale, title = titles, layout = layout, ...),
                         pc = graph_pc(CorMat, scale = scale, title = titles, layout = layout, n = nrow(x), pcAlpha = pcAlpha, ...),
                         pcskel = graph_pcskel(CorMat, scale = scale, title = titles, layout = layout, n = nrow(x), pcAlpha = pcAlpha, ...),
                         BDbest = graph_BDbest(BDobject, title = titles, layout = layout, ...),
                         BDpost = graph_BDpost(BDobject, title = titles, layout = layout, ...),
                         BDavg = graph_BDavg(BDobject, title = titles, layout = layout, ...)
                         )
    
    if (m == 1 && layoutToFirst)
    {
      layout <- Results[[methods[m]]]$qgraph$layout
    }
    
    setTxtProgressBar(pb, m)
  }
  close(pb)
  
  return(Results)
}

