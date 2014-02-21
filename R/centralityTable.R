centralityTable <- function(x, labels)
{
  # Check package:
  stopifnot(is(x,"psynet") | is(x,"psynetGraph"))
  
  # Get labels:
  # set labels:
  if (missing(labels))
  {
    W <- getWmat(x)
    if (is.list(W)) labels <- colnames(W[[1]]) else labels <- colnames(W) 
  } 
  
  # Is the graph a single graph?
  singleGraph <- is(x, "psynetGraph") || (is(x, "psynet") & length(x) == 1)
  
  # Compute centrality:
  CentAuto <- qgraph:::centrality_auto(x)
  if (singleGraph)
  {
    CentAuto <- list(CentAuto)
  }
  
  # Compute clustering:
  ClustAuto <- qgraph:::clustcoef_auto(x)
  if (singleGraph)
  {
    ClustAuto <- list(ClustAuto)
  }
  
  # Add method and labels to tables:
  for (i in seq_along(x))
  {
    CentAuto[[i]][['node.centrality']][['method']] <- x[[i]]$method
    CentAuto[[i]][['node.centrality']][['node']] <- labels
    CentAuto[[i]][['edge.centrality']][['method']] <- x[[i]]$method
    ClustAuto[[i]][['method']] <- x[[i]]$method
    ClustAuto[[i]][['node']] <- labels
  }
  ## WIDE FORMAT TABLE:
  WideCent <- rbind.fill(lapply(CentAuto,'[[','node.centrality'))
  
  # LONG FORMAT:
  LongCent <- melt(WideCent, variable.name = "measure", id.var = c("method","node"))
  
  return(LongCent)  
}