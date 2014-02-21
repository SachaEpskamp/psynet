centralityTable <- function(x, labels, relative = TRUE)
{
  # Check package:
  stopifnot(is(x,"psynet") | is(x,"psynetGraph"))
  
  # Get labels:
  # set labels:
  if (missing(labels))
  {
    W <- getWmat(x)
    if (is.list(W)) 
    {
      labels <- colnames(W[[1]]) 

      if (is.null(labels)) labels <- seq_len(nrow(W[[1]]))
    } else 
    {
      labels <- colnames(W) 
      if (is.null(labels)) labels <- seq_len(nrow(W))
    }
  } 
  
  # Is the graph a single graph?
  singleGraph <- is(x, "psynetGraph") || (is(x, "psynet") & length(x) == 1)
  
  # Compute centrality:
  CentAuto <- qgraph:::centrality_auto(x)
  if (singleGraph)
  {
    CentAuto <- list(CentAuto)
  }
  
#   # Compute clustering:
#   ClustAuto <- qgraph:::clustcoef_auto(x)
#   if (singleGraph)
#   {
#     ClustAuto <- list(ClustAuto)
#   }
  
  # Add method and labels to tables:
  for (i in seq_along(x))
  {
    # Relativate:
    if (relative)
    {
      for (j in seq_len(ncol(CentAuto[[i]][['node.centrality']])))
      {
        CentAuto[[i]][['node.centrality']][,j] <- CentAuto[[i]][['node.centrality']][,j] / max(abs(CentAuto[[i]][['node.centrality']][,j]), na.rm = TRUE)
      } 
    }
    
    CentAuto[[i]][['node.centrality']][['method']] <- x[[i]]$method
    CentAuto[[i]][['node.centrality']][['node']] <- labels
    CentAuto[[i]][['edge.centrality']][['method']] <- x[[i]]$method
#     ClustAuto[[i]][['method']] <- x[[i]]$method
#     ClustAuto[[i]][['node']] <- labels
  }

  ## WIDE FORMAT TABLE:
  WideCent <- rbind.fill(lapply(CentAuto,'[[','node.centrality'))
  
  # LONG FORMAT:
  LongCent <- melt(WideCent, variable.name = "measure", id.var = c("method","node"))
  
  return(LongCent)  
}