clusteringTable <- function(x, labels, signed = FALSE, relative = TRUE)
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
  
  # Compute clustering:
  ClustAuto <- qgraph:::clustcoef_auto(x)
  if (singleGraph)
  {
    ClustAuto <- list(ClustAuto)
  }

  # Removed signed, add method and labels to tables:
  for (i in seq_along(x))
  {
    if (any(grepl("signed_",names(ClustAuto[[i]]))))
    {
      ClustAuto[[i]] <- ClustAuto[[i]][,grepl("signed_",names(ClustAuto[[i]])) == signed]
      names(ClustAuto[[i]]) <- gsub("signed_","",names(ClustAuto[[i]])) 
    }
    
    # Relativate:
    if (relative)
    {
      for (j in seq_len(ncol(ClustAuto[[i]])))
      {
        ClustAuto[[i]][j] <- ClustAuto[[i]][j] / max(abs(ClustAuto[[i]][j]))
      } 
    }
    
    ClustAuto[[i]][['method']] <- x[[i]]$method
    ClustAuto[[i]][['node']] <- labels
  }
  
  ## WIDE FORMAT TABLE:
  Wide <- rbind.fill(ClustAuto)
  
  # LONG FORMAT:
  Long <- melt(Wide, variable.name = "measure", id.var = c("method","node"))
  
  return(Long)  
}