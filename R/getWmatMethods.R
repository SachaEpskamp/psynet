getWmat.psynetGraph <- function(x,...)
{
  return(getWmat(x$qgraph))
}

getWmat.psynet <- function(x,...)
{
  if (length(x)==1) return(getWmat(x[[1]]$graph)) else return(lapply(lapply(x,'[[',"qgraph"),getWmat))
}