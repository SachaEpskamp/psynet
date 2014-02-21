getWmat.psynetGraph <- function(x,...)
{
  return(x$graph)
}

getWmat.psynet <- function(x,...)
{
  if (length(x)==1) return(x[[1]]$graph) else return(lapply(x,'[[',"graph"))
}