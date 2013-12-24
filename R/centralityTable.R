centralityList <- function(x)
{
  lapply(lapply(x,'[[','qgraph'),centrality)
}