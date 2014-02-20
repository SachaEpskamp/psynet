centralityList <- function(x)
{
  # A small change
  lapply(lapply(x,'[[','qgraph'),centrality)
}