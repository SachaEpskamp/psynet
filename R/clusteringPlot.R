clusteringPlot <- function(x, labels, signed = FALSE, relative = TRUE)
{
  Long <- clusteringTable(x, labels, signed, relative)
  
  # Ordereing by node name to make nice paths:
  Long <- Long[order(Long$node),] 
  
  # PLOT:
  g <- ggplot(Long, aes(x = value, y = node, group = method, colour = method)) + geom_path() + 
    facet_grid(~ measure, scales = "free") +  xlab("") + ylab("") + geom_point()
  
  return(g)  
}
