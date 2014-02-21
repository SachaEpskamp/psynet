centralityPlot <- function(x, labels, relative = TRUE)
{
  LongCent <- centralityTable(x, labels, relative)
  
  # Ordereing by node name to make nice paths:
  LongCent <- LongCent[order(LongCent$node),] 
  
  # PLOT:
  g <- ggplot(LongCent, aes(x = value, y = node, group = method, colour = method)) + geom_path() + 
    facet_grid(~ measure, scales = "free") +  xlab("") + ylab("") + geom_point()
  
  return(g)  
}
