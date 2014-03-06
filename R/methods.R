plot.psynet <- function(
  x, # psynet object
  include, # character string with graphs to include
  ask = FALSE,
  layout = "spring", # layout to be used in all graphs
  avgLayout = TRUE,
  layoutToFirst = FALSE, # equate layout to the layout of the first plot?
  layout.par = list(),
  ... # qgraph args
  )
{
 par(ask = ask) 
 
 if (missing(include))
 {
   include <- names(x)   
 }
 
 Res <- list()
 
 if (avgLayout)
 {
   layout <- averageLayout(x, layout = layout, layout.par = layout.par)
 }
 
 for (m in seq_along(include))
 {
   Res[[include[m]]] <- qgraph(x[[include[m]]]$qgraph, layout = layout, DoNotPlot = FALSE, ...)
  
   if (m == 1 && layoutToFirst)
   {
     layout <- Res[[include[m]]]$layout
   }
 }
   
 return(Res)
}