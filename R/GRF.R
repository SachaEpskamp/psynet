## Wrapper around networkAnalysis to strictly estimate Gaussian Random Fields:
GRF <-  function(
  x, # dataset, a matrix
  methods = c("pcor","pcor.shrink",  "alpcor","plspcor","BDavg",
             "CVglasso"),
  unweighted = FALSE, # Can be used to estimate unweighted networks instead
  nonparanormal = TRUE,
  minimum = 0,
  maximum = 1,
  cut = 0.1,
  esize = 10,
  ... # Arguments sent to networkAnalysis()
)
{
  if (unweighted)
  {
    methods <- paste0(methods,".UW")
    methods[grepl("BDavg",methods)] <- "BDbest"
  }
  networkAnalysis(x, methods, nonparanormal = nonparanormal, scale = "continuous", minimum = minimum, maximum = maximum, cut = cut, esize = esize, ...)
}