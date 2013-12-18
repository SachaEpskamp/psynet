addTitle <- function(x)
{
  text(par('usr')[1] + (par('usr')[2] - par('usr')[1])/40 ,par("usr")[4] - (par('usr')[4] - par('usr')[3])/40,x, adj = c(0,1))      
}

autoScale <- function(x)
{
  resp <- unique(unlist(x))
  resp <- resp[!is.na(resp)]
  
  if (all(resp %in% c(0,1)))
  {
    scale <- "dichotomous"
  } else if (all(resp%%1 == 0))
  {
    scale <- "ordinal"
  } else scale <- "continuous"
  
  message(paste("Scale set to",scale))
  return(scale)
}

getCors <- function(x,scale)
{
  if (missing(scale))
  {
    scale <- autoScale(x)
  }
  
  CorMat <- switch(scale,
                   dichotomous = tetrachoric(x)$rho,
                   ordinal = polychoric(x)$rho,
                   continuous = cor(x, use = "pairwise.complete.obs")
  )
  
  return(CorMat)
}