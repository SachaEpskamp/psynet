
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
  
  colnames(CorMat) <- rownames(CorMat) <- colnames(x)
  
  return(CorMat)
}

getCitation <- function(x)
{
  str <- capture.output(citation(x))
  str <- gsub("^\\s+","",str[(grep("^To cite",str)+2):(grep("^A BibTeX entry",str)-2)])
  return(paste(str,collapse="\n"))
}

CitationExpr <- function(x,ref,cex=0.4)
{
  if (missing(ref)) x <- getCitation(x) else
  {
    x <- ref
    x <- gsub("^\\s+","",x)
    x <- paste(x,collapse="\n")
  }
  paste("text(par('usr')[2] - (par('usr')[2] - par('usr')[1])/40 ,par('usr')[3] + (par('usr')[4] - par('usr')[3])/40,",deparse(x),", adj = c(1,0),cex=",cex,")")      
}
