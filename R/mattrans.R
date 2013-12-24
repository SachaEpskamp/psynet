mat2facdf <- function(x)
{
  x <- as.data.frame(x)
  for (i in 1:ncol(x)) x[,i] <- factor(x[,i])
  return(x)
}