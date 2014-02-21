\name{networkAnalysis}
\alias{networkAnalysis}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Estimate network structures
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
networkAnalysis(x, methods = c("cor", "pcor", "alpcor", "plspcor", "pc", "pcskel", "BDbest", "BDpost", "BDavg", "bn.gs", "bnboot.gs", "bn.iamb", "bnboot.iamb", "bn.hc", "bnboot.hc", "bn.tabu", "bnboot.tabu", "bn.mmhc", "bnboot.mmhc", "bn.rsmax2", "bnboot.rsmax2", "bn.mmpc", "bnboot.mmpc", "bn.si.hiton.pc", "bnboot.si.hiton.pc", "bn.chow.liu", "bnboot.chow.liu", "bn.aracne", "bnboot.aracne", "IsingFit"), scale, nonparanormal = FALSE, ask = FALSE, titles = TRUE, citations = TRUE, layout = "spring", layoutToFirst = TRUE, pcAlpha = 0.05, BDargs = list(), bnlearnArgs = list(), bnbootArgs = list(R = 200), parallelEdge = TRUE, labels = TRUE, verbose = TRUE, pcNoDicho = FALSE, adaptDF = TRUE, graphArgs = list(), IsingFitArgs = list(), ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{
A dataset with rows indicating measures and columns indicating variables. All variables must be of the same scale: dichotomous, ordinal or continous.
}
  \item{methods}{
A vector of strings indicating which networks to estimate. The function will estimate all graphs and plot them after each other (e.g., so that \code{pdf()} will create a page for each plot) with layout based on the first graph (rearainging the order of this argument causes the layout to be based on a different graph). Currently supported are: \code{"cor"}, \code{"pcor"}, \code{"alpcor"}, \code{"plspcor"}, \code{"IsingFit"},  \code{"pc"}, \code{"pcskel"}, \code{"BDbest"}, \code{"BDpost"}, \code{"BDavg"}, \code{"bn.gs"}, \code{"bnboot.gs"}, \code{"bn.iamb"}, \code{"bnboot.iamb"}, \code{"bn.hc"}, \code{"bnboot.hc"}, \code{"bn.tabu"}, \code{"bnboot.tabu"}, \code{"bn.mmhc"}, \code{"bnboot.mmhc"}, \code{"bn.rsmax2"}, \code{"bnboot.rsmax2"}, \code{"bn.mmpc"}, \code{"bnboot.mmpc"}, \code{"bn.si.hiton.pc"}, \code{"bnboot.si.hiton.pc"}, \code{"bn.chow.liu"}, \code{"bnboot.chow.liu"}, \code{"bn.aracne"} and \code{"bnboot.aracne"}
}
  \item{scale}{
\code{"dichotomous"}, \code{"ordinal"} or \code{"continuous"}. Is otherwise detected.
}
  \item{nonparanormal}{
Should the nonparanormal transfromation from \code{\link[huge]{huge.npn}} be used on the data?
}
  \item{ask}{
Logical indicating if R should ask to go to the next plot.
}
  \item{titles}{
Logical indicating if titles should be added.
}
  \item{citations}{
Logical indicating if citations should be added.
}
  \item{layout}{
Layout to be used in all graphs
}
  \item{layoutToFirst}{
Equate layout to the layout of the first plot?
}
  \item{pcAlpha}{
Alpha used in pcalg functions
}
  \item{BDargs}{
List of arguments used in \code{\link[BDgraph]{bdgraph}}
}
  \item{bnlearnArgs}{
List of arguments used in \code{\link[bnlearn]{bn}}
}
  \item{bnbootArgs}{
List of arguments used in \code{\link[bnlearn]{bn.boot}}
}
  \item{parallelEdge}{
Same as in \code{\link[qgraph]{qgraph}}
}
  \item{labels}{
Same as in \code{\link[qgraph]{qgraph}}
}
  \item{verbose}{
Should messages be printed to the console indicating what is happening?
}
  \item{pcNoDicho}{
Logical. If \code{TRUE} pcalg for dichotomous data will be run on tetrachoric correlations and not the discrete version. Seems to have more power.
}
  \item{adaptDF}{
Argument used in pcalg
}
  \item{graphArgs}{
Named list with qgraph args that can be used to specify specific arguments. E.g., \code{list(alpcor = list(fade = FALSE))}
}
  \item{IsingFitArgs}{
Arguments used in \code{\link[IsingFit]{IsingFit}}
}
  \item{\dots}{
Arguments sent to \code{\link[qgraph]{qgraph}}
}
}

\value{
A named list with for each estimated graph:
\item{graph}{The weights matrix of the graph}
\item{output}{Output of the method}
\item{qgraph}{The qgraph object}
\item{method}{A string indicating the method used}
}

\author{
Sacha Epskamp <mail@sachaepskamp.com>
}

\examples{
# Load smoking dataset:
data(smoking)

# Estimate three networks:
Res <- networkAnalysis(smoking, methods = c( "cor", "alpcor", "IsingFit" ), ask = FALSE)

# Centrality measures:
centralityPlot(Res)

# Clustering:
centralityPlot(Res)
}
