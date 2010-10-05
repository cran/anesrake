\name{wtd.partial.cor}
\alias{wtd.partial.cor}
\alias{onecor.partial.wtd}
\title{
Produces weighted partial correlations with standard errors and significance.
}
\description{
\code{wtd.cor} produces a weighted Pearsons correlation coefficient comparing two variables or matrices, with control variables.  
}
\usage{
wtd.partial.cor(x, y=NULL, preds=NULL, weight=NULL, collapse=TRUE)
}
\arguments{
  \item{x}{
\code{x} should be a matrix or vector which the researcher wishes to correlate with \code{y}.
}
  \item{y}{
\code{y} should be a numerical vector or matrix which the researcher wishes to correlate with \code{x}.  If \code{y} is NULL, \code{x} will be used instead
}
  \item{preds}{
\code{preds} should be a numerical vector or matrix which the researcher wishes to control for when correlating \code{x} with itself or with \code{y}.  If \code{preds} is not defined, this function will produce standard Pearson's correlations instead.
}
  \item{weight}{
\code{weight} is an optional vector of weights to be used to determining the weighted mean(s) and variance(s) for calculation of the correlations.
}
  \item{collapse}{
\code{collapse} is an indicator for whether the data should be collapsed to a simpler form if either x or y is a vector instead of a matrix.
}
}
\value{
A list with matrices for the estimated correlation coefficient, the standard error on that correlation coefficient, the t-value for that correlation coefficient, and the p value for the significance of the correlation.  If the list can be simplified, simplification will be done.
}
\author{
Josh Pasek, Ph.D. Candidate in Communication at Stanford University (www.joshpasek.com) 
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
\code{\link{stdz}}
\code{\link{wtd.cor}}
\code{\link{wtd.t.test}}
\code{\link{wtd.chi.sq}}
}
\examples{
test <- c(1,1,1,1,1,1,2,2,2,3,3,3,4,4)
t2 <- rev(test)
weight <- c(.5,.5,.5,.5,.5,1,1,1,1,2,2,2,2,2)
control <- c(1,1,1,1,2,2,2,2,2,3,3,3,4,4)

wtd.partial.cor(x=test, y=t2)
wtd.partial.cor(x=test, y=t2, weight=weight)
wtd.partial.cor(x=test, y=t2, preds=control, weight=weight)
}
\keyword{ ~correlation }
\keyword{ ~Pearson }
\keyword{ ~weights }
