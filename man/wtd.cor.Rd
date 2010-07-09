\name{wtd.cor}
\alias{wtd.cor}
\alias{onecor.wtd}
\title{
Produces weighted correlations with standard errors and significance.
}
\description{
\code{wtd.cor} produces a Pearsons correlation coefficient comparing two variables or matrices.  
}
\usage{
wtd.cor(x, y=NULL, weight=NULL, collapse=TRUE)
}
\arguments{
  \item{x}{
\code{x} should be a matrix or vector which the researcher wishes to correlate with \code{y}.
}
  \item{y}{
\code{y} should be a numerical vector or matrix which the researcher wishes to correlate with \code{x}.  If \code{y} is NULL, \code{x} will be used instead
}
  \item{weight}{
\code{weight} is an optional vector of weights to be used to determining the weighted mean and variance for calculation of the correlations.
}
  \item{collapse}{
\code{collapse} is an indicator for whether the data should be collapsed to a simpler form if either x or y is a vector instead of a matrix.
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
A list with matrices for the estimated correlation coefficient, the standard error on that correlation coefficient, the t-value for that correlation coefficient, and the p value for the significance of the correlation.  If the list can be simplified, simplification will be done.
}
\author{
Josh Pasek, Ph.D. Candidate in Communication at Stanford University (www.joshpasek.com) 
}
\note{
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
test <- c(1,1,1,1,1,1,2,2,2,3,3,3,4,4)
t2 <- rev(test)
weight <- c(.5,.5,.5,.5,.5,1,1,1,1,2,2,2,2,2)

wtd.cor(test, t2)
wtd.cor(test, t2, weight)
}
\keyword{ ~correlation }
\keyword{ ~Pearson }
\keyword{ ~weights }
