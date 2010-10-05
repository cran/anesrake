\name{stdz}
\alias{stdz}
\title{
Standardizes any numerical vector, with weights.
}
\description{
\code{stdz} produces a standardized copy of any input variable.  It can also standardize a weighted variable to produce a copy of the original variable standardized around its weighted mean and variance.
}
\usage{
stdz(x, weight=NULL)
}
\arguments{
  \item{x}{
\code{x} should be a numerical vector which the researcher wishes to standardize.
}
  \item{weight}{
\code{weight} is an optional vector of weights to be used to determining the weighted mean and variance for standardization.
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
A vector of length equal to x with a (weighted) mean of zero and a (weighted) standard deviation of 1.
}
\author{
Josh Pasek, Ph.D. Candidate in Communication at Stanford University (www.joshpasek.com) 
}
\note{
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
\code{\link{wtd.cor}}
\code{\link{wtd.partial.cor}}
\code{\link{wtd.chi.sq}}
\code{\link{wtd.t.test}}

}
\examples{
test <- c(1,1,1,1,1,1,2,2,2,3,3,3,4,4)
weight <- c(.5,.5,.5,.5,.5,1,1,1,1,2,2,2,2,2)

summary(stdz(test))
summary(stdz(test, weight))
wtd.mean(stdz(test, weight), weight)
wtd.var(stdz(test, weight), weight)
}
\keyword{ ~standardize }
\keyword{ ~standardization }
\keyword{ ~weights }
