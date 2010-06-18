\name{wpct}
\alias{wpct}
\title{
Provides a weighted table of percentages for any variable.
}
\description{
\code{wpct} produces a weighted table of the proportion of data in each category for any variable.  This is simply a weighted frequency table divided by its sum.
}
\usage{
wpct(x, weight, na.rm=TRUE)
}
\arguments{
  \item{x}{
\code{x} should be a vector for which a set of proportions is desired.
}
  \item{weight}{
\code{weight} is a vector of weights to be used to determining the weighted proportion in each category of \code{x}.
}
  \item{na.rm}{
If \code{na.rm} is true, missing data will be dropped.  If \code{na.rm} is false, missing data will return an error.
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
A table object of length equal to the number of separate values of \code{x}.
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
weight <- c(.5,.5,.5,.5,.5,1,1,1,1,2,2,2,2,2)

wpct(test)
wpct(test, weight)
}
\keyword{ ~frequency }
\keyword{ ~contingency }
\keyword{ ~weights }
\keyword{ ~tables }

