\name{wtd.t.test}
\alias{wtd.t.test}
\title{
Produces weighted Student's t-tests with standard errors and significance.
}
\description{
\code{wtd.t.test} produces either one- or two-sample t-tests comparing weighted data streams to one another.
}
\usage{
wtd.t.test(x, y=0, weight=NULL, weighty=NULL, samedata=TRUE)
}
\arguments{
  \item{x}{
\code{x} is a numerical vector which the researcher wishes to test against \code{y}.
}
  \item{y}{
\code{y} can be either a single number representing an alternative hypothesis or a second numerical vector which the researcher wishes to compare against \code{x}.
}
  \item{weight}{
\code{weight} is an optional vector of weights to be used to determine the weighted mean and variance for the \code{x} vector for all t-tests.  If \code{weighty} is unspecified and \code{samedata} is TRUE, this weight will be assumed to apply to both \code{x} and \code{y}.
}
  \item{weighty}{
\code{weighty} is an optional vector of weights to be used to determine the weighted mean and variance for the \code{y} vector for two-sample t-tests.  If \code{weighty} is unspecified and \code{samedata} is TRUE, this weight will be assumed to equal \code{weightx}.  If \code{weighty} is unspecified and \code{samedata} is FALSE, this weight will be assumed to equal 1 for all cases.
}
  \item{samedata}{
\code{samedata} is an optional identifier for whether the \code{x} and \code{y} data come from the same data stream for a two-sample test.  If true, \code{wtd.t.test} assumes that \code{weighty} should equal \code{weightx} if (1) \code{weighty} is unspecified, and (2) the lengths of the two vectors are identical.
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
A list element with an identifier for the test; coefficients for the t value, degrees of freedom, and p value of the t-test; and additional statistics of potential interest.
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
t2 <- rev(test)+1
weight <- c(.5,.5,.5,.5,.5,1,1,1,1,2,2,2,2,2)

wtd.t.test(test, t2)
wtd.t.test(test, t2, weight)
}
\keyword{ ~t.test }
\keyword{ ~weights }
