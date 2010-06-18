\name{dummify}
\alias{dummify}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Separate a factor into separate dummy variables for each level.
}
\description{
\code{dummify} creates a matrix with columns signifying separate dummy variables for each level of a factor.  The column names are the former levels of the factor.
}
\usage{
dummify(x)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{
\code{x} is a factor the researcher desires to split into separate dummy variables.
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
\code{dummify} returns a matrix with a number of rows equal to the length of \code{x} and a number of columns equal to the number of levels of \code{x}.
}
\references{
%% ~put references to the literature/web site here ~
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
data("anes04")

anes04$agecats <- cut(anes04$age, c(17, 25,35,45,55,65, 99))
levels(anes04$agecats) <- c("age1824", "age2534", "age3544",
          "age4554", "age5564", "age6599")

agedums <- dummify(anes04$agecats)
table(anes04$agecats)
summary(agedums)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~dummy }
\keyword{ ~split }
