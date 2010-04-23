\name{rakelist}
\alias{rakelist}
\alias{summary.anesrakelist}
\alias{print.anesrakelist}
\alias{rakeonvar}
\alias{rakeonvar.factor}
\alias{rakeonvar.logical}
\alias{rakeonvar.numeric}
\alias{rakeonvar.default}
\title{
Function to perform full ANES weighting on selected variables.
}
\description{
\code{rakelist} takes a list of variables and target values weights a dataset with those variables to match the targets via raking.  It is the primary workhorse command of \code{anesrake}.
}
\usage{
rakelist(inputter, dataframe, caseid, weightvec = NULL, cap = 999999, verbose = FALSE, maxit = 1000, convcrit = 0.01)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{inputter}{
The \code{inputter} object should contain a list of all target values for the raking procedure.  Each list element in \code{inputter}  should be a vector corresponding to the weighting targets for a single variable.  Hence, the vector enumerating the weighting targets for a variable with 2 levels should be of length 2, while a vector enumerating the weighting targets for a variable with 5 levels should be of length 5.  List elements in inputter should be named according to the variable that they will match in the corresponding dataset.  Hence, a list element enumerating the proportion of the sample that should be of each gender should be labeled "female" if the variable in \code{dataframe} is also titled "female."

\code{inputter} elements must be vectors and can be of class numeric, or factor and must match the class of the corresponding variable in \code{dataframe}.  Logical variables in \code{dataframe} can be matched to a numeric vector of length 2 and ordered with the \code{TRUE} target as the first element and the \code{FALSE} target as the second element.  Targets for factors must be labeled to match every level present in the dataframe (e.g. a variable with 2 age groups "under40" and "over40" should have elements named "under40" and "over40" respectively).  \code{anesrake} attempts to conform any unrecognized types of vectors to \code{class(numeric)}. Weighting targets can be entered either as an N to be reached or as a percent for any given variable.  Targets can be either proportions (ideal) or the number of individuals in the population in each target category (N).  Totals of greater than 1.5 for any given list element are treated as Ns, while values of less than 1.5 are treated as percentages.
}
  \item{dataframe}{
The \code{dataframe} command identifies a \code{data.frame} object of the data to be weighted.  The data.frame must contain all of the variables that will be used in the weighting process and those variables must have the same names as are present in the \code{inputter} list element.
}
  \item{caseid}{
The \code{caseid} command identifies a unique case identifier for each individual in the dataset.  If filters are to be used, the resulting list of weights will be a different length from the overall \code{dataframe}.  \code{caseid} is included in the output so that weights can be matched to the dataset of relevance.  \code{caseid} must be of a length matching the number of cases in \code{dataframe}.
}
  \item{weightvec}{
\code{weightvec} is an optional input if some kind of base weights, stratification correction, or other sampling probability of note that should be accounted for before weighting is conducted.  If defined, \code{weightvec} must be of a length equivalent to the number of cases in the \code{dataframe}.  If undefined, \code{weightvec} will be automatically seeded with a vector of 1s.
}
  \item{cap}{
\code{cap} defines the maximum weight to be used.  \code{cap} can be defined by the user with the command \code{cap=x}, where \code{x} is any value above 1 at which the algorithm will cap weights.  If \code{cap} is set below 1, the function will return an error.  If \code{cap} is set between 1 and 1.5, the function will return a warning that the low cap may substantially increase the amount of time required for weighting.  In the absence of a user-defined cap, the algorithm defaults to a starting value of 5 in line with DeBell and Krosnick, 2009.  For no cap, \code{cap} simply needs to be set to an arbitrarily high number. (Note: Capping using the \code{cap} command caps at each iteration.)
}
  \item{verbose}{
Users interested in seeing the progress of the algorithm can set \code{verbose} to equal \code{TRUE}.  The algorithm will then inform the user of the progress of each raking and capping iteration.
}
  \item{maxit}{
Users can set a maximum number of iterations for the function should it fail to converge using \code{maxit=X}, where \code{X} is the maximum number of iterations.  The default is set to 1000.
}
  \item{convcrit}{
\code{convcrit} is the criterion for convergence.  The raking algorithm is determined to have converged when the most recent iteration represents less than a \code{convcrit} percentage improvement over the prior iteration.
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
A list object of \code{rakelist} has the following elements:
\item{weightvec}{Vector of weights From raking algorithm}
\item{caseid}{Case IDs for final weights -- helpful for matching \code{weightvec} to cases if a filter is used}
\item{iterations}{Number of iterations required for convergence (or non-convergence) of final model}
\item{nonconvergence}{Measure of remaining discrepancy from benchmarks if convergence was not achieved}
\item{converge}{Notes whether full convergence was achieved, algorithm failed to converge because convergence was not possible, or maximum iterations were reached}
\item{varsused}{List of variables selected for weighting}
\item{targets}{\code{inputter} from above, a list of the targets used for weighting}
\item{dataframe}{Copy of the original \code{dataframe} used for weighting (\code{filter} variable applied if specified)}
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
}
\keyword{ ~raking }
\keyword{ ~weights }
