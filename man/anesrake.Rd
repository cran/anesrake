\name{anesrake}
\alias{anesrake}
\alias{print.anesrake}
\alias{summary.anesrake}
\title{
Function to perform full ANES variable selection and weighting.
}
\description{
\code{anesrake} takes a list of variables and target values and determines how they should be weighted to match the procedures outlined in DeBell and Krosnick, 2009.  It then performs raking to develop weights for the variables selected such that they match the targets provided.
}
\usage{
anesrake(inputter, dataframe, caseid, weightvec = NULL,
cap = 5, verbose = FALSE, maxit = 1000, type = "pctlim",
pctlim = 5, nlim = 5, filter = 1, choosemethod = "total",
iterate = TRUE, convcrit = 0.01)
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
  \item{type}{
\code{type} identifies which manner of variable identification should be used to select weighting variables.  Five options are available: \code{type=c("nolim", "pctlim", "nlim", "nmin", "nmax")}.  If \code{type="nolim"}, all variables specified in \code{inputter} will be included in the weighting procedure.  If \code{type="pctlim"} (DEFAULT), the variable selection algorithm will assess which variables have distributions that deviate from their targets by more than the amount specified by the \code{pctlim} command using the method \code{choosemethod}.  If \code{type="nlim"}, the variable selection algorithm will use the number of varibles specified by \code{nlim}, choosing the most discrepant variables as identified by the \code{choosemethod} command.  If \code{type="nmin"}, the variable selection algorithm will use at least \code{nlim} variables, but will include more if additional variables are off by more than \code{pctmin} (all identified using \code{choosemethod}).  If \code{type="nmax"}, the variable selection algorithm will use no more than \code{nlim} variables, but will only use that many variables if at least that many are off by more than \code{pctlim} (all identified using \code{choosemethod}).
}
  \item{pctlim}{
\code{pctlim} is the discrepancy limit for selection.  Variable selection will only select variables that are discrepant by more than the amount specified. \code{pctlim} can be specified either in percentage points (5 is 5 percent) or as a decimal (.05 is 5 percent).  The algorithm assumes that a decimal is being used if \code{pctlim<1}.  Hence researchers interested in a discrepancy limit of half a percent would need to use \code{pctlim=.005}.  
}
  \item{nlim}{
\code{nlim} is the number of variables to be chosen via the variable selection method chosen in \code{choosemethod}.
}
  \item{filter}{
\code{filter} is a vector of 1 for cases to be included in weighting and 0 for cases that should not be included.  The \code{filter} vector must have the same number of cases as the \code{dataframe}.  In the absence of a user-defined \code{filter}, the algorithm defaults to a starting value of 1 (inclusion) for all individuals.
}
  \item{choosemethod}{
\code{choosemethod} is the method for choosing most discrepant variables.  Six options are available: \code{choosemethod=c("total", "max", "average", "totalsquared", "maxsquared", "averagesquared")}.
If \code{choosemethod="total"}, variable choice is determined by the sum of the differences between actual and target values for each prospective weighting variable.  If \code{choosemethod="max"}, variable choice is determined by the largest individual difference between actual and target values for each prospective weighting variable.  If \code{choosemethod="average"}, variable choice is determined by the mean of the differences between actual and target values for each prospective weighting variable.  If \code{choosemethod="totalsquared"}, variable choice is determined by the sum of the squared differences between actual and target values for each prospective weighting variable.  If  \code{choosemethod="maxsquared"}, variable choice is determined by the largest squared difference between actual and target values for each prospective weighting variable (note that this is identical to  \code{choosemethod="max"} if the selection type is \code{nlim}).  If  \code{choosemethod="averagesquared"}, variable choice is determined by the mean of the squared differences between actual and target values for each prospective weighting variable.  
}
  \item{iterate}{
\code{iterate} is a logical variable for how raking should proceed if  \code{type=c("pctlim", "nmin", "nmax")} conditions.  If \code{iterate=TRUE}, \code{anesrake} will check whether any variables that were not used in raking deviate from their targets by more than \code{pctlim} percent.  When this is the case, raking will be rerun using the raked weights as seeds (\code{weightvec}) with additional varibles that meet this qualification after raking included as well.  For the \code{type="nmax"} condition, this will only occur if \code{nlim} has not been met.
}
  \item{convcrit}{
\code{convcrit} is the criterion for convergence.  The raking algorithm is determined to have converged when the most recent iteration represents less than a \code{convcrit} percentage improvement over the prior iteration.
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
A list object of \code{anesrake} has the following elements:
\item{weightvec}{Vector of weights From raking algorithm}
\item{type}{Type of variable selection used (identical to specified \code{type})}
\item{caseid}{Case IDs for final weights -- helpful for matching \code{weightvec} to cases if a filter is used}
\item{varsused}{List of variables selected for weighting}
\item{choosemethod}{Method for choosing variables for weighting (identical to specified \code{choosemethod})}
\item{converge}{Notes whether full convergence was achieved, algorithm failed to converge because convergence was not possible, or maximum iterations were reached}
\item{nonconvergence}{Measure of remaining discrepancy from benchmarks if convergence was not achieved}
\item{targets}{\code{inputter} from above, a list of the targets used for weighting}
\item{dataframe}{Copy of the original \code{dataframe} used for weighting (\code{filter} variable applied if specified)}
\item{iterations}{Number of iterations required for convergence (or non-convergence) of final model}
\item{iterate}{Copy of \code{iterate} from above}
}
\references{
DeBell, M. and J.A. Krosnick. (2009). Computing Weights for American National Election Study Survey Data, ANES Technical Report Series, No. nes012427. Available from:  ftp://ftp.electionstudies.org/ftp/nes/bibliography/documents/nes012427.pdf
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

anes04$caseid <- 1:length(anes04$age)

anes04$agecats <- cut(anes04$age, c(0, 25,35,45,55,65,99))
levels(anes04$agecats) <- c("age1824", "age2534", "age3544",
          "age4554", "age5564", "age6599")

marriedtarget <- c(.4, .6)

agetarg <- c(.10, .15, .17, .23, .22, .13)
names(agetarg) <- c("age1824", "age2534", "age3544",
          "age4554", "age5564", "age6599")

targets <- list(marriedtarget, agetarg)

names(targets) <- c("married", "agecats")

outsave <- anesrake(targets, anes04, caseid=anes04$caseid,
          verbose=TRUE)

write.csv(print(outsave), "outsave.csv")

summary(outsave)
}
\keyword{ ~raking }
\keyword{ ~variable selection }
\keyword{ ~weights }
