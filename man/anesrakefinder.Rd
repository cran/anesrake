\name{anesrakefinder}
\alias{anesrakefinder}
\alias{selecthighestpcts}
\alias{selectnhighest}
\title{
Function to determine what variables should be used for weighting.
}
\description{
\code{anesrake} takes a list of variables and target values and determines which variables should be used for weighting in accordance with DeBell and Krosnick, 2009.  Used as part of \code{anesrake}.
}
\usage{
anesrakefinder(inputter, dataframe, weightvec = NULL,
choosemethod = "total")
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{inputter}{
The \code{inputter} object should contain a list of all target values for the raking procedure.  Each list element in  \code{inputter}  should be a vector corresponding to the weighting targets for a single variable.  Hence, the vector enumerating the weighting targets for a variable with 2 levels should be of length 2, while a vector enumerating the weighting targets for a variable with 5 levels should be of length 5.  List elements in inputter should be named according to the variable that they will match in the corresponding dataset.  Hence, a list element enumerating the proportion of the sample that should be of each gender should be labeled "female" if the variable in  \code{dataframe} is also titled "female."

\code{inputter} elements must be vectors and can be of class numeric, or factor and must match the class of the corresponding variable in  \code{dataframe}.  Logical variables in  \code{dataframe} can be matched to a numeric vector of length 2 and ordered with the \code{TRUE} target as the first element and the  \code{FALSE} target as the second element.  Targets for factors must be labeled to match every level present in the dataframe (e.g. a variable with 2 age groups "under40" and "over40" should have elements named "under40" and "over40" respectively).  \code{anesrake} attempts to conform any unrecognized types of vectors to  \code{class(numeric)}. Weighting targets can be entered either as an N to be reached or as a percent for any given variable.  Targets can be either proportions (ideal) or the number of individuals in the population in each target category (N).  Totals of greater than 1.5 for any given list element are treated as Ns, while values of less than 1.5 are treated as percentages.
}
  \item{dataframe}{
The  \code{dataframe} command identifies a \code{data.frame} object of the data to be weighted.  The data.frame must contain all of the variables that will be used in the weighting process and those variables must have the same names as are present in the \code{inputter} list element.
}
  \item{weightvec}{
 \code{weightvec} is an optional input if some kind of base weights, stratification correction, or other sampling probability of note that should be accounted for before weighting is conducted.  If defined, \code{weightvec} must be of a length equivalent to the number of cases in the  \code{dataframe}.  If undefined, \code{weightvec} will be automatically seeded with a vector of 1s.
}
  \item{choosemethod}{
\code{choosemethod} is the method for choosing most discrepant variables.  Six options are available:  \code{choosemethod=c("total", "max", "average", "totalsquared", "maxsquared", "averagesquared")}.  If  \code{choosemethod="total"}, variable choice is determined by the sum of the differences between actual and target values for each prospective weighting variable.  If  \code{choosemethod="max"}, variable choice is determined by the largest individual difference between actual and target values for each prospective weighting variable.  If  \code{choosemethod="average"}, variable choice is determined by the mean of the differences between actual and target values for each prospective weighting variable.  If  \code{choosemethod="totalsquared"}, variable choice is determined by the sum of the squared differences between actual and target values for each prospective weighting variable.  If  \code{choosemethod="maxsquared"}, variable choice is determined by the largest squared difference between actual and target values for each prospective weighting variable (note that this is identical to  \code{choosemethod="max"} if the selection type is \code{nlim}).  If  \code{choosemethod="averagesquared"}, variable choice is determined by the mean of the squared differences between actual and target values for each prospective weighting variable. 
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
Returns a vector of variable names and discrepancies via the method chosen in \code{choosemethod}.
}
\references{
%% ~put references to the literature/web site here ~
}
\author{
Josh Pasek, Assistant Professor of Communication Studies at the University of Michigan (www.joshpasek.com).
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
\keyword{ ~variable selection }
