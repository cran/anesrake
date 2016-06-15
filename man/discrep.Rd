\name{discrep}
\alias{discrep}
\alias{discrep.default}
\alias{discrep.factor}
\alias{discrep.logical}
\alias{discrep.numeric}
\title{
Function to determine the discrepancy for each level of a variable from targets.
}
\description{
Finds the discrepancy between the proportion of data in each level of a weighted vector and a set of targets for each level of that same vector.  Used as part of \code{anesrake}.
}
\usage{
discrep(datavec, targetvec, weightvec)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{datavec}{
Vector of values for a particular variable.
}
  \item{targetvec}{
Vector of targets with a single item per level of that variable.
}
  \item{weightvec}{
Weighting vector to be applied to \code{datavec}.
}
}
\value{
Vector of discrepancies at each level.
}
\author{
Josh Pasek, Assistant Professor of Communication Studies at the University of Michigan (www.joshpasek.com).
}
