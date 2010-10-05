\name{wtd.hist}
\alias{wtd.hist}
\title{
Weighted Histograms
}
\description{
Produces weighted histograms by adding a "weight" option to the generic histogram function from the graphics package. The generic function hist computes a histogram of the given data values. If plot=TRUE, the resulting object of class "histogram" is plotted by plot.histogram, before it is returned.  
}
\usage{
wtd.hist(x, breaks = "Sturges",
     freq = NULL, probability = !freq,
     include.lowest = TRUE, right = TRUE,
     density = NULL, angle = 45, col = NULL, border = NULL,
     main = paste("Histogram of" , xname),
     xlim = range(breaks), ylim = NULL,
     xlab = xname, ylab,
     axes = TRUE, plot = TRUE, labels = FALSE,
     nclass = NULL, weight = NULL, ...)
}
\arguments{
  \item{x}{a vector of values for which the histogram is desired.}
  \item{breaks}{one of:
    \itemize{
      \item a vector giving the breakpoints between histogram cells,
      \item a single number giving the number of cells for the histogram,
      \item a character string naming an algorithm to compute the
      number of cells (see \sQuote{Details}),
      \item a function to compute the number of cells.
    }
    In the last three cases the number is a suggestion only.
  }
  \item{freq}{logical; if \code{TRUE}, the histogram graphic is a
    representation of frequencies, the \code{counts} component of
    the result; if \code{FALSE}, probability densities, component
    \code{density}, are plotted (so that the histogram has a total area
    of one).  Defaults to \code{TRUE} \emph{if and only if} \code{breaks} are
    equidistant (and \code{probability} is not specified).}
  \item{probability}{an \emph{alias} for \code{!freq}, for S compatibility.}
  \item{include.lowest}{logical; if \code{TRUE}, an \code{x[i]} equal to
    the \code{breaks} value will be included in the first (or last, for
    \code{right = FALSE}) bar.  This will be ignored (with a warning)
    unless \code{breaks} is a vector.}
  \item{right}{logical; if \code{TRUE}, the histogram cells are
    right-closed (left open) intervals.}
  \item{density}{the density of shading lines, in lines per inch.
    The default value of \code{NULL} means that no shading lines
    are drawn. Non-positive values of \code{density} also inhibit the
    drawing of shading lines.}
  \item{angle}{the slope of shading lines, given as an angle in
    degrees (counter-clockwise).}
  \item{col}{a colour to be used to fill the bars.
    The default of \code{NULL} yields unfilled bars.}
  \item{border}{the color of the border around the bars.  The default
    is to use the standard foreground color.}
  \item{main, xlab, ylab}{these arguments to \code{title} have useful
    defaults here.}
  \item{xlim, ylim}{the range of x and y values with sensible defaults.
    Note that \code{xlim} is \emph{not} used to define the histogram (breaks),
    but only for plotting (when \code{plot = TRUE}).}
  \item{axes}{logical.  If \code{TRUE} (default), axes are draw if the
    plot is drawn.}
  \item{plot}{logical.  If \code{TRUE} (default), a histogram is
    plotted, otherwise a list of breaks and counts is returned.  In the
    latter case, a warning is used if (typically graphical) arguments
    are specified that only apply to the \code{plot = TRUE} case.}
  \item{labels}{logical or character.  Additionally draw labels on top
    of bars, if not \code{FALSE}; see \code{plot.histogram} in the \code{graphics} package.}
  \item{nclass}{numeric (integer).  For S(-PLUS) compatibility only,
    \code{nclass} is equivalent to \code{breaks} for a scalar or
    character argument.}
  \item{weight}{numeric.  Defines a set of weights to produce a weighted histogram.  Will default to 1 for each case if no other weight is defined.}
  \item{\dots}{further arguments and graphical parameters passed to
    \code{plot.histogram} and thence to \code{title} and
    \code{axis} (if \code{plot=TRUE}).}
}
\details{
  The definition of \emph{histogram} differs by source (with
  country-specific biases). R's default with equi-spaced breaks (also
  the default) is to plot the (weighted) counts in the cells defined by
  \code{breaks}.  Thus the height of a rectangle is proportional to
  the (weighted) number of points falling into the cell, as is the area
  \emph{provided} the breaks are equally-spaced.

  The default with non-equi-spaced breaks is to give
  a plot of area one, in which the \emph{area} of the rectangles is the
  fraction of the data points falling in the cells.

  If \code{right = TRUE} (default), the histogram cells are intervals
  of the form \code{(a, b]}, i.e., they include their right-hand endpoint,
  but not their left one, with the exception of the first cell when
  \code{include.lowest} is \code{TRUE}.

  For \code{right = FALSE}, the intervals are of the form \code{[a, b)},
  and \code{include.lowest} means \sQuote{\emph{include highest}}.

  The default for \code{breaks} is \code{"Sturges"}: see
  \code{nclass.Sturges}.  Other names for which algorithms
  are supplied are \code{"Scott"} and \code{"FD"} /
  \code{"Freedman-Diaconis"} (with corresponding functions
  \code{nclass.scott} and \code{nclass.FD}).
  Case is ignored and partial matching is used.
  Alternatively, a function can be supplied which
  will compute the intended number of breaks as a function of \code{x}.
}
\value{
  an object of class \code{"histogram"} which is a list with components:
  \item{breaks}{the \eqn{n+1} cell boundaries (= \code{breaks} if that
    was a vector). These are the nominal breaks, not with the boundary fuzz.}
  \item{counts}{\eqn{n} values; for each cell, the number of
    \code{x[]} inside.}
  \item{density}{values for each bin such that the area under the histogram totals 1.
  \eqn{\hat f(x_i \omega_i)} / \eqn{f^(x[i] \omega[i])}, as estimated
    density values. If \code{all(diff(breaks) == 1)}, they are the
    relative frequencies \code{counts/n} and in general satisfy
    \eqn{\sum_i \hat f(x_i  \omega_i) (b_{i+1}-b_i) = 1} / \eqn{sum[i; f^(x[i]  \omega[i])
      (b[i+1]-b[i])] = 1}, where \eqn{b_i}{b[i]} = \code{breaks[i]}.}
  \item{intensities}{same as \code{density}. Deprecated, but retained
    for compatibility.}
  \item{mids}{the \eqn{n} cell midpoints.}
  \item{xname}{a character string with the actual \code{x} argument name.}
  \item{equidist}{logical, indicating if the distances between
    \code{breaks} are all the same.}
}
\author{
Josh Pasek, Ph.D. Candidate in Communication at Stanford University (www.joshpasek.com) was responsible for the updates to the original hist function necessary to implement weighted counts.
}
\examples{
var1 <- c(1:100)
wgt <- var1/mean(var1)
par(mfrow=c(2, 2))
wtd.hist(var1)
wtd.hist(var1, weight=wgt)
wtd.hist(var1, weight=var1)
}
\keyword{ ~dplot }
\keyword{ ~hplot }
\keyword{ ~distribution }
\keyword{ ~weights }
