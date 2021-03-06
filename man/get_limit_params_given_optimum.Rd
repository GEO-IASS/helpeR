% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/get_limit_params_given_optimum.R
\name{get_limit_params_given_optimum}
\alias{get_limit_params_given_optimum}
\title{Find the optimums parameters given it occurs in the limits.}
\usage{
get_limit_params_given_optimum(x, params = NULL, optcrit = NULL)
}
\arguments{
\item{x}{A data frame with one column for each parameter and 
the optimization criteria.}

\item{params}{The column name(s) of the parameters. 
Optional, if \code{NULL} it is assumed that the parameters are 
in the columns \code{1:(ncol(x)-1)} and the optimization criteria 
in the column \code{ncol(x)}.   
are parameter columns and the last one is the column holding 
the values of the optimization criteria.}

\item{optcrit}{The column name of the optimization criteria.
Optiona, if \code{NULL} the last column is used.}
}
\value{
List with the optimums parameters given it occures at 
the limits of the parameters.
}
\description{
Grid-search is often used for parameter selection.
An optimization criteria is calculated  for pre-defined 
modl parameters (e.g. sigma/gamma and C for the SVM).
When the optimum occurs at the limits of the parameters it is 
possible that extending the parameter range could improve the 
model performance. This functions returns the values of the parameter
limits given the optimum occurs at the limits.
}
\examples{
{
x <- expand.grid(p1=1:3,
                     p2=2:5)
x$oc <- rnorm(nrow(x))
x[8, 3] <- max(x$oc) + 1 # no limit
get_limit_params_given_optimum(x)
x[4, 3] <- max(x$oc) + 1 # lower limit of p1
get_limit_params_given_optimum(x)
x[12, 3] <- max(x$oc) + 1 # upper limits of p1 and p2
get_limit_params_given_optimum(x)
}
}

