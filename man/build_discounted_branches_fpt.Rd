% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/BuildDiscountedBranchesData.R
\name{build_discounted_branches_fpt}
\alias{build_discounted_branches_fpt}
\title{Compute dicounted macroeconomic values of societal footprints by 'NACE' branches (38).}
\usage{
build_discounted_branches_fpt(
  indicator,
  startYear,
  endYear = as.numeric(format(Sys.Date(), "\%Y")) - 1,
  verbose = T
)
}
\arguments{
\item{indicator}{requested non-financial dimension.}

\item{startYear}{year of original data.}

\item{endYear}{year of discounted data.}

\item{verbose}{TRUE by default, FALSE refers to the silent mode}
}
\value{
A table of macroeconomic footprint values by economic activities branch.
}
\description{
Compute dicounted macroeconomic values of societal footprints by 'NACE' branches (38).
}
\details{
This function calls the latest available footprints for branches
 and discount them in specified-year euro terms.
}
\examples{

build_discounted_branches_fpt("GHG",2019,2022)

}
\seealso{
\code{\link{build_divisions_fpt}}, \code{\link{get_indicator_list}}.
}
