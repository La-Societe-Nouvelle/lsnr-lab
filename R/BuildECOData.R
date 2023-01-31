#'Build and returns all data required to the ECO indicator computations.
#'
#'Returns a `list` made up of value added impacts by French branches, imported products associated coefficient,
#'Data sources and values unit. This data will be used in both BuildBranchesData and BuildDivisionsData functions.
#'
#' @param Year Considered year.
#'
#' @return An object `list` made up of 4 elements : value added impacts by French branches,
#' imported products associated coefficient, data sources and values unit.
#' @seealso \code{\link{BuildGHGData}}, \code{\link{BuildNRGData}},
#'  \code{\link{BuildBranchesData}}, \code{\link{BuildDivisionsData}}, \code{\link{FetchDataAvailability}}.
#' @examples
#' BuildECOData(max(FetchDataAvailability("ECO"))
#' @noRd

build_branches_nva_fpt_eco = function(selectedYear)
{
  # get branches aggregates -------------------------- #

  branches_aggregates = get_branches_aggregates(selectedYear)

  # build nva fpt dataframe -------------------------- #

  nva_fpt_data = as.data.frame(cbind(branches_aggregates$BRANCH, branches_aggregates$NVA))
  colnames(nva_fpt_data) = c("BRANCH", "NVA")

  for(i in 1:nrow(nva_fpt_data))
  {
    # build values
    nva_fpt_data$GROSS_IMPACT[i] = branches_aggregates$NVA[i]
    nva_fpt_data$UNIT_GROSS_IMPACT[i] = "CPMEUR"
    nva_fpt_data$FOOTPRINT[i] = 100.0
    nva_fpt_data$UNIT_FOOTPRINT[i] = "P100"
  }

  return(nva_fpt_data)
  # -------------------------------------------------- #
}

build_divisions_nva_fpt_eco = function(selectedYear)
{
  # get divisions aggregates -------------------------- #

  divisions_aggregates = get_divisions_aggregates(selectedYear)

  # build nva fpt dataframe -------------------------- #

  nva_fpt_data = as.data.frame(cbind(divisions_aggregates$DIVISION, divisions_aggregates$NVA))
  colnames(nva_fpt_data) = c("DIVISION", "NVA")

  for(i in 1:nrow(nva_fpt_data))
  {
    # build values
    nva_fpt_data$GROSS_IMPACT[i] = divisions_aggregates$NVA[i]
    nva_fpt_data$UNIT_GROSS_IMPACT[i] = "CPMEUR"
    nva_fpt_data$FOOTPRINT[i] = 100.0
    nva_fpt_data$UNIT_FOOTPRINT[i] = "P100"
  }

  return(nva_fpt_data)
  # -------------------------------------------------- #
}

get_branches_imp_coef_eco = function(selectedYear)
{
  branches_imp_coef = 0
  return(branches_imp_coef)
}
