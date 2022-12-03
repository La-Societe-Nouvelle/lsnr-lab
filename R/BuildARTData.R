#'Build and returns all data required to the ART indicator computations.
#'
#'Returns a `list` made up of value added impacts by French branches, imported products associated coefficient,
#'Data sources and values unit. This data will be used in both BuildBranchesData and BuildDivisionsData functions.
#'
#' @param Year Considered Year.
#'
#' @return An object `list` made up of 4 elements : value added impacts by French branches,
#' imported products associated coefficient, data sources and values unit.
#' @seealso \code{\link{BuildECOData}}, \code{\link{BuildGHGData}}, \code{\link{BuildNRGData}},
#'  \code{\link{BuildBranchesData}}, \code{\link{BuildDivisionsData}}, \code{\link{FetchDataDisponibility}}.
#' @examples
#' BuildARTData(max(FetchDataDisponibility("ART"))
#' @export

source('R/InseeDataManager.R')

build_branches_nva_fpt_art = function(year) 
{
  # get branches aggregates
  branches_aggregates = get_branches_aggregates(year)

  nva_fpt_data = as.data.frame(cbind(branches_aggregates$BRANCH, branches_aggregates$NVA))

  for(i in 1:nrow(nva_fpt_data))
  {
    nva_fpt_data$GROSS_IMPACT[i] = round((as.numeric(nva_fpt_data[i,2]) / sum(as.numeric(nva_fpt_data[,2]))) * 111600, digits = 3)
    nva_fpt_data$FOOTPRINT[i] = round(100*(111600 / sum(as.numeric(nva_fpt_data[,2]))), digits = 3)
    nva_fpt_data$UNIT_FOOTPRINT[i] = "P100"
  }

  return(nva_fpt_data)
}

get_branches_imp_coef_art = function(year)
{
  branches_imp_coef = 0
  return(branches_imp_coef)
}