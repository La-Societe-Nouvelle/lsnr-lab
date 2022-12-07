#'Build and returns all data required to the SOC indicator computations.
#'
#'Returns a `list` made up of value added impacts by French branches, imported products associated coefficient,
#'Data sources and values unit. This data will be used in both BuildBranchesData and BuildDivisionsData functions.
#'
#' @param Year Considered year.
#'
#' @return An object `list` made up of 4 elements : value added impacts by French branches,
#' imported products associated coefficient, data sources and values unit.
#' @seealso \code{\link{BuildECOData}}, \code{\link{BuildGHGData}},
#'  \code{\link{BuildBranchesData}}, \code{\link{BuildDivisionsData}}, \code{\link{FetchDataDisponibility}}.
#' @examples
#' BuildSOCData(max(FetchDataDisponibility("SOC"))
#' @export

source('R/InseeDataManager.R')

build_branches_nva_fpt_soc = function(year) 
{
  # get branches aggregates -------------------------- #

  branches_aggregates = get_branches_aggregates(year)

  # fetch data --------------------------------------- #

  wd = getwd()
  ess_data = read.csv(paste0(wd,"/datasets/","SOC_DATA.csv"), header=T, sep=";")

  # build nva fpt dataframe -------------------------- #

  nva_fpt_data = as.data.frame(cbind(branches_aggregates$BRANCH, branches_aggregates$NVA))
  colnames(nva_fpt_data) = c("BRANCH", "NVA")

  for(i in 1:nrow(nva_fpt_data))
  {    
    # build values
    nva_fpt_data$GROSS_IMPACT[i] = ess_data$FOOTPRINT[i] * branches_aggregates$NVA[i]
    nva_fpt_data$FOOTPRINT[i] = ess_data$FOOTPRINT[i]
    nva_fpt_data$UNIT_FOOTPRINT[i] = ess_data$UNIT_FOOTPRINT[i]
  }

  return(nva_fpt_data)
  # -------------------------------------------------- #
}

get_branches_imp_coef_soc = function(year)
{
  branches_imp_coef = 0
  return(branches_imp_coef)
}