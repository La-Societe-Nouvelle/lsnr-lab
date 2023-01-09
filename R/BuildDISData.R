#'Build and returns all data required to the DIS indicator computations.
#'
#'Returns a `list` made up of value added impacts by French branches, imported products associated coefficient,
#'Data sources and values unit. This data will be used in both BuildBranchesData and BuildDivisionsData functions.
#'
#' @param Year Considered year.
#'
#' @importFrom eurostat get_eurostat
#' @return An object `list` made up of 4 elements : value added impacts by French branches,
#' imported products associated coefficient, data sources and values unit.
#' @seealso \code{\link{BuildECOData}}, \code{\link{BuildGHGData}},
#'  \code{\link{BuildBranchesData}}, \code{\link{BuildDivisionsData}}, \code{\link{FetchDataDisponibility}}.
#' @examples
#' BuildDISData(max(FetchDataDisponibility("DIS"))
#' @export

source('R/InseeDataManager.R')

build_branches_nva_fpt_dis = function(year) 
{
  # get branches aggregates -------------------------- #

  branches_aggregates = get_branches_aggregates(year)

  # fetch data --------------------------------------- #

  eurostat_data = get_eurostat("ilc_di12", time_format = "date", filters = list(geo = c("FR"), time = Year))
  
  # build nva fpt dataframe -------------------------- #

  nva_fpt_data = as.data.frame(cbind(branches_aggregates$BRANCH, branches_aggregates$NVA))
  colnames(nva_fpt_data) = c("BRANCH", "NVA")

  for(i in 1:nrow(nva_fpt_data))
  {
    nva_fpt_data$GROSS_IMPACT[i] = eurostat_data$values[0]
    nva_fpt_data$FOOTPRINT[i] = eurostat_data$values[0]
    nva_fpt_data$UNIT_FOOTPRINT[i] = "I100"
  }

  return(nva_fpt_data)
  # -------------------------------------------------- #
}

build_divisions_nva_fpt_dis = function(year) 
{
  # get divisions aggregates -------------------------- #

  divisions_aggregates = get_divisions_aggregates(year)

  # fetch data --------------------------------------- #

  eurostat_data = get_eurostat("ilc_di12", time_format = "date", filters = list(geo = c("FR"), time = Year))
  
  # build nva fpt dataframe -------------------------- #

  nva_fpt_data = as.data.frame(cbind(divisions_aggregates$DIVISION, divisions_aggregates$NVA))
  colnames(nva_fpt_data) = c("DIVISION", "NVA")

  for(i in 1:nrow(nva_fpt_data))
  {
    nva_fpt_data$GROSS_IMPACT[i] = eurostat_data$values[0]
    nva_fpt_data$FOOTPRINT[i] = eurostat_data$values[0]
    nva_fpt_data$UNIT_FOOTPRINT[i] = "I100"
  }

  return(nva_fpt_data)
  # -------------------------------------------------- #
}

get_branches_imp_coef_dis = function(year)
{
  # get dis data
  eurostat_data = get_eurostat("ilc_di12", time_format = "date", filters = list(geo = c("FR","EU28"), time = year))
  dis_fra =  eurostat_data$values[eurostat_data$geo=="FR"]
  dis_euu =  eurostat_data$values[eurostat_data$geo=="EU28"]
  branches_imp_coef = dis_euu / dis_fra
  return(branches_imp_coef)
}