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
#'  \code{\link{BuildBranchesData}}, \code{\link{BuildDivisionsData}}, \code{\link{FetchDataAvailability}}.
#' @examples
#' BuildSOCData(max(FetchDataAvailability("SOC"))
#' @noRd

build_branches_nva_fpt_soc = function(selectedYear)
{
  # get branches aggregates -------------------------- #

  branches_aggregates = get_branches_aggregates(selectedYear)

  # fetch data --------------------------------------- #

<<<<<<< HEAD
  ess_data = lsnr:::SOC_DATA
=======
  tryCatch({
    ess_data = as.data.frame(cbind(branches_aggregates$BRANCH))
    colnames(ess_data) = c("BRANCH")
    # loop to fetch data for each branch
    for(i in 1:nrow(branches_aggregates)) 
    {
      res = GET(paste0("https://api.lasocietenouvelle.org/serie/SIRENE_ESS_LEGALUNITS_P100_FRA_BRANCH?area=FRA&code=",ess_data$BRANCH[i]))
      ess_branch_data = fromJSON(rawToChar(res$content))$data %>% filter(year == selectedYear)
      ess_data$NVA_FPT[i] = ess_branch_data$value
    }
  }, error = function(e) {
    stop(paste0("Données indisponibles pour ",selectedYear))
  })
>>>>>>> 861152fe4597143143ebda2ccc7843a80d4f6b35

  # build nva fpt dataframe -------------------------- #

  nva_fpt_data = as.data.frame(cbind(branches_aggregates$BRANCH, branches_aggregates$NVA))
  colnames(nva_fpt_data) = c("BRANCH", "NVA")

  for(i in 1:nrow(nva_fpt_data))
  {
    # build values
    nva_fpt_data$GROSS_IMPACT[i] = ess_data$NVA_FPT[i] * branches_aggregates$NVA[i]
    nva_fpt_data$UNIT_GROSS_IMPACT[i] = "CPMEUR"
    nva_fpt_data$FOOTPRINT[i] = ess_data$NVA_FPT[i]*100
    nva_fpt_data$UNIT_FOOTPRINT[i] = ess_data$UNIT_FOOTPRINT[i]
  }

  return(nva_fpt_data)
  # -------------------------------------------------- #
}

build_divisions_nva_fpt_soc = function(selectedYear)
{
  # get branches aggregates -------------------------- #

  divisions_aggregates() = get_divisions_aggregates(selectedYear)

  # fetch data --------------------------------------- #

  tryCatch({
    # for loop to fetch data for each branch
    res = GET("https://api.lasocietenouvelle.org/serie/SIRENE_ESS_LEGALUNITS_P100_FRA_BRANCH")
    dge_data = fromJSON(rawToChar(res$content))$data %>%
      filter(dge_data$year == selectedYear)
  }, error = function(e) {
    stop(paste0("Données indisponibles pour ",selectedYear))
  })

  # build nva fpt dataframe -------------------------- #

  nva_fpt_data = as.data.frame(cbind(divisions_aggregates$BRANCH, divisions_aggregates$NVA))
  colnames(nva_fpt_data) = c("DIVISION", "NVA")

  for(i in 1:nrow(nva_fpt_data))
  {
    # build values
    nva_fpt_data$GROSS_IMPACT[i] = ess_data$FOOTPRINT[i] * divisions_aggregates$NVA[i]
    nva_fpt_data$UNIT_GROSS_IMPACT[i] = "CPMEUR"
    nva_fpt_data$FOOTPRINT[i] = ess_data$FOOTPRINT[i]
    nva_fpt_data$UNIT_FOOTPRINT[i] = ess_data$UNIT_FOOTPRINT[i]
  }

  return(nva_fpt_data)
  # -------------------------------------------------- #
}

get_branches_imp_coef_soc = function(selectedYear)
{
  branches_imp_coef = 0
  return(branches_imp_coef)
}
