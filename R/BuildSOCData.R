#' @importFrom httr GET
#' @importFrom jsonlite fromJSON
#' @importFrom dplyr %>%
#' @importFrom dplyr filter
#' @noRd

build_branches_nva_fpt_soc = function(selectedYear)
{
  # get branches aggregates -------------------------- #

  branches_aggregates = get_branches_aggregates(selectedYear)

  # fetch data --------------------------------------- #

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

  if("NVA_FPT" %in% names(ess_data) == F){
    stop(paste0("Données indisponibles pour ",selectedYear," (Indicateur SOC)"))
  }

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

  divisions_aggregates = get_divisions_aggregates(selectedYear)
  branches_aggregates = get_branches_aggregates(selectedYear)

  # fetch data --------------------------------------- #

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

  if("NVA_FPT" %in% names(ess_data) == F){
    stop(paste0("Données indisponibles pour ",selectedYear," (Indicateur SOC)"))
  }

  # build nva fpt dataframe -------------------------- #

  nva_fpt_data = data.frame(DIVISION = as.character(divisions_aggregates$CNA_ACTIVITE), NVA = as.numeric(divisions_aggregates$NVA))

  mapping = lsnr:::Divisions

  for(i in 1:nrow(nva_fpt_data))
  {
    # build values
    line_ess = which(ess_data$BRANCH == mapping$BRANCH[mapping$DIVISION==nva_fpt_data$DIVISION[i]])
    nva_fpt_data$GROSS_IMPACT[i] = ess_data$NVA_FPT[line_ess] * 100 * divisions_aggregates$NVA[i]
    nva_fpt_data$UNIT_GROSS_IMPACT[i] = "CPMEUR"
    nva_fpt_data$FOOTPRINT[i] = ess_data$NVA_FPT[line_ess] * 100
    nva_fpt_data$UNIT_FOOTPRINT[i] = ess_data$UNIT_FOOTPRINT[line_ess]
  }

  return(nva_fpt_data)
  # -------------------------------------------------- #
}

get_branches_imp_coef_soc = function(selectedYear)
{
  branches_imp_coef = 0
  return(branches_imp_coef)
}
