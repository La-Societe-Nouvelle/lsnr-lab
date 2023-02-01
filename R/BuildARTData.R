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
#'  \code{\link{BuildBranchesData}}, \code{\link{BuildDivisionsData}}, \code{\link{FetchDataAvailability}}.
#' @examples
#' BuildARTData(max(FetchDataAvailability("ART"))
#' @noRd


build_branches_nva_fpt_art = function(selectedYear)
{
  # get branches aggregates -------------------------- #

  branches_aggregates = get_branches_aggregates(selectedYear)

  # fetch data --------------------------------------- #

  tryCatch({

    res = GET("https://api.lasocietenouvelle.org/serie/MACRO_CRAFTEDNVA_DGE__FRA_CPMEUR")

    dge_data = fromJSON(rawToChar(res$content))$data %>%
      filter(year == selectedYear)
    
  }, error = function(e) {
    stop(paste0("Données indisponibles pour ",selectedYear))
  })

  # sector fpt --------------------------------------- #

  all_sector_fpt = round((dge_data$value / sum(as.numeric(branches_aggregates$NVA + branches_aggregates$CFC))), digits = 3)

  # build nva fpt dataframe -------------------------- #

  nva_fpt_data = as.data.frame(cbind(branches_aggregates$BRANCH, branches_aggregates$NVA))
  colnames(nva_fpt_data) = c("BRANCH", "NVA")

  for(i in 1:nrow(nva_fpt_data))
  {
    nva_fpt_data$GROSS_IMPACT[i] = round((as.numeric(nva_fpt_data[i,2]) * all_sector_fpt), digits = 0)
    nva_fpt_data$UNIT_GROSS_IMPACT[i] = "CPMEUR"
    nva_fpt_data$FOOTPRINT[i] = round(all_sector_fpt*100, digits = 1)
    nva_fpt_data$UNIT_FOOTPRINT[i] = "P100"
  }

  return(nva_fpt_data)
  # -------------------------------------------------- #
}

build_divisions_nva_fpt_art = function(selectedYear)
{
  # get divisions aggregates -------------------------- #

  divisions_aggregates = get_divisions_aggregates(selectedYear)

  # fetch data --------------------------------------- #

  tryCatch({

    res = GET("https://api.lasocietenouvelle.org/serie/MACRO_CRAFTEDNVA_DGE__FRA_CPMEUR")

    dge_data = fromJSON(rawToChar(res$content))$data %>%
      filter(dge_data$year == selectedYear)
    
  }, error = function(e) {
    stop(paste0("Données indisponibles pour ",selectedYear))
  })

  # sector fpt --------------------------------------- #

  all_sector_fpt = round((dge_data$value / sum(as.numeric(divisions_aggregates$NVA))), digits = 3)

  # build nva fpt dataframe -------------------------- #

  nva_fpt_data = as.data.frame(cbind(divisions_aggregates$DIVISION, divisions_aggregates$NVA))
  colnames(nva_fpt_data) = c("DIVISION", "NVA")

  for(i in 1:nrow(nva_fpt_data))
  {
    nva_fpt_data$GROSS_IMPACT[i] = round((as.numeric(nva_fpt_data[i,2]) * all_sector_fpt), digits = 0)
    nva_fpt_data$UNIT_GROSS_IMPACT[i] = "CPMEUR"
    nva_fpt_data$FOOTPRINT[i] = round(all_sector_fpt*100, digits = 1)
    nva_fpt_data$UNIT_FOOTPRINT[i] = "P100"
  }

  return(nva_fpt_data)
  # -------------------------------------------------- #
}

get_branches_imp_coef_art = function(selectedYear)
{
  branches_imp_coef = 0
  return(branches_imp_coef)
}
