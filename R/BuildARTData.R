#' @importFrom httr GET
#' @importFrom dplyr %>%
#' @importFrom dplyr filter
#' @importFrom jsonlite fromJSON
#'
#' @noRd

# Donnees exploitees
#   - Valeur ajoutee brute de l'Artisanat (donnees issues de la DGE - Chiffres clefs)
#
# /!\ l'empreinte est identique pour l'ensemble des branches
#
# Les importations sont considereees comme nullement contributrices aux metiers d'art et aux savoir-faire (i.e. coef = 0)

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
    stop(paste0("Donnees indisponibles pour ",selectedYear))
  })

  if(nrow(dge_data) == 0){
    stop(paste0("Donnees indisponibles pour ",selectedYear," (Indicateur ART)"))
  }

  # sector fpt --------------------------------------- #

  all_sector_fpt = round((dge_data$value / sum(as.numeric(branches_aggregates$NVA + branches_aggregates$CFC))), digits = 3)
  # NVA + CFC -> Valeur ajoutee brute disponible pour l'Artisanat

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
      filter(year == selectedYear)

  }, error = function(e) {
    stop(paste0("Donnees indisponibles pour ",selectedYear))
  })

  if(nrow(dge_data) == 0){
    stop(paste0("Donnees indisponibles pour ",selectedYear," (Indicateur ART)"))
  }

  # sector fpt --------------------------------------- #

  all_sector_fpt = round((dge_data$value / sum(as.numeric(divisions_aggregates$NVA + divisions_aggregates$CFC))), digits = 3)

  # build nva fpt dataframe -------------------------- #

  nva_fpt_data = data.frame(DIVISION = as.character(divisions_aggregates$CNA_ACTIVITE), NVA = as.numeric(divisions_aggregates$NVA))

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
