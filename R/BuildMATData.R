#' @importFrom dplyr %>%
#' @importFrom tidyr pivot_longer
#'
#' @noRd

build_branches_nva_fpt_mat = function(selectedYear)
{
  # get branches aggregates -------------------------- #

  branches_aggregates = get_branches_aggregates(selectedYear)

  # fetch data --------------------------------------- #

  set=c("MF1","MF2","MF3","MF4")

  main = "https://ec.europa.eu/eurostat/api/dissemination/statistics/1.0/data/env_ac_mfa"
  filters = paste0("?geo=FR&unit=THS_T&time=",selectedYear,"&indic_env=DE&")
  mf = paste0("material=",set, collapse = "&")

  ac_mfa_data = get_eurostat_data(paste0(main,filters,mf))
  ac_mfa_data$value = as.numeric(ac_mfa_data$value)
  # sector fpt --------------------------------------- #

  sector_fpt_list = list()

  sector_fpt_list[["A"]]    = ac_mfa_data$value[ac_mfa_data$material=="MF1"]*1000 / branches_aggregates$NVA[branches_aggregates$BRANCH=="AZ"]
  sector_fpt_list[["B"]]    = sum(ac_mfa_data$value[ac_mfa_data$material %in% c("MF2","MF3","MF4")])*1000 / branches_aggregates$NVA[branches_aggregates$BRANCH=="BZ"]
  sector_fpt_list[["C-T"]]  = 0

  sector_fpt = cbind.data.frame(sector_fpt_list) %>% pivot_longer(cols = names(sector_fpt_list))
  colnames(sector_fpt) = c("SECTOR", "FOOTPRINT")

  # build nva fpt dataframe -------------------------- #

  nva_fpt_data = data.frame(BRANCH = as.character(branches_aggregates$BRANCH), NVA = as.numeric(branches_aggregates$NVA))

  branch_sector_fpt_matrix = lsnr:::MatrixMAT

  for(i in 1:nrow(nva_fpt_data))
  {
    # get sector
    branch = nva_fpt_data$BRANCH[i]
    sector = branch_sector_fpt_matrix$SECTOR[branch_sector_fpt_matrix$BRANCH==branch]

    # build values
    nva_fpt_data$GROSS_IMPACT[i] = sector_fpt$FOOTPRINT[sector_fpt$SECTOR==sector] * branches_aggregates$NVA[i]
    nva_fpt_data$UNIT_GROSS_IMPACT[i] = "T"
    nva_fpt_data$FOOTPRINT[i] = sector_fpt$FOOTPRINT[sector_fpt$SECTOR==sector]
    nva_fpt_data$UNIT_FOOTPRINT[i] = "G_CPEUR"
  }

  return(nva_fpt_data)
  # -------------------------------------------------- #
}

build_divisions_nva_fpt_mat = function(selectedYear)
{
  # get branches aggregates -------------------------- #

  divisions_aggregates = get_divisions_aggregates(selectedYear)

  # fetch data --------------------------------------- #

  set=c("MF1","MF2","MF3","MF4")

  main = "https://ec.europa.eu/eurostat/api/dissemination/statistics/1.0/data/env_ac_mfa"
  filters = paste0("?geo=FR&unit=THS_T&time=",selectedYear,"&indic_env=DE&")
  mf = paste0("material=",set, collapse = "&")

  ac_mfa_data = get_eurostat_data(paste0(main,filters,mf))

  # sector fpt --------------------------------------- #

  sector_fpt_list = list()

  # 01 -> 03 / A
  sector_fpt_list[["A"]] = sum(ac_mfa_data$value[ac_mfa_data$material %in% c("MF1")])*1000 / sum(divisions_aggregates$NVA[divisions_aggregates$CNA_ACTIVITE %in% c("01","02","03")])
  # 05 -> 08 / B
  sector_fpt_list[["B"]] = sum(ac_mfa_data$value[ac_mfa_data$material %in% c("MF2","MF3","MF4")])*1000 / sum(divisions_aggregates$NVA[divisions_aggregates$CNA_ACTIVITE %in% c("05","06","07","08")])
  # 09 -> 98 / C-T
  sector_fpt_list[["C-T"]] = 0

  sector_fpt = cbind.data.frame(sector_fpt_list) %>% pivot_longer(cols = names(sector_fpt_list))
  colnames(sector_fpt) = c("SECTOR", "FOOTPRINT")

  # build nva fpt dataframe -------------------------- #

  nva_fpt_data = data.frame(DIVISION = as.character(divisions_aggregates$CNA_ACTIVITE), NVA = as.numeric(divisions_aggregates$NVA))

  branch_sector_fpt_matrix = lsnr:::DivisionMappingMAT
  branch_sector_fpt_matrix$DIVISION[nchar(branch_sector_fpt_matrix$DIVISION)==1] = paste0("0",branch_sector_fpt_matrix$DIVISION[nchar(branch_sector_fpt_matrix$DIVISION)==1])

  for(i in 1:nrow(nva_fpt_data))
  {
    # get sector
    division = nva_fpt_data$DIVISION[i]
    sector = branch_sector_fpt_matrix$SECTOR[branch_sector_fpt_matrix$DIVISION==division]

    # build values
    nva_fpt_data$GROSS_IMPACT[i] = sector_fpt$FOOTPRINT[sector_fpt$SECTOR==sector] * divisions_aggregates$NVA[i]
    nva_fpt_data$UNIT_GROSS_IMPACT[i] = "T"
    nva_fpt_data$FOOTPRINT[i] = sector_fpt$FOOTPRINT[sector_fpt$SECTOR==sector]
    nva_fpt_data$UNIT_FOOTPRINT[i] = "G_CPEUR"
  }

  return(nva_fpt_data)
  # -------------------------------------------------- #
}

get_branches_imp_coef_mat = function(selectedYear)
{
  # fetch data

  eurostat_mfa_data = get_eurostat_data(paste0("https://ec.europa.eu/eurostat/api/dissemination/statistics/1.0/data/env_ac_mfa?geo=FR&geo=EU27_2020&unit=THS_T&time=",selectedYear,"&indic_env=DE&material=TOTAL"))

   # domestic production

  eurostat_nama_data = get_eurostat_data(paste0("https://ec.europa.eu/eurostat/api/dissemination/statistics/1.0/data/nama_10_a64?geo=FR&geo=EU27_2020&unit=CP_MEUR&time=",selectedYear,"&nace_r2=TOTAL&na_item=B1G"))

  fpt_fra =  eurostat_mfa_data$value[eurostat_mfa_data$geo=="FR"] / eurostat_nama_data$value[eurostat_nama_data$geo=="FR"]
  fpt_euu =  eurostat_mfa_data$value[eurostat_mfa_data$geo=="EU27_2020"] / eurostat_nama_data$value[eurostat_nama_data$geo=="EU27_2020"]

  branches_imp_coef = fpt_euu / fpt_fra

  return(branches_imp_coef)
}
