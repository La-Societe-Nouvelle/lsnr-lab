#'Build and returns all data required to the NRG indicator computations.
#'
#'Returns a `list` made up of value added impacts by French branches, imported products associated coefficient,
#'Data sources, values unit and value added impacts by French divisions. This data will be used in both BuildBranchesData and BuildDivisionsData functions.
#'
#' @param Year Considered year.
#'
#' @return An object `list` made up of 5 elements : value added impacts by French branches,
#' imported products associated coefficient, data sources, values unit and value added impacts by French divisions.
#' @seealso \code{\link{BuildECOData}}, \code{\link{BuildGHGData}},
#'  \code{\link{BuildBranchesData}}, \code{\link{BuildDivisionsData}}, \code{\link{FetchDataAvailability}}.
#' @examples
#' BuildNRGData(max(FetchDataAvailability("NRG"))
#' @noRd

build_branches_nva_fpt_nrg = function(selectedYear)
{
  # -------------------------------------------------- #

  branches = lsnr:::Branches

  # get branches aggregates -------------------------- #

  branches_aggregates = get_branches_aggregates(selectedYear)

  # fetch data --------------------------------------- #

  set=c("A","A01","A02","A03","B","C","C10-C12","C13-C15","C16","C17","C18","C19","C20","C21","C22","C23","C24","C25","C26","C27","C28","C29","C30",
         "C31_C32","C33","CH_INV_PA","D","E","E36","E37-E39","ENV","F","G","G45","G46","G47","H","H49","H50","H51","H52","H53","HH", "HH_HEAT","HH_OTH",
         "HH_TRA","I","J","J58","J59_J60","J61","J62_J63","K","K64","K65","K66","L","L68A","M","M69_M70","M71","M72","M73","M74_M75","N","N77","N78","N79",
         "N80-N82","NRG_FLOW","O","P","Q","Q86","Q87_Q88","R","R90-R92","R93","ROW_ACT","S","S94","S95","S96","SD_SU","T","TOTAL","U")

  main = "https://ec.europa.eu/eurostat/api/dissemination/statistics/1.0/data/env_ac_pefasu"
  filters = paste0("?geo=FR&unit=TJ&time=",selectedYear,"&stk_flow=SUP&prod_nrg=R30&")
  nace = paste0("nace_r2=",set, collapse = "&")

  ac_pefasu_data = get_eurostat_data(paste0(main,filters,nace))

  # sector fpt --------------------------------------- #

  sector_fpt_list = list()

  for (i in 1:nrow(branches)) {
    code_nace = branches$NACE_R2[i]
    if (code_nace %in% ac_pefasu_data$nace_r2) {
      sector_fpt_list[[code_nace]] = ac_pefasu_data$values[ac_pefasu_data$nace_r2==code_nace] / branches_aggregates$NVA[i]
    }
  }

  # CC / C16-C18
  sector_fpt_list[["C16-C18"]] = sum(ac_pefasu_data$values[ac_pefasu_data$nace_r2 %in% c("C16","C17","C18")]) / branches_aggregates$NVA[branches_aggregates$BRANCH=="CC"]
  # CG / C22_C23
  sector_fpt_list[["C22_C23"]] = sum(ac_pefasu_data$values[ac_pefasu_data$nace_r2 %in% c("C22","C23")]) / branches_aggregates$NVA[branches_aggregates$BRANCH=="CG"]
  # CH / C24_C25
  sector_fpt_list[["C24_C25"]] = sum(ac_pefasu_data$values[ac_pefasu_data$nace_r2 %in% c("C24","C25")]) / branches_aggregates$NVA[branches_aggregates$BRANCH=="CH"]
  # CL / C29_C30
  sector_fpt_list[["C29_C30"]] = sum(ac_pefasu_data$values[ac_pefasu_data$nace_r2 %in% c("C29","C30")]) / branches_aggregates$NVA[branches_aggregates$BRANCH=="CL"]
  # CM / C31-C33
  sector_fpt_list[["C31-C33"]] = sum(ac_pefasu_data$values[ac_pefasu_data$nace_r2 %in% c("C31_C32","C33")]) / branches_aggregates$NVA[branches_aggregates$BRANCH=="CM"]
  # JA / J58-J60
  sector_fpt_list[["J58-J60"]] = sum(ac_pefasu_data$values[ac_pefasu_data$nace_r2 %in% c("J58","J59_J60")]) / branches_aggregates$NVA[branches_aggregates$BRANCH=="JA"]
  # MA / M69-M71
  sector_fpt_list[["M69-M71"]] = sum(ac_pefasu_data$values[ac_pefasu_data$nace_r2 %in% c("M69_M70","M71")]) / branches_aggregates$NVA[branches_aggregates$BRANCH=="MA"]
  # MC / M73-M75
  sector_fpt_list[["M73-M75"]] = sum(ac_pefasu_data$values[ac_pefasu_data$nace_r2 %in% c("M73","M74_M75")]) / branches_aggregates$NVA[branches_aggregates$BRANCH=="MC"]
  # TZ
  sector_fpt_list[["T"]] = 0

  sector_fpt = cbind.data.frame(sector_fpt_list) %>% pivot_longer(cols = names(sector_fpt_list))
  colnames(sector_fpt) = c("SECTOR", "FOOTPRINT")

  # build nva fpt dataframe -------------------------- #

  nva_fpt_data = as.data.frame(cbind(branches_aggregates$BRANCH, branches_aggregates$NVA))
  colnames(nva_fpt_data) = c("BRANCH", "NVA")

  branch_sector_fpt_matrix = lsnr:::MatrixNRG

  for(i in 1:nrow(nva_fpt_data))
  {
    # get sector
    branch = nva_fpt_data$BRANCH[i]
    sector = branch_sector_fpt_matrix$SECTOR[branch_sector_fpt_matrix$BRANCH==branch]

    # build values
    nva_fpt_data$GROSS_IMPACT[i] = sector_fpt$FOOTPRINT[sector_fpt$SECTOR==sector]*1000 * branches_aggregates$NVA[i]
    nva_fpt_data$FOOTPRINT[i] = sector_fpt$FOOTPRINT[sector_fpt$SECTOR==sector]*1000
    nva_fpt_data$UNIT_FOOTPRINT[i] = "KJ_CPEUR"
  }

  return(nva_fpt_data)
  # -------------------------------------------------- #
}

build_divisions_nva_fpt_nrg = function(selectedYear)
{
  # -------------------------------------------------- #

  branches = lsnr:::Branches
  divisions = lsnr:::Divisions

  # get branches aggregates -------------------------- #

  branches_aggregates = get_branches_aggregates(selectedYear)
  divisions_aggregates = get_divisions_aggregates(selectedYear)

  # fetch data --------------------------------------- #

  set=c("A","A01","A02","A03","B","C","C10-C12","C13-C15","C16","C17","C18","C19","C20","C21","C22","C23","C24","C25","C26","C27","C28","C29","C30",
        "C31_C32","C33","CH_INV_PA","D","E","E36","E37-E39","ENV","F","G","G45","G46","G47","H","H49","H50","H51","H52","H53","HH", "HH_HEAT","HH_OTH",
        "HH_TRA","I","J","J58","J59_J60","J61","J62_J63","K","K64","K65","K66","L","L68A","M","M69_M70","M71","M72","M73","M74_M75","N","N77","N78","N79",
        "N80-N82","NRG_FLOW","O","P","Q","Q86","Q87_Q88","R","R90-R92","R93","ROW_ACT","S","S94","S95","S96","SD_SU","T","TOTAL","U")

  main = "https://ec.europa.eu/eurostat/api/dissemination/statistics/1.0/data/env_ac_pefasu"
  filters = paste0("?geo=FR&unit=TJ&time=",selectedYear,"&stk_flow=SUP&prod_nrg=R30&")
  nace = paste0("nace_r2=",set, collapse = "&")

  ac_pefasu_data = get_eurostat_data(paste0(main,filters,nace))

  # sector fpt --------------------------------------- #

  sector_fpt_list = list()

  for (i in 1:nrow(divisions)) {
    code_nace = divisions$NACE_R2[i]
    if (code_nace %in% ac_pefasu_data$nace_r2) {
      sector_fpt_list[[code_nace]] = ac_pefasu_data$values[ac_pefasu_data$nace_r2==code_nace] / divisions_aggregates$NVA[i]
    }
  }

  # 05 -> 09 / B
  sector_fpt_list[["B"]] = sum(ac_pefasu_data$values[ac_pefasu_data$nace_r2 %in% c("B")]) / divisions_aggregates$NVA[divisions_aggregates$DIVISION %in% c("05","06","07","08","09")]
  # 10 -> 12 / C10-C12
  sector_fpt_list[["C10-C12"]] = sum(ac_pefasu_data$values[ac_pefasu_data$nace_r2 %in% c("C10-C12")]) / divisions_aggregates$NVA[divisions_aggregates$DIVISION %in% c("10","11","12")]
  # 13 -> 15 / C13-C15
  sector_fpt_list[["C13-C15"]] = sum(ac_pefasu_data$values[ac_pefasu_data$nace_r2 %in% c("C13-C15")]) / divisions_aggregates$NVA[divisions_aggregates$DIVISION %in% c("13","14","15")]
  # 31 -> 32 / C31_C32
  sector_fpt_list[["C31_C32"]] = sum(ac_pefasu_data$values[ac_pefasu_data$nace_r2 %in% c("C31_C32")]) / divisions_aggregates$NVA[divisions_aggregates$DIVISION %in% c("31","32")]
  # 37 -> 39 / E37-E39
  sector_fpt_list[["E37-E39"]] = sum(ac_pefasu_data$values[ac_pefasu_data$nace_r2 %in% c("E37-E39")]) / divisions_aggregates$NVA[divisions_aggregates$DIVISION %in% c("37","38","39")]
  # 41 -> 43 / F
  sector_fpt_list[["F"]] = sum(ac_pefasu_data$values[ac_pefasu_data$nace_r2 %in% c("F")]) / divisions_aggregates$NVA[divisions_aggregates$DIVISION %in% c("41","42","43")]
  # 59 -> 60 / J59_J60
  sector_fpt_list[["J59_J60"]] = sum(ac_pefasu_data$values[ac_pefasu_data$nace_r2 %in% c("J59_J60")]) / divisions_aggregates$NVA[divisions_aggregates$DIVISION %in% c("59","60")]
  # 59 -> 60 / J62_J63
  sector_fpt_list[["J62_J63"]] = sum(ac_pefasu_data$values[ac_pefasu_data$nace_r2 %in% c("J62_J63")]) / divisions_aggregates$NVA[divisions_aggregates$DIVISION %in% c("62","63")]
  # 69 -> 70 / M69_M70
  sector_fpt_list[["M69_M70"]] = sum(ac_pefasu_data$values[ac_pefasu_data$nace_r2 %in% c("M69_M70")]) / divisions_aggregates$NVA[divisions_aggregates$DIVISION %in% c("69","70")]
  # 74 -> 75 / M74_M75
  sector_fpt_list[["M74_M75"]] = sum(ac_pefasu_data$values[ac_pefasu_data$nace_r2 %in% c("M74_M75")]) / divisions_aggregates$NVA[divisions_aggregates$DIVISION %in% c("74","75")]
  # 80 -> 82 / N80-N82
  sector_fpt_list[["N80-N82"]] = sum(ac_pefasu_data$values[ac_pefasu_data$nace_r2 %in% c("N80-N82")]) / divisions_aggregates$NVA[divisions_aggregates$DIVISION %in% c("80","81","82")]
  # 87 -> 88 / Q87_Q88
  sector_fpt_list[["Q87_Q88"]] = sum(ac_pefasu_data$values[ac_pefasu_data$nace_r2 %in% c("Q87_Q88")]) / divisions_aggregates$NVA[divisions_aggregates$DIVISION %in% c("87","88")]
  # 90 -> 92 / R90-R92
  sector_fpt_list[["R90-R92"]] = sum(ac_pefasu_data$values[ac_pefasu_data$nace_r2 %in% c("R90-R92")]) / divisions_aggregates$NVA[divisions_aggregates$DIVISION %in% c("90","91","92")]
  # 97 -> 98 / T
  sector_fpt_list[["T"]] = sum(ac_pefasu_data$values[ac_pefasu_data$nace_r2 %in% c("T")]) / divisions_aggregates$NVA[divisions_aggregates$DIVISION %in% c("97","98")]

  sector_fpt = cbind.data.frame(sector_fpt_list) %>% pivot_longer(cols = names(sector_fpt_list))
  colnames(sector_fpt) = c("SECTOR", "FOOTPRINT")

  # build nva fpt dataframe -------------------------- #

  nva_fpt_data = data.frame(DIVISION = as.character(divisions_aggregates$CNA_ACTIVITE), NVA = as.numeric(divisions_aggregates$NVA))

  division_sector_fpt_matrix = lsnr:::DivisionMappingNRG

  for(i in 1:nrow(nva_fpt_data))
  {
    # get sector
    division = nva_fpt_data$BRANCH[i]
    sector = division_sector_fpt_matrix$SECTOR[division_sector_fpt_matrix$DIVISION==division]

    # build values
    nva_fpt_data$GROSS_IMPACT[i] = sector_fpt$FOOTPRINT[sector_fpt$SECTOR==sector]*1000 * branches_aggregates$NVA[i]
    nva_fpt_data$UNIT_GROSS_IMPACT[i] = "MJ"
    nva_fpt_data$FOOTPRINT[i] = sector_fpt$FOOTPRINT[sector_fpt$SECTOR==sector]*1000
    nva_fpt_data$UNIT_FOOTPRINT[i] = "KJ_CPEUR"
  }

  return(nva_fpt_data)
  # -------------------------------------------------- #
}

get_branches_imp_coef_nrg = function(selectedYear)
{
  # fetch data

  eurostat_pefasu_data = get_eurostat_data(paste0("https://ec.europa.eu/eurostat/api/dissemination/statistics/1.0/data/env_ac_pefasu?geo=FR&geo=EU27_2020&unit=TJ&time=",selectedYear,"&stk_flow=SUP&prod_nrg=R30&nace_r2=TOTAL"))

  # domestic production

  eurostat_nama_data = get_eurostat_data(paste0("https://ec.europa.eu/eurostat/api/dissemination/statistics/1.0/data/nama_10_a64?geo=FR&geo=EU27_2020&unit=CP_MEUR&time=",selectedYear,"&nace_r2=TOTAL&na_item=B1G"))


  fpt_fra =  eurostat_pefasu_data$values[eurostat_pefasu_data$geo=="FR"] / eurostat_nama_data$values[eurostat_nama_data$geo=="FR"]
  fpt_euu =  eurostat_pefasu_data$values[eurostat_pefasu_data$geo=="EU27_2020"] / eurostat_nama_data$values[eurostat_nama_data$geo=="EU27_2020"]

  branches_imp_coef = fpt_euu / fpt_fra

  return(branches_imp_coef)
}
