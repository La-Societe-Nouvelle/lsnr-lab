#'Build and returns all data required to the WAT indicator computations.
#'
#'Returns a `list` made up of value added impacts by French branches, imported products associated coefficient,
#'Data sources and values unit. This data will be used in both <BuildBranchesData> and <BuildDivisionsData> functions.
#'
#' @param Year Considered year.
#'
#' @importFrom dplyr %>%
#' @importFrom tidyr pivot_wider
#' @importFrom tidyr pivot_longer
#'
#' @return An object `list` made up of 4 elements : value added impacts by French branches,
#' imported products associated coefficient, data sources and values unit.
#'
#' @seealso \code{\link{build_branches_nva_fpt_ghg}}, \code{\link{build_branches_nva_fpt_nrg}},
#'  \code{\link{build_branches_fpt}}, \code{\link{build_divisions_fpt}}, \code{\link{get_indicator_list}}.
#'
#' @examples
#' build_branches_nva_fpt_wat(2018)
#' @noRd

# List of sector
#   AGR       Agriculture
#   MIN       Industrie extractive
#   IND       Industrie
#   ELC       Production d'electricite
#   CON       Construction
#   SER       Services

build_branches_nva_fpt_wat = function(selectedYear)
{
  # get branches aggregates -------------------------- #

  branches_aggregates = get_branches_aggregates(selectedYear)

  # fetch data --------------------------------------- #

  main = "https://ec.europa.eu/eurostat/api/dissemination/statistics/1.0/data/env_wat_abs"
  filters = paste0("?geo=FR&unit=MIO_M3&time=",selectedYear,"&wat_src=FRW")

  wat_abs_data = get_eurostat_data(paste0(main,filters)) %>%
    pivot_wider(names_from = wat_proc, values_from = value)

  # raw fpt ------------------------------------------ #

  wat_cons_coef = data.frame(
    SECTOR = c("AGR","MIN","IND","ELC","CON","SER"),
    COEFFICIENT = c(0.82,0.07,0.07,0.1,0.21,0.21)
  ) %>% pivot_wider(names_from = SECTOR, values_from = COEFFICIENT)

  # wat_cons_coef = lsnr:::WaterConsumptionCoefficients

  raw_fpt = list()
  raw_fpt$AGR_FPT = (wat_abs_data$ABS_AGR[1]*1000)*wat_cons_coef$AGR / branches_aggregates$NVA[branches_aggregates$BRANCH == "AZ"]
  raw_fpt$MIN_FPT = (wat_abs_data$ABS_MIN[1]*1000)*wat_cons_coef$MIN / branches_aggregates$NVA[branches_aggregates$BRANCH == "BZ"]
  raw_fpt$IND_FPT = (wat_abs_data$ABS_IND[1]*1000)*wat_cons_coef$IND / sum(branches_aggregates$NVA[branches_aggregates$BRANCH %in% c("CA","CB","CC","CD","CE","CF","CG","CH","CI","CJ","CK","CL","CM","DZ","EZ")])
  raw_fpt$ELC_FPT = (wat_abs_data$ABS_ELC_CL[1]*1000)*wat_cons_coef$ELC / branches_aggregates$NVA[branches_aggregates$BRANCH == "DZ"]
  raw_fpt$CON_FPT = (wat_abs_data$ABS_CON[1]*1000)*wat_cons_coef$CON / branches_aggregates$NVA[branches_aggregates$BRANCH == "FZ"]
  raw_fpt$SER_FPT = (wat_abs_data$ABS_SER[1]*1000)*wat_cons_coef$SER / sum(branches_aggregates$NVA[branches_aggregates$BRANCH %in% c("GZ","HZ","IZ","JA","JB","JC","KZ","LZ","MA","MB","MC","NZ","OZ","PZ","QA","QB","RZ","SZ","TZ")])

  # sector fpt --------------------------------------- #

  sector_fpt_list = list()

  sector_fpt_list[["A"]]    = raw_fpt$AGR_FPT
  sector_fpt_list[["B"]]    = raw_fpt$MIN_FPT
  sector_fpt_list[["C-E"]]  = raw_fpt$IND_FPT
  sector_fpt_list[["D"]]    = raw_fpt$IND_FPT + raw_fpt$ELC_FPT
  sector_fpt_list[["F"]]    = raw_fpt$CON_FPT
  sector_fpt_list[["G-T"]]  = raw_fpt$SER_FPT

  sector_fpt = cbind.data.frame(sector_fpt_list) %>% pivot_longer(cols = names(sector_fpt_list))
  colnames(sector_fpt) = c("SECTOR", "FOOTPRINT")
  print(sector_fpt)

  # build nva fpt dataframe -------------------------- #

  nva_fpt_data = data.frame(BRANCH = as.character(branches_aggregates$BRANCH), NVA = as.numeric(branches_aggregates$NVA))

  branch_sector_fpt_matrix = lsnr:::MatrixWAT

  for(i in 1:nrow(nva_fpt_data))
  {
    # get sector
    branch = nva_fpt_data$BRANCH[i]
    sector = branch_sector_fpt_matrix$SECTOR[branch_sector_fpt_matrix$BRANCH==branch]

    # build values
    nva_fpt_data$GROSS_IMPACT[i] = round(sector_fpt$FOOTPRINT[sector_fpt$SECTOR==sector] * branches_aggregates$NVA[i] *1000, digits = 0)
    nva_fpt_data$UNIT_FOOTPRINT[i] = "M3"
    nva_fpt_data$FOOTPRINT[i] = round(sector_fpt$FOOTPRINT[sector_fpt$SECTOR==sector], digits = 1)
    nva_fpt_data$UNIT_FOOTPRINT[i] = "L_CPEUR"
  }

  return(nva_fpt_data)
  # -------------------------------------------------- #
}

build_divisions_nva_fpt_wat = function(selectedYear)
{
  # get divisions aggregates ------------------------- #

  divisions_aggregates = get_divisions_aggregates(selectedYear)
  branches_aggregates = get_branches_aggregates(selectedYear)

  # fetch data --------------------------------------- #

  main = "https://ec.europa.eu/eurostat/api/dissemination/statistics/1.0/data/env_wat_abs"
  filters = paste0("?geo=FR&unit=MIO_M3&time=",selectedYear,"&wat_src=FRW")

  wat_abs_data = get_eurostat_data(paste0(main,filters)) %>%
    pivot_wider(names_from = wat_proc, values_from = value)

  # raw fpt ------------------------------------------ #

  wat_cons_coef = data.frame(
    SECTOR = c("AGR","MIN","IND","ELC","CON","SER"),
    COEFFICIENT = c(0.82,0.07,0.07,0.1,0.21,0.21)
  ) %>% pivot_wider(names_from = SECTOR, values_from = COEFFICIENT)

  # wat_cons_coef = lsnr:::WaterConsumptionCoefficients

  raw_fpt = list()
  raw_fpt$AGR_FPT = (wat_abs_data$ABS_AGR[1]*1000)*wat_cons_coef$AGR / branches_aggregates$NVA[branches_aggregates$BRANCH == "AZ"]
  raw_fpt$MIN_FPT = (wat_abs_data$ABS_MIN[1]*1000)*wat_cons_coef$MIN / branches_aggregates$NVA[branches_aggregates$BRANCH == "BZ"]
  raw_fpt$IND_FPT = (wat_abs_data$ABS_IND[1]*1000)*wat_cons_coef$IND / sum(branches_aggregates$NVA[branches_aggregates$BRANCH %in% c("CA","CB","CC","CD","CE","CF","CG","CH","CI","CJ","CK","CL","CM","DZ","EZ")])
  raw_fpt$ELC_FPT = (wat_abs_data$ABS_ELC_CL[1]*1000)*wat_cons_coef$ELC / branches_aggregates$NVA[branches_aggregates$BRANCH == "DZ"]
  raw_fpt$CON_FPT = (wat_abs_data$ABS_CON[1]*1000)*wat_cons_coef$CON / branches_aggregates$NVA[branches_aggregates$BRANCH == "FZ"]
  raw_fpt$SER_FPT = (wat_abs_data$ABS_SER[1]*1000)*wat_cons_coef$SER / sum(branches_aggregates$NVA[branches_aggregates$BRANCH %in% c("GZ","HZ","IZ","JA","JB","JC","KZ","LZ","MA","MB","MC","NZ","OZ","PZ","QA","QB","RZ","SZ","TZ")])

    # sector fpt --------------------------------------- #

  sector_fpt_list = list()

  sector_fpt_list[["A"]]        = raw_fpt$AGR_FPT
  sector_fpt_list[["B"]]        = raw_fpt$MIN_FPT
  sector_fpt_list[["C-E_X_D"]]  = raw_fpt$IND_FPT
  sector_fpt_list[["D"]]        = raw_fpt$IND_FPT + raw_fpt$ELC_FPT
  sector_fpt_list[["F"]]        = raw_fpt$CON_FPT
  sector_fpt_list[["G-T"]]      = raw_fpt$SER_FPT

  sector_fpt = cbind.data.frame(sector_fpt_list) %>% pivot_longer(cols = names(sector_fpt_list))
  colnames(sector_fpt) = c("SECTOR", "FOOTPRINT")

  # build nva fpt dataframe -------------------------- #

  nva_fpt_data = data.frame(DIVISION = as.character(divisions_aggregates$CNA_ACTIVITE), NVA = as.numeric(divisions_aggregates$NVA))

  division_sector_fpt_matrix = lsnr:::DivisionMappingWAT
  division_sector_fpt_matrix$DIVISION[nchar(division_sector_fpt_matrix$DIVISION)==1] = paste0("0",division_sector_fpt_matrix$DIVISION[nchar(division_sector_fpt_matrix$DIVISION)==1])

  for(i in 1:nrow(nva_fpt_data))
  {
    # get sector
    division = nva_fpt_data$DIVISION[i]
    sector = division_sector_fpt_matrix$SECTOR[division_sector_fpt_matrix$DIVISION==division]

    # build values
    nva_fpt_data$GROSS_IMPACT[i] = sector_fpt$FOOTPRINT[sector_fpt$SECTOR==sector] * divisions_aggregates$NVA[i]
    nva_fpt_data$UNIT_GROSS_IMPACT[i] = "THS_M3"
    nva_fpt_data$FOOTPRINT[i] = sector_fpt$FOOTPRINT[sector_fpt$SECTOR==sector]
    nva_fpt_data$UNIT_FOOTPRINT[i] = "L_CPEUR"
  }

  return(nva_fpt_data)
  # -------------------------------------------------- #
}

get_branches_imp_coef_wat = function(selectedYear)
{
  # fetch data

  main = "https://ec.europa.eu/eurostat/api/dissemination/statistics/1.0/data/env_wat_abs"
  filters = paste0("?geo=FR&unit=MIO_M3&time=",selectedYear,"&wat_src=FRW")

  eurostat_wat_abs_data = lsnr:::get_eurostat_data(paste0(main,filters)) %>%
    pivot_wider(names_from = wat_proc, values_from = value)

  # domestic production

  eurostat_nama_data = get_eurostat_data(paste0("https://ec.europa.eu/eurostat/api/dissemination/statistics/1.0/data/nama_10_a64?geo=FR&geo=EU27_2020&unit=CP_MEUR&time=",selectedYear,"&nace_r2=TOTAL&na_item=B1G"))

  fpt_fra =  eurostat_wat_abs_data$ABST[eurostat_wat_abs_data$geo=="FR"] / eurostat_nama_data$value[eurostat_nama_data$geo=="FR"]
  fpt_euu =  eurostat_wat_abs_data$ABST[eurostat_wat_abs_data$geo=="FR"] / eurostat_nama_data$value[eurostat_nama_data$geo=="FR"] #LACK of EU impact data

  branches_imp_coef = fpt_euu / fpt_fra

  return(branches_imp_coef)
}
