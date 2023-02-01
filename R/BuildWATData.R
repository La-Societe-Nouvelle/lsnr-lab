#'Build and returns all data required to the WAT indicator computations.
#'
#'Returns a `list` made up of value added impacts by French branches, imported products associated coefficient,
#'Data sources and values unit. This data will be used in both BuildBranchesData and BuildDivisionsData functions.
#'
#' @param Year Considered year.
#'
#' @return An object `list` made up of 4 elements : value added impacts by French branches,
#' imported products associated coefficient, data sources and values unit.
#'
#' @seealso \code{\link{BuildECOData}}, \code{\link{BuildGHGData}}, \code{\link{BuildNRGData}},
#'  \code{\link{BuildBranchesData}}, \code{\link{BuildDivisionsData}}, \code{\link{FetchDataAvailability}}.
#'
#' @examples
#' BuildWATData(max(FetchDataAvailability("WAT"))
#' @noRd



build_branches_nva_fpt_wat = function(selectedYear)
{
  # get branches aggregates -------------------------- #

  branches_aggregates = get_branches_aggregates(selectedYear)

  # fetch data --------------------------------------- #

  tryCatch({
    eurostat_data = get_eurostat(
      "env_wat_abs",
      time_format = "num",
      filters = list(geo = c("FR"), unit = "MIO_M3", time = selectedYear, wat_src = "FRW")
    )
  }, error = function(e) {
    stop(paste0("Données eurostat indisponibles pour ",selectedYear," (table env_wat_abs)"))
  })

  wat_abs_data = eurostat_data %>%
      pivot_wider(names_from = wat_proc, values_from = values)

  # raw fpt ------------------------------------------ #

  raw_fpt = list()
  raw_fpt$AGR_FPT = wat_abs_data$ABS_AGR[1]*1000 / branches_aggregates$NVA[branches_aggregates$BRANCH == "AZ"]
  raw_fpt$MIN_FPT = wat_abs_data$ABS_MIN[1]*1000 / branches_aggregates$NVA[branches_aggregates$BRANCH == "BZ"]
  raw_fpt$IND_FPT = wat_abs_data$ABS_IND[1]*1000 / sum(branches_aggregates$NVA[branches_aggregates$BRANCH %in% c("CA","CB","CC","CD","CE","CF","CG","CH","CI","CJ","CK","CL","CM","DZ","EZ")])
  raw_fpt$ELC_FPT = wat_abs_data$ABS_ELC_CL[1]*1000 / branches_aggregates$NVA[branches_aggregates$BRANCH == "DZ"]
  raw_fpt$CON_FPT = wat_abs_data$ABS_CON[1]*1000 / branches_aggregates$NVA[branches_aggregates$BRANCH == "FZ"]
  raw_fpt$SER_FPT = wat_abs_data$ABS_SER[1]*1000 / sum(branches_aggregates$NVA[branches_aggregates$BRANCH %in% c("GZ","HZ","IZ","JA","JB","JC","KZ","LZ","MA","MB","MC","NZ","OZ","PZ","QA","QB","RZ","SZ","TZ")])

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

  nva_fpt_data = as.data.frame(cbind(branches_aggregates$BRANCH, branches_aggregates$NVA))
  colnames(nva_fpt_data) = c("BRANCH", "NVA")

  branch_sector_fpt_matrix = lsnr:::MatrixWAT

  for(i in 1:nrow(nva_fpt_data))
  {
    # get sector
    branch = nva_fpt_data$BRANCH[i]
    sector = branch_sector_fpt_matrix$SECTOR[branch_sector_fpt_matrix$BRANCH==branch]

    # build values
    nva_fpt_data$GROSS_IMPACT[i] = sector_fpt$FOOTPRINT[sector_fpt$SECTOR==sector] * branches_aggregates$NVA[i]
    nva_fpt_data$FOOTPRINT[i] = sector_fpt$FOOTPRINT[sector_fpt$SECTOR==sector]
    nva_fpt_data$UNIT_FOOTPRINT[i] = "L_CPEUR"
  }

  return(nva_fpt_data)
  # -------------------------------------------------- #
}

build_divisions_nva_fpt_wat = function(selectedYear)
{
  # get divisions aggregates ------------------------- #

  divisions_aggregates = get_divisions_aggregates(selectedYear)

  # fetch data --------------------------------------- #

  tryCatch({

    eurostat_data = get_eurostat(
      "env_wat_abs",
      time_format = "num",
      filters = list(geo = c("FR"), unit = "MIO_M3", time = selectedYear, wat_src = "FRW")
    )

  }, error = function(e) {
    stop(paste0("Données eurostat indisponibles pour ",selectedYear," (table env_wat_abs)"))
  })

  wat_abs_data = eurostat_data %>%
      pivot_wider(names_from = wat_proc, values_from = values)

  # raw fpt ------------------------------------------ #

  raw_fpt = list()
  raw_fpt$AGR_FPT = wat_abs_data$ABS_AGR[1]*1000 / divisions_aggregates$NVA[divisions_aggregates$BRANCH == "AZ"]
  raw_fpt$MIN_FPT = wat_abs_data$ABS_MIN[1]*1000 / divisions_aggregates$NVA[divisions_aggregates$BRANCH == "BZ"]
  raw_fpt$IND_FPT = wat_abs_data$ABS_IND[1]*1000 / sum(divisions_aggregates$NVA[divisions_aggregates$BRANCH %in% c("CA","CB","CC","CD","CE","CF","CG","CH","CI","CJ","CK","CL","CM","DZ","EZ")])
  raw_fpt$ELC_FPT = wat_abs_data$ABS_ELC_CL[1]*1000 / divisions_aggregates$NVA[divisions_aggregates$BRANCH == "DZ"]
  raw_fpt$CON_FPT = wat_abs_data$ABS_CON[1]*1000 / divisions_aggregates$NVA[divisions_aggregates$BRANCH == "FZ"]
  raw_fpt$SER_FPT = wat_abs_data$ABS_SER[1]*1000 / sum(divisions_aggregates$NVA[divisions_aggregates$BRANCH %in% c("GZ","HZ","IZ","JA","JB","JC","KZ","LZ","MA","MB","MC","NZ","OZ","PZ","QA","QB","RZ","SZ","TZ")])

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

  nva_fpt_data = as.data.frame(cbind(divisions_aggregates$DIVISION, divisions_aggregates$NVA))
  colnames(nva_fpt_data) = c("DIVISION", "NVA")

  division_sector_fpt_matrix = lsnr:::DivisionMappingWAT

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
  eurostat_wat_abs_data = get_eurostat(
    "env_wat_abs",
    time_format = "num",
    filters = list(geo=c("FR","EU27_2020"), unit = "MIO_M3", time = selectedYear, wat_src = "FRW")
  )

  # domestic production
  eurostat_nama_data = get_eurostat(
    "nama_10_a64",
    filters = list(geo=c("FR","EU27_2020"), na_item="B1G", time=selectedYear, unit="CP_MEUR", nace_r2="TOTAL")
  )

  fpt_fra =  eurostat_wat_abs_data$values[eurostat_wat_abs_data$geo=="FR"] / eurostat_nama_data$values[eurostat_nama_data$geo=="FR"]
  fpt_euu =  eurostat_wat_abs_data$values[eurostat_wat_abs_data$geo=="EU27_2020"] / eurostat_nama_data$values[eurostat_nama_data$geo=="EU27_2020"]

  branches_imp_coef = fpt_euu / fpt_fra

  return(branches_imp_coef)
}
