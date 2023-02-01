#'Build and returns all data required to the MAT indicator computations.
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
#'  \code{\link{BuildBranchesData}}, \code{\link{BuildDivisionsData}}, \code{\link{FetchDataAvailability}}.
#' @examples
#' build_branches_nva_fpt_mat(2018)
#' @noRd



build_branches_nva_fpt_mat = function(selectedYear)
{
  # get branches aggregates -------------------------- #

  branches_aggregates = get_branches_aggregates(selectedYear)

  # fetch data --------------------------------------- #

  tryCatch({
    eurostat_data = get_eurostat("env_ac_mfa")
  }, error = function(e) {
    stop(paste0("Données eurostat indisponibles pour ",selectedYear," (table env_ac_mfa)"))
  })

  ac_mfa_data = eurostat_data %>%
    filter(geo == "FR") %>%
    filter(indic_env == "DE") %>%
    filter(unit == "THS_T") %>%
    filter(time == paste0(selectedYear,"-01-01")) %>%
    filter(material %in% c("MF1","MF2","MF3","MF4"))

  # sector fpt --------------------------------------- #

  sector_fpt_list = list()

  sector_fpt_list[["A"]]    = ac_mfa_data$values[ac_mfa_data$material=="MF1"]*1000 / branches_aggregates$NVA[branches_aggregates$BRANCH=="AZ"]
  sector_fpt_list[["B"]]    = sum(ac_mfa_data$values[ac_mfa_data$material %in% c("MF2","MF3","MF4")])*1000 / branches_aggregates$NVA[branches_aggregates$BRANCH=="BZ"]
  sector_fpt_list[["C-T"]]  = 0

  sector_fpt = cbind.data.frame(sector_fpt_list) %>% pivot_longer(cols = names(sector_fpt_list))
  colnames(sector_fpt) = c("SECTOR", "FOOTPRINT")

  # build nva fpt dataframe -------------------------- #

  nva_fpt_data = as.data.frame(cbind(branches_aggregates$BRANCH, branches_aggregates$NVA))
  colnames(nva_fpt_data) = c("BRANCH", "NVA")

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

  tryCatch({
    eurostat_data = get_eurostat("env_ac_mfa")
  }, error = function(e) {
    stop(paste0("Données eurostat indisponibles pour ",selectedYear," (table env_ac_mfa)"))
  })

  ac_mfa_data = eurostat_data %>%
    filter(geo == "FR") %>%
    filter(indic_env == "DE") %>%
    filter(unit == "THS_T") %>%
    filter(time == paste0(selectedYear,"-01-01")) %>%
    filter(material %in% c("MF1","MF2","MF3","MF4"))

  # sector fpt --------------------------------------- #

  sector_fpt_list = list()

  # 01 -> 03 / A
  sector_fpt_list[["A"]] = sum(ac_mfa_data$values[ac_mfa_data$material %in% c("MF1")])*1000 / divisions_aggregates$NVA[divisions_aggregates$DIVISION %in% c("01","02","03")]
  # 05 -> 08 / B
  sector_fpt_list[["B"]] = sum(ac_mfa_data$values[ac_mfa_data$material %in% c("MF2","MF3","MF4")])*1000 / divisions_aggregates$NVA[divisions_aggregates$DIVISION %in% c("05","06","07","08")]
  # 09 -> 98 / C-T
  sector_fpt_list[["C-T"]] = 0

  sector_fpt = cbind.data.frame(sector_fpt_list) %>% pivot_longer(cols = names(sector_fpt_list))
  colnames(sector_fpt) = c("SECTOR", "FOOTPRINT")

  # build nva fpt dataframe -------------------------- #

  nva_fpt_data = as.data.frame(cbind(divisions_aggregates$DIVISION, divisions_aggregates$NVA))
  colnames(nva_fpt_data) = c("DIVISION", "NVA")

  branch_sector_fpt_matrix = lsnr:::DivisionMappingMAT

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
  eurostat_mfa_data = get_eurostat(
    "env_ac_mfa",
    time_format = "num",
    filters = list(geo=c("FR","EU27_2020"), indic_env="DE", unit="THS_T", time=selectedYear, material="TOTAL", nace_r2="TOTAL")
  )

   # domestic production
  eurostat_nama_data = get_eurostat(
    "nama_10_a64",
    filters = list(geo=c("FR","EU27_2020"), na_item="B1G", time=selectedYear, unit="CP_MEUR", nace_r2="TOTAL")
  )

  fpt_fra =  eurostat_mfa_data$values[eurostat_mfa_data$geo=="FR"] / eurostat_nama_data$values[eurostat_nama_data$geo=="FR"]
  fpt_euu =  eurostat_mfa_data$values[eurostat_mfa_data$geo=="EU27_2020"] / eurostat_nama_data$values[eurostat_nama_data$geo=="EU27_2020"]

  branches_imp_coef = fpt_euu / fpt_fra

  return(branches_imp_coef)
}
