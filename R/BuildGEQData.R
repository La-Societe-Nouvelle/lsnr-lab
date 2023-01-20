#'Build and returns all data required to the GEQ indicator computations.
#'
#'Returns a `list` made up of value added impacts by French branches, imported products associated coefficient,
#'Data sources and values unit. This data will be used in both BuildBranchesData and BuildDivisionsData functions.
#'
#' @param Year Considered year.
#'
#' @importFrom eurostat get_eurostat
#' @return An object `list` made up of 4 elements : value added impacts by French branches,
#' imported products associated coefficient, data sources and values unit.
#' @seealso \code{\link{BuildECOData}}, \code{\link{BuildGHGData}}, \code{\link{BuildNRGData}},
#'  \code{\link{BuildBranchesData}}, \code{\link{BuildDivisionsData}}, \code{\link{FetchDataAvailability}}.
#' @examples
#' BuildGEQData(max(FetchDataAvailability("GEQ"))
#' @export

source('R/InseeDataManager.R')

build_branches_nva_fpt_geq = function(year)
{
  # get branches aggregates -------------------------- #

  branches_aggregates = get_branches_aggregates(year)

  # fetch data --------------------------------------- #

  tryCatch({
    eurostat_data = get_eurostat("earn_ses_hourly")
  }, error = function(e) {
    stop(paste0("Données eurostat indisponibles pour ",year," (table earn_ses_hourly)"))
  })

  ses_data = eurostat_data %>%
    filter(geo == "FR") %>%
    filter(age == "TOTAL") %>%
    filter(indic_se == "MEAN_E_EUR") %>%
    filter(isco08 == "TOTAL") %>%
    filter(worktime == "TOTAL") %>%
    filter(time == paste0(year,"-01-01")) %>%
    pivot_wider(names_from = sex, values_from = values)

  # sector fpt --------------------------------------- #

  sector_fpt_list = list()

  sector_fpt_list[["B-F"]] = abs(ses_data$F[ses_data$nace_r2=="B-F"] - ses_data$M[ses_data$nace_r2=="B-F"]) / ses_data$T[ses_data$nace_r2=="B-F"] *100
  sector_fpt_list[["G-N"]] = abs(ses_data$F[ses_data$nace_r2=="G-N"] - ses_data$M[ses_data$nace_r2=="G-N"]) / ses_data$T[ses_data$nace_r2=="G-N"] *100
  sector_fpt_list[["P-S"]] = abs(ses_data$F[ses_data$nace_r2=="P-S"] - ses_data$M[ses_data$nace_r2=="P-S"]) / ses_data$T[ses_data$nace_r2=="P-S"] *100
  sector_fpt_list[["B-S_X_O"]] = abs(ses_data$F[ses_data$nace_r2=="B-S_X_O"] - ses_data$M[ses_data$nace_r2=="B-S_X_O"]) / ses_data$T[ses_data$nace_r2=="B-S_X_O"] *100

  sector_fpt = cbind.data.frame(sector_fpt_list) %>% pivot_longer(cols = names(sector_fpt_list))
  colnames(sector_fpt) = c("SECTOR", "FOOTPRINT")

  # build nva fpt dataframe -------------------------- #

  nva_fpt_data = as.data.frame(cbind(branches_aggregates$BRANCH, branches_aggregates$NVA))
  colnames(nva_fpt_data) = c("BRANCH", "NVA")

  wd = getwd()
  branch_sector_fpt_matrix = read.csv(paste0(wd,"/lib/","MatrixGEQ.csv"), header=T, sep=";")

  for(i in 1:nrow(nva_fpt_data))
  {
    # get sector
    branch = nva_fpt_data$BRANCH[i]
    sector = branch_sector_fpt_matrix$SECTOR[branch_sector_fpt_matrix$BRANCH==branch]

    # build values
    nva_fpt_data$GROSS_IMPACT[i] = sector_fpt$FOOTPRINT[sector_fpt$SECTOR==sector]
    nva_fpt_data$FOOTPRINT[i] = sector_fpt$FOOTPRINT[sector_fpt$SECTOR==sector]
    nva_fpt_data$UNIT_FOOTPRINT[i] = "P100"
  }

  return(nva_fpt_data)
  # -------------------------------------------------- #
}

build_divisions_nva_fpt_geq = function(year)
{
  # get divisions aggregates -------------------------- #

  divisions_aggregates = get_divisions_aggregates(year)

  # fetch data --------------------------------------- #

  tryCatch({
    eurostat_data = get_eurostat("earn_ses_hourly")
  }, error = function(e) {
    stop(paste0("Données eurostat indisponibles pour ",year," (table earn_ses_hourly)"))
  })

  ses_data = eurostat_data %>%
    filter(geo == "FR") %>%
    filter(age == "TOTAL") %>%
    filter(indic_se == "MEAN_E_EUR") %>%
    filter(isco08 == "TOTAL") %>%
    filter(worktime == "TOTAL") %>%
    filter(time == paste0(year,"-01-01")) %>%
    pivot_wider(names_from = sex, values_from = values)

  # sector fpt --------------------------------------- #

  sector_fpt_list = list()

  sector_fpt_list[["B-F"]] = abs(ses_data$F[ses_data$nace_r2=="B-F"] - ses_data$M[ses_data$nace_r2=="B-F"]) / ses_data$T[ses_data$nace_r2=="B-F"] *100
  sector_fpt_list[["G-N"]] = abs(ses_data$F[ses_data$nace_r2=="G-N"] - ses_data$M[ses_data$nace_r2=="G-N"]) / ses_data$T[ses_data$nace_r2=="G-N"] *100
  sector_fpt_list[["P-S"]] = abs(ses_data$F[ses_data$nace_r2=="P-S"] - ses_data$M[ses_data$nace_r2=="P-S"]) / ses_data$T[ses_data$nace_r2=="P-S"] *100
  sector_fpt_list[["B-S_X_O"]] = abs(ses_data$F[ses_data$nace_r2=="B-S_X_O"] - ses_data$M[ses_data$nace_r2=="B-S_X_O"]) / ses_data$T[ses_data$nace_r2=="B-S_X_O"] *100

  sector_fpt = cbind.data.frame(sector_fpt_list) %>% pivot_longer(cols = names(sector_fpt_list))
  colnames(sector_fpt) = c("SECTOR", "FOOTPRINT")

  # build nva fpt dataframe -------------------------- #

  nva_fpt_data = as.data.frame(cbind(divisions_aggregates$DIVISION, divisions_aggregates$NVA))
  colnames(nva_fpt_data) = c("DIVISION", "NVA")

  wd = getwd()
  branch_sector_fpt_matrix = read.csv(paste0(wd,"/lib/","MatrixGEQ.csv"), header=T, sep=";")

  for(i in 1:nrow(nva_fpt_data))
  {
    # get sector
    branch = nva_fpt_data$BRANCH[i]
    sector = branch_sector_fpt_matrix$SECTOR[branch_sector_fpt_matrix$BRANCH==branch]

    # build values
    nva_fpt_data$GROSS_IMPACT[i] = sector_fpt$FOOTPRINT[sector_fpt$SECTOR==sector]
    nva_fpt_data$FOOTPRINT[i] = sector_fpt$FOOTPRINT[sector_fpt$SECTOR==sector]
    nva_fpt_data$UNIT_FOOTPRINT[i] = "P100"
  }

  return(nva_fpt_data)
  # -------------------------------------------------- #
}

get_branches_imp_coef_geq = function(year)
{
  # fetch data
  eurostat_data = get_eurostat(
    "earn_ses_hourly",
    time_format = "num",
    filters = list(geo=c("FR","EU28"), age="TOTAL", indic_se="MEAN_E_EUR", isco08="TOTAL", worktime="TOTAL", time=year, nace_r2="TOTAL")
  )

  fpt_fra =  eurostat_data$values[eurostat_data$geo=="FR"]
  fpt_euu =  eurostat_data$values[eurostat_data$geo=="EU28"]

  branches_imp_coef = fpt_euu / fpt_fra

  return(branches_imp_coef)
}
