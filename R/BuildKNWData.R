#'Build and returns all data required to the KNW indicator computations.
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
#' build_branches_nva_fpt_knw(2018)
#' @noRd



build_branches_nva_fpt_knw = function(selectedYear)
{
  # get branches aggregates -------------------------- #

  branches_aggregates = get_branches_aggregates(selectedYear)

  # fetch data --------------------------------------- #

  trng_cvt_data = get_eurostat("trng_cvt_16n2") %>%
    filter(geo == "FR",
           cost =="TOTAL",
           time == paste0(selectedYear,"-01-01"),
           unit =="PC")

  # sector fpt --------------------------------------- #

  sector_fpt_list = list()

  sector_fpt_list[["TOTAL"]]   = trng_cvt_data$values[trng_cvt_data$nace_r2=="TOTAL"]
  sector_fpt_list[["B-E"]]     = trng_cvt_data$values[trng_cvt_data$nace_r2=="B-E"]
  sector_fpt_list[["F"]]       = trng_cvt_data$values[trng_cvt_data$nace_r2=="F"]
  sector_fpt_list[["G-I"]]     = trng_cvt_data$values[trng_cvt_data$nace_r2=="G-I"]
  sector_fpt_list[["J_K"]]     = trng_cvt_data$values[trng_cvt_data$nace_r2=="J_K"]
  sector_fpt_list[["L-N_R_S"]] = trng_cvt_data$values[trng_cvt_data$nace_r2=="L-N_R_S"]

  sector_fpt = cbind.data.frame(sector_fpt_list) %>% pivot_longer(cols = names(sector_fpt_list))
  colnames(sector_fpt) = c("SECTOR", "FOOTPRINT")
  print(sector_fpt)

  # build nva fpt dataframe -------------------------- #

  nva_fpt_data = as.data.frame(cbind(branches_aggregates$BRANCH, branches_aggregates$NVA))
  colnames(nva_fpt_data) = c("BRANCH", "NVA")

  branch_sector_fpt_matrix = lsnr:::MatrixKNW

  for(i in 1:nrow(nva_fpt_data))
  {
    # get sector
    branch = nva_fpt_data$BRANCH[i]
    sector = branch_sector_fpt_matrix$SECTOR[branch_sector_fpt_matrix$BRANCH==branch]

    # build values
    nva_fpt_data$GROSS_IMPACT[i] = sector_fpt$FOOTPRINT[sector_fpt$SECTOR==sector]/100 * branches_aggregates$NVA[i]
    nva_fpt_data$FOOTPRINT[i] = sector_fpt$FOOTPRINT[sector_fpt$SECTOR==sector]
    nva_fpt_data$UNIT_FOOTPRINT[i] = "P100"
  }

  return(nva_fpt_data)
  # -------------------------------------------------- #
}

build_divisions_nva_fpt_knw = function(selectedYear)
{
  # get branches aggregates -------------------------- #

  divisions_aggregates = get_divisions_aggregates(selectedYear)

  # fetch data --------------------------------------- #

  trng_cvt_data = get_eurostat("trng_cvt_16n2") %>%
    filter(geo == "FR",
           cost =="TOTAL",
           time == paste0(selectedYear,"-01-01"),
           unit =="PC")

  # sector fpt --------------------------------------- #

  sector_fpt_list = list()

  sector_fpt_list[["TOTAL"]]  = trng_cvt_data$values[trng_cvt_data$nace_r2=="TOTAL"]
  sector_fpt_list[["B-E"]]    = trng_cvt_data$values[trng_cvt_data$nace_r2=="B-E"]
  sector_fpt_list[["F"]]      = trng_cvt_data$values[trng_cvt_data$nace_r2=="F"]
  sector_fpt_list[["G-I"]]    = trng_cvt_data$values[trng_cvt_data$nace_r2=="G-I"]
  sector_fpt_list[["J_K"]]    = trng_cvt_data$values[trng_cvt_data$nace_r2=="J_K"]
  sector_fpt_list[["L-N_R_S"]]= trng_cvt_data$values[trng_cvt_data$nace_r2=="L-N_R_S"]

  sector_fpt = cbind.data.frame(sector_fpt_list) %>% pivot_longer(cols = names(sector_fpt_list))
  colnames(sector_fpt) = c("SECTOR", "FOOTPRINT")
  print(sector_fpt)

  # build nva fpt dataframe -------------------------- #

  nva_fpt_data = as.data.frame(cbind(divisions_aggregates$BRANCH, divisions_aggregates$NVA))
  colnames(nva_fpt_data) = c("BRANCH", "NVA")

  branch_sector_fpt_matrix =lsnr:::MatrixKNW

  for(i in 1:nrow(nva_fpt_data))
  {
    # get sector
    branch = nva_fpt_data$BRANCH[i]
    sector = branch_sector_fpt_matrix$SECTOR[branch_sector_fpt_matrix$BRANCH==branch]

    # build values
    nva_fpt_data$GROSS_IMPACT[i] = sector_fpt$FOOTPRINT[sector_fpt$SECTOR==sector]/100 * divisions_aggregates$NVA[i]
    nva_fpt_data$FOOTPRINT[i] = sector_fpt$FOOTPRINT[sector_fpt$SECTOR==sector]
    nva_fpt_data$UNIT_FOOTPRINT[i] = "P100"
  }

  return(nva_fpt_data)
  # -------------------------------------------------- #
}

get_branches_imp_coef_knw = function(selectedYear)
{
  # fetch data

  trng_cvt_data = get_eurostat("trng_cvt_16n2") %>%
    filter(geo == "FR",
           cost =="TOTAL",
           time == paste0(selectedYear,"-01-01"),
           unit =="PC")

  fpt_fra =  eurostat_data$values[eurostat_data$geo=="FR"]
  fpt_euu =  eurostat_data$values[eurostat_data$geo=="EU28"]

  branches_imp_coef = fpt_euu / fpt_fra

  return(branches_imp_coef)
}
