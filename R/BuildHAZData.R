#'Build and returns all data required to the HAZ indicator computations.
#'
#'Returns a `list` made up of value added impacts by French branches, imported products associated coefficient,
#'Data sources and values unit. This data will be used in both BuildBranchesData and BuildDivisionsData functions.
#'
#' @param Year Considered Year.
#'
#' @return An object `list` made up of 4 elements : value added impacts by French branches,
#' imported products associated coefficient, data sources and values unit.
#' @seealso \code{\link{BuildECOData}}, \code{\link{BuildGHGData}},
#'  \code{\link{BuildBranchesData}}, \code{\link{BuildDivisionsData}}, \code{\link{FetchDataDisponibility}}.
#' @examples
#' BuildHAZData(max(FetchDataDisponibility("HAZ"))
#' @export

source('R/InseeDataManager.R')

build_branches_nva_fpt_haz = function(year) 
{
  # get branches aggregates -------------------------- #

  branches_aggregates = get_branches_aggregates(year)

  # fetch data --------------------------------------- #
  
  tryCatch({
    eurostat_env_chmhaz_data = get_eurostat(
      "env_chmhaz",
      time_format = "date",
      filters = list(geo="EU27_2020", time=year, indic_env="CONS", hazard="HAZ", unit="MIO_T")
    )
  }, error = function(e) {
    stop(paste0("Données eurostat indisponibles pour ",year," (table env_chmhaz)"))
  })

  env_chmhaz_data = eurostat_env_chmhaz_data

  tryCatch({
    eurostat_nama_data = get_eurostat(
      "nama_10_a64",
      filters = list(geo="EU27_2020", na_item="B1G", time=year, unit="CP_MEUR", nace_r2="TOTAL")
    )
  }, error = function(e) {
    stop(paste0("Données eurostat indisponibles pour ",year," (table nama_10_a64)"))
  })
  
  nama_data = eurostat_nama_data

  # sector fpt --------------------------------------- #

  sector_fpt_list = list()

  sector_fpt_list[["TOTAL"]] = env_chmhaz_data$values*1000000 / nama_data$values

  sector_fpt = cbind.data.frame(sector_fpt_list) %>% pivot_longer(cols = names(sector_fpt_list))
  colnames(sector_fpt) = c("SECTOR", "FOOTPRINT")
  print(sector_fpt)

  # build nva fpt dataframe -------------------------- #

  nva_fpt_data = as.data.frame(cbind(branches_aggregates$BRANCH, branches_aggregates$NVA))
  colnames(nva_fpt_data) = c("BRANCH", "NVA")

  wd = getwd()
  branch_sector_fpt_matrix = read.csv(paste0(wd,"/lib/","MatrixHAZ.csv"), header=T, sep=";")

  for(i in 1:nrow(nva_fpt_data))
  {
    # get sector
    branch = nva_fpt_data$BRANCH[i]
    sector = branch_sector_fpt_matrix$SECTOR[branch_sector_fpt_matrix$BRANCH==branch]
    
    # build values
    nva_fpt_data$GROSS_IMPACT[i] = sector_fpt$FOOTPRINT[sector_fpt$SECTOR==sector] * branches_aggregates$NVA[i]
    nva_fpt_data$FOOTPRINT[i] = sector_fpt$FOOTPRINT[sector_fpt$SECTOR==sector]
    nva_fpt_data$UNIT_FOOTPRINT[i] = "G_CPEUR"
  }

  return(nva_fpt_data)
  # -------------------------------------------------- #
}

get_branches_imp_coef_haz = function(year)
{
  branches_imp_coef = 1.0
  return(branches_imp_coef)
}