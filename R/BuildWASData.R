#'Build and returns all data required to the WAS indicator computations.
#'
#'Returns a `list` made up of value added impacts by French branches, imported products associated coefficient,
#'Data sources and values unit. This data will be used in both BuildBranchesData and BuildDivisionsData functions.
#'
#' @param Year Considered year.
#'
#' @return An object `list` made up of 4 elements : value added impacts by French branches,
#' imported products associated coefficient, data sources and values unit.
#' @seealso \code{\link{BuildECOData}}, \code{\link{BuildGHGData}},
#'  \code{\link{BuildBranchesData}}, \code{\link{BuildDivisionsData}}, \code{\link{FetchDataAvailability}}.
#' @examples
#' BuildWASData(max(FetchDataAvailability("WAS"))
#' @noRd



build_branches_nva_fpt_was = function(year)
{
  # -------------------------------------------------- #

  branches = lsnr:::Branches

  # get branches aggregates -------------------------- #

  branches_aggregates = get_branches_aggregates(year)

  # fetch data --------------------------------------- #

  tryCatch({
    eurostat_data = get_eurostat("env_wasgen")
  }, error = function(e) {
    stop(paste0("Données eurostat indisponibles pour ",year," (table env_wasgen)"))
  })

  wasgen_data = eurostat_data %>%
    filter(geo=="FR") %>%
    filter(waste=="TOTAL") %>%
    filter(hazard=="HAZ_NHAZ") %>%
    filter(unit=="T") %>%
    filter(time == paste0(year,"-01-01"))

  # sector fpt --------------------------------------- #

  sector_fpt_list = list()

  for (i in 1:nrow(branches)) {
    code_nace = branches$NACE_R2[i]
    if (code_nace %in% wasgen_data$nace_r2) {
      sector_fpt_list[[code_nace]] = wasgen_data$values[wasgen_data$nace_r2==code_nace] / branches_aggregates$NVA[i]
    }
  }

  # CC / C16-C18
  sector_fpt_list[["C16-C18"]] = sum(wasgen_data$values[wasgen_data$nace_r2 %in% c("C16","C17_C18")]) / branches_aggregates$NVA[branches_aggregates$BRANCH=="CC"]
  # CE - CF - CG / C20-C23
  sector_fpt_list[["C20-C23"]] = sum(wasgen_data$values[wasgen_data$nace_r2 %in% c("C20-C22","C23")]) / sum(branches_aggregates$NVA[branches_aggregates$BRANCH %in% c("CE","CF","CG")])
  # CI - CJ - CK - CL / C26-C30
  sector_fpt_list[["C26-C30"]] = wasgen_data$values[wasgen_data$nace_r2=="C26-C30"] / sum(branches_aggregates$NVA[branches_aggregates$BRANCH %in% c("CI","CJ","CK","CL")])
  # GZ -> TZ / G-U_X_G4677 + G4677
  fpt_g_u_x_g4677 = wasgen_data$values[wasgen_data$nace_r2=="G-U_X_G4677"] / sum(branches_aggregates$NVA[branches_aggregates$BRANCH %in% c("GZ","HZ","IZ","JA","JB","JC","KZ","LZ","MA","MB","MC","NZ","OZ","PZ","QA","QB","RZ","SZ","TZ")])
  sector_fpt_list[["G"]] = fpt_g_u_x_g4677 + wasgen_data$values[wasgen_data$nace_r2=="G4677"] / branches_aggregates$NVA[branches_aggregates$BRANCH=="GZ"]
  sector_fpt_list[["H-T"]] = fpt_g_u_x_g4677

  sector_fpt = cbind.data.frame(sector_fpt_list) %>% pivot_longer(cols = names(sector_fpt_list))
  colnames(sector_fpt) = c("SECTOR", "FOOTPRINT")
  print(sector_fpt)

  # build nva fpt dataframe -------------------------- #

  nva_fpt_data = as.data.frame(cbind(branches_aggregates$BRANCH, branches_aggregates$NVA))
  colnames(nva_fpt_data) = c("BRANCH", "NVA")

  branch_sector_fpt_matrix = lsnr:::MatrixWAS

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

get_branches_imp_coef_was = function(year)
{
  branches_imp_coef = 0
  return(branches_imp_coef)
}
