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
#'  \code{\link{BuildBranchesData}}, \code{\link{BuildDivisionsData}}, \code{\link{FetchDataDisponibility}}.
#' @examples
#' BuildNRGData(max(FetchDataDisponibility("NRG"))
#' @export

source('R/InseeDataManager.R')

build_branches_nva_fpt_nrg = function(year) 
{
  # -------------------------------------------------- #

  wd = getwd()
  branches = read.csv(paste0(wd,"/lib/","Branches.csv"), header=T, sep=";")

  # get branches aggregates -------------------------- #

  branches_aggregates = get_branches_aggregates(year)

  # fetch data --------------------------------------- #
  
  tryCatch({
    set1=c("A","A01","A02","A03","B","C","C10-C12","C13-C15","C16","C17","C18","C19","C20","C21","C22","C23","C24","C25","C26","C27","C28","C29","C30","C31_C32","C33","CH_INV_PA","D","E","E36","E37-E39","ENV","F","G","G45","G46","G47","H","H49","H50","H51","H52","H53","HH", "HH_HEAT","HH_OTH","HH_TRA","I","J","J58")
    eurostat_data_1 = get_eurostat(
      "env_ac_pefasu",
      time_format = "num",
      filters = list(geo="FR", unit="TJ", time=year, stk_flow="SUP", prod_nrg="R30", nace_r2=set1)
    )
    
    set2=c("J59_J60","J61","J62_J63","K","K64","K65","K66","L","L68A","M","M69_M70","M71","M72","M73","M74_M75","N","N77","N78","N79","N80-N82","NRG_FLOW","O","P","Q","Q86","Q87_Q88","R","R90-R92","R93","ROW_ACT","S","S94","S95","S96","SD_SU","T","TOTAL","U")
    eurostat_data_2 = get_eurostat(
      "env_ac_pefasu",
      time_format = "num",
      filters = list(geo="FR", unit="TJ", time=year, stk_flow="SUP", prod_nrg="R30", nace_r2=set2)
    )
  }, error = function(e) {
    stop(paste0("Données eurostat indisponibles pour ",year," (table env_ac_pefasu)"))
  })

  ac_pefasu_data = rbind(eurostat_data_1,eurostat_data_2)

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

  sector_fpt = cbind.data.frame(sector_fpt_list) %>% pivot_longer(cols = names(sector_fpt_list))
  colnames(sector_fpt) = c("SECTOR", "FOOTPRINT")

  # build nva fpt dataframe -------------------------- #

  nva_fpt_data = as.data.frame(cbind(branches_aggregates$BRANCH, branches_aggregates$NVA))
  colnames(nva_fpt_data) = c("BRANCH", "NVA")

  branch_sector_fpt_matrix = read.csv(paste0(wd,"/lib/","MatrixNRG.csv"), header=T, sep=";")

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

get_branches_imp_coef_nrg = function(year)
{
  # fetch data
  eurostat_data = get_eurostat(
    "env_ac_pefasu",
    time_format = "num",
    filters = list(geo=c("FR","EU27_2020"), unit="TJ", time=year, stk_flow="SUP", prod_nrg="R30", nace_r2="TOTAL")
  )

  fpt_fra =  eurostat_data$values[eurostat_data$geo=="FR"]
  fpt_euu =  eurostat_data$values[eurostat_data$geo=="EU27_2020"]

  branches_imp_coef = fpt_euu / fpt_fra
  
  return(branches_imp_coef)
}