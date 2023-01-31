#'Build and returns all data required to the GHG indicator computations.
#'
#'Returns a `list` made up of value added impacts by French branches, imported products associated coefficient,
#'Data sources, values unit and value added impacts by French divisions. This data will be used in both BuildBranchesData and BuildDivisionsData functions.
#'
#' @param Year Considered year.
#'
#' @importFrom WDI WDI
#' @return An object `list` made up of 5 elements : value added impacts by French branches,
#' imported products associated coefficient, data sources, values unit and value added impacts by French divisions.
#' @seealso \code{\link{BuildECOData}}, \code{\link{BuildNRGData}},
#'  \code{\link{BuildBranchesData}}, \code{\link{BuildDivisionsData}}, \code{\link{FetchDataAvailability}}.
#' @examples
#' BuildGHGData(max(FetchDataAvailability("GHG"))
#' @noRd

build_branches_nva_fpt_ghg = function(selectedYear)
{
  # -------------------------------------------------- #

  branches = lsnr:::Branches

  # get branches aggregates -------------------------- #

  branches_aggregates = get_branches_aggregates(selectedYear)

  # fetch data --------------------------------------- #

  tryCatch({

    set1=c("A","B","C","C10-C12","C13-C15","C16","C17","C18","C19","C20","C21","C22","C23","C24","C25","C26","C27","C28","C29","C30","C31_C32","C33","D","E","F","G","H","I","J","J58","J59_J60","J61","J62_J63","K","L","L68A","M","M69_M70","M71","M72","M73","M74_M75","N","O","P","Q","Q86","Q87_Q88","R")
    eurostat_data_1 = get_eurostat(
      "env_ac_ainah_r2",
      time_format = "num",
      filters = list(geo="FR", unit="T", time=selectedYear, airpol="GHG", nace_r2=set1)
    )

    set2=c("S","T","TOTAL")
    eurostat_data_2 = get_eurostat(
      "env_ac_ainah_r2",
      time_format = "num",
      filters = list(geo="FR", unit="T", time=selectedYear, airpol="GHG", nace_r2=set2)
    )

  }, error = function(e) {
    stop(paste0("Données eurostat indisponibles pour ",selectedYear," (table env_ac_ainah_r2)"))
  })

  ac_ainah_data = rbind(eurostat_data_1,eurostat_data_2)

  # sector fpt --------------------------------------- #

  sector_fpt_list = list()

  for (i in 1:nrow(branches)) 
  {
    code_nace = branches$NACE_R2[i]
    if (code_nace %in% ac_ainah_data$nace_r2) {
      if (branches_aggregates$NVA[i]>0) {
        sector_fpt_list[[code_nace]] = ac_ainah_data$values[ac_ainah_data$nace_r2==code_nace] / branches_aggregates$NVA[i]
      } else {
        sector_fpt_list[[code_nace]] = 0 # null ?
      }
    }
  }

  # CC / C16-C18
  sector_fpt_list[["C16-C18"]] = sum(ac_ainah_data$values[ac_ainah_data$nace_r2 %in% c("C16","C17","C18")]) / branches_aggregates$NVA[branches_aggregates$BRANCH=="CC"]
  # CG / C22_C23
  sector_fpt_list[["C22_C23"]] = sum(ac_ainah_data$values[ac_ainah_data$nace_r2 %in% c("C22","C23")]) / branches_aggregates$NVA[branches_aggregates$BRANCH=="CG"]
  # CH / C24_C25
  sector_fpt_list[["C24_C25"]] = sum(ac_ainah_data$values[ac_ainah_data$nace_r2 %in% c("C24","C25")]) / branches_aggregates$NVA[branches_aggregates$BRANCH=="CH"]
  # CL / C29_C30
  sector_fpt_list[["C29_C30"]] = sum(ac_ainah_data$values[ac_ainah_data$nace_r2 %in% c("C29","C30")]) / branches_aggregates$NVA[branches_aggregates$BRANCH=="CL"]
  # CM / C31-C33
  sector_fpt_list[["C31-C33"]] = sum(ac_ainah_data$values[ac_ainah_data$nace_r2 %in% c("C31_C32","C33")]) / branches_aggregates$NVA[branches_aggregates$BRANCH=="CM"]
  # JA / J58-J60
  sector_fpt_list[["J58-J60"]] = sum(ac_ainah_data$values[ac_ainah_data$nace_r2 %in% c("J58","J59_J60")]) / branches_aggregates$NVA[branches_aggregates$BRANCH=="JA"]
  # MA / M69-M71
  sector_fpt_list[["M69-M71"]] = sum(ac_ainah_data$values[ac_ainah_data$nace_r2 %in% c("M69_M70","M71")]) / branches_aggregates$NVA[branches_aggregates$BRANCH=="MA"]
  # MC / M73-M75
  sector_fpt_list[["M73-M75"]] = sum(ac_ainah_data$values[ac_ainah_data$nace_r2 %in% c("M73","M74_M75")]) / branches_aggregates$NVA[branches_aggregates$BRANCH=="MC"]
  # TZ

  sector_fpt = cbind.data.frame(sector_fpt_list) %>% pivot_longer(cols = names(sector_fpt_list))
  colnames(sector_fpt) = c("SECTOR", "FOOTPRINT")

  # build nva fpt dataframe -------------------------- #

  nva_fpt_data = as.data.frame(cbind(branches_aggregates$BRANCH, branches_aggregates$NVA))
  colnames(nva_fpt_data) = c("BRANCH", "NVA")

  branch_sector_fpt_matrix = lsnr:::MatrixGHG

  for(i in 1:nrow(nva_fpt_data))
  {
    # get sector
    branch = nva_fpt_data$BRANCH[i]
    sector = branch_sector_fpt_matrix$SECTOR[branch_sector_fpt_matrix$BRANCH==branch]

    # build values
    nva_fpt_data$GROSS_IMPACT[i] = sector_fpt$FOOTPRINT[sector_fpt$SECTOR==sector] * branches_aggregates$NVA[i]
    nva_fpt_data$UNIT_GROSS_IMPACT[i] = "TCO2E"
    nva_fpt_data$FOOTPRINT[i] = sector_fpt$FOOTPRINT[sector_fpt$SECTOR==sector]
    nva_fpt_data$UNIT_FOOTPRINT[i] = "GCO2E_CPEUR"
  }

  return(nva_fpt_data)
  # -------------------------------------------------- #
}

build_divisions_nva_fpt_ghg = function(selectedYear)
{
  # -------------------------------------------------- #

  wd = getwd()
  divisions = read.csv(paste0(wd,"/lib/","Divisions.csv"), header=T, sep=";")

  # get divisions aggregates ------------------------- #

  divisions_aggregates = get_divisions_aggregates(selectedYear)

  # fetch data --------------------------------------- #

  tryCatch({

    set1=c("A","B","C","C10-C12","C13-C15","C16","C17","C18","C19","C20","C21","C22","C23","C24","C25","C26","C27","C28","C29","C30","C31_C32","C33","D","E","F","G","H","I","J","J58","J59_J60","J61","J62_J63","K","L","L68A","M","M69_M70","M71","M72","M73","M74_M75","N","O","P","Q","Q86","Q87_Q88","R")
    eurostat_data_1 = get_eurostat(
      "env_ac_ainah_r2",
      time_format = "num",
      filters = list(geo="FR", unit="T", time=selectedYear, airpol="GHG", nace_r2=set1)
    )

    set2=c("S","T","TOTAL")
    eurostat_data_2 = get_eurostat(
      "env_ac_ainah_r2",
      time_format = "num",
      filters = list(geo="FR", unit="T", time=selectedYear, airpol="GHG", nace_r2=set2)
    )

  }, error = function(e) {
    stop(paste0("Données eurostat indisponibles pour ",selectedYear," (table env_ac_ainah_r2)"))
  })

  ac_ainah_data = rbind(eurostat_data_1,eurostat_data_2)

  # sector fpt --------------------------------------- #

  sector_fpt_list = list()

  for (i in 1:nrow(divisions)) 
  {
    code_nace = divisions$NACE_R2[i]
    if (code_nace %in% ac_ainah_data$nace_r2) {
      if (divisions_aggregates$NVA[i]>0) {
        sector_fpt_list[[code_nace]] = ac_ainah_data$values[ac_ainah_data$nace_r2==code_nace] / divisions_aggregates$NVA[i]
      } else {
        print(code_nace)
        sector_fpt_list[[code_nace]] = 0 # null ?
      }
    }
  }

  sector_fpt = cbind.data.frame(sector_fpt_list) %>% 
    pivot_longer(cols = names(sector_fpt_list))
  colnames(sector_fpt) = c("SECTOR", "FOOTPRINT")

  # build nva fpt dataframe -------------------------- #

  nva_fpt_data = as.data.frame(cbind(divisions_aggregates$DIVISION, divisions_aggregates$NVA))
  colnames(nva_fpt_data) = c("DIVISION", "NVA")

  wd = getwd()
  division_sector_fpt_matrix = read.csv(paste0(wd,"/lib/","MatrixGHG.csv"), header=T, sep=";")

  for(i in 1:nrow(nva_fpt_data))
  {
    # get sector
    division = nva_fpt_data$DIVISION[i]
    sector = division_sector_fpt_matrix$SECTOR[division_sector_fpt_matrix$DIVISION==division]

    # build values
    nva_fpt_data$GROSS_IMPACT[i] = sector_fpt$FOOTPRINT[sector_fpt$SECTOR==sector] * divisions_aggregates$NVA[i]
    nva_fpt_data$UNIT_GROSS_IMPACT[i] = "TCO2E"
    nva_fpt_data$FOOTPRINT[i] = sector_fpt$FOOTPRINT[sector_fpt$SECTOR==sector]
    nva_fpt_data$UNIT_FOOTPRINT[i] = "GCO2E_CPEUR"
  }

  return(nva_fpt_data)
  # -------------------------------------------------- #
}

get_branches_imp_coef_ghg = function(year)
{
  # fetch data

  wdi_data = WDI(
    indicator = "EN.ATM.CO2E.KD.GD",
    country=c("FR","1W"),
    start = year,
    end = year
  )

  fpt_fra =  wdi_data$EN.ATM.CO2E.KD.GD[wdi_data$iso2c=="FR"]
  fpt_wld =  wdi_data$EN.ATM.CO2E.KD.GD[wdi_data$iso2c=="1W"]

  branches_imp_coef = fpt_wld / fpt_fra

  return(branches_imp_coef)
}
