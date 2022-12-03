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
#'  \code{\link{BuildBranchesData}}, \code{\link{BuildDivisionsData}}, \code{\link{FetchDataDisponibility}}.
#' @examples
#' BuildGEQData(max(FetchDataDisponibility("GEQ"))
#' @export

source('R/InseeDataManager.R')

BuildGEQData=function(Year){
  GEQDisponibility=FetchDataDisponibility("GEQ")
  if((Year %in% GEQDisponibility)==F){
    return("Desired year is unavailable. Please, refer to FetchDataDisponibility function in order to find available years for a given indicator")
  }else{
    ##Build ERE Database
    ERE=get_products_aggregates(Year)
    ReferenceTable=as.data.frame(unique(ERE$CNA_PRODUIT))

    ##Build CPEB Database : P1 - P2

    CPEB=get_branches_aggregates(Year)

    RAWGEQ=get_eurostat("earn_ses_hourly")
    RAWGEQ=RAWGEQ[RAWGEQ$geo %in% c("EU28","FR") & RAWGEQ$age=="TOTAL" & RAWGEQ$indic_se=="MEAN_E_EUR" & RAWGEQ$isco08=="TOTAL" & RAWGEQ$worktime=="TOTAL" & RAWGEQ$time==paste0(Year,"-01-01"),]
    FRAGEQ=as.data.frame(ReferenceTable)
    names(FRAGEQ)="id"

    FRAGEQ$val[substr(FRAGEQ$id,1,1) %in% c("B","C","D","E","F")]=abs((RAWGEQ$values[RAWGEQ$nace_r2=="B-F" & RAWGEQ$sex=="F" & RAWGEQ$geo=="FR"]-RAWGEQ$values[RAWGEQ$nace_r2=="B-F" & RAWGEQ$sex=="M" & RAWGEQ$geo=="FR"])/RAWGEQ$values[RAWGEQ$nace_r2=="B-F" & RAWGEQ$sex=="T" & RAWGEQ$geo=="FR"])
    FRAGEQ$val[substr(FRAGEQ$id,1,1) %in% c("G","H","I","J","K","L","M","N")]=abs((RAWGEQ$values[RAWGEQ$nace_r2=="G-N" & RAWGEQ$sex=="F" & RAWGEQ$geo=="FR"]-RAWGEQ$values[RAWGEQ$nace_r2=="G-N" & RAWGEQ$sex=="M" & RAWGEQ$geo=="FR"])/RAWGEQ$values[RAWGEQ$nace_r2=="G-N" & RAWGEQ$sex=="T" & RAWGEQ$geo=="FR"])
    FRAGEQ$val[substr(FRAGEQ$id,1,1) %in% c("P","Q","R","S")]=abs((RAWGEQ$values[RAWGEQ$nace_r2=="P-S" & RAWGEQ$sex=="F" & RAWGEQ$geo=="FR"]-RAWGEQ$values[RAWGEQ$nace_r2=="P-S" & RAWGEQ$sex=="M" & RAWGEQ$geo=="FR"])/RAWGEQ$values[RAWGEQ$nace_r2=="P-S" & RAWGEQ$sex=="T" & RAWGEQ$geo=="FR"])
    FRAGEQ$val[is.na(FRAGEQ$val)]=abs((RAWGEQ$values[RAWGEQ$nace_r2=="B-S_X_O" & RAWGEQ$sex=="F" & RAWGEQ$geo=="FR"]-RAWGEQ$values[RAWGEQ$nace_r2=="B-S_X_O" & RAWGEQ$sex=="M" & RAWGEQ$geo=="FR"])/RAWGEQ$values[RAWGEQ$nace_r2=="B-S_X_O" & RAWGEQ$sex=="T" & RAWGEQ$geo=="FR"])
    FRAGEQ$val=100*FRAGEQ$val

    EU28GEQ=abs((RAWGEQ$values[RAWGEQ$nace_r2=="B-S_X_O" & RAWGEQ$sex=="F" & RAWGEQ$geo=="EU28"]-RAWGEQ$values[RAWGEQ$nace_r2=="B-S_X_O" & RAWGEQ$sex=="M" & RAWGEQ$geo=="EU28"])/RAWGEQ$values[RAWGEQ$nace_r2=="B-S_X_O" & RAWGEQ$sex=="T" & RAWGEQ$geo=="EU28"])/abs((RAWGEQ$values[RAWGEQ$nace_r2=="B-S_X_O" & RAWGEQ$sex=="F" & RAWGEQ$geo=="FR"]-RAWGEQ$values[RAWGEQ$nace_r2=="B-S_X_O" & RAWGEQ$sex=="M" & RAWGEQ$geo=="FR"])/RAWGEQ$values[RAWGEQ$nace_r2=="B-S_X_O" & RAWGEQ$sex=="T" & RAWGEQ$geo=="FR"])
    Source="Insee and Eurostat"
    Unit="P100"
    DataGEQ=list(FRAGEQ,EU28GEQ,Source,Unit)
    return(DataGEQ)}}

build_branches_nva_fpt_geq = function(year) 
{
  # get branches aggregates
  branches_aggregates = get_branches_aggregates(year)

  # get eurostat data

  eurostat_data = get_eurostat("earn_ses_hourly")

  ses_data = eurostat_data %>%
    filter(geo == "FR") %>%
    filter(age == "TOTAL") %>%
    filter(indic_se == "MEAN_E_EUR") %>%
    filter(isco08 == "TOTAL") %>%
    filter(worktime == "TOTAL") %>%
    filter(time == paste0(year,"-01-01")) %>%
    pivot_wider(names_from = sex, values_from = values)

  # sector fpt

  wd = getwd()
  branches = read.csv(paste0(wd,"/lib/","Branches.csv"), header=T, sep=";")

  sector_fpt_list = list()
  sector_fpt_list[["B-F"]] = abs(ses_data$F[ses_data$nace_r2=="B-F"] - ses_data$M[ses_data$nace_r2=="B-F"]) / ses_data$T[ses_data$nace_r2=="B-F"] *100
  sector_fpt_list[["G-N"]] = abs(ses_data$F[ses_data$nace_r2=="G-N"] - ses_data$M[ses_data$nace_r2=="G-N"]) / ses_data$T[ses_data$nace_r2=="G-N"] *100
  sector_fpt_list[["P-S"]] = abs(ses_data$F[ses_data$nace_r2=="P-S"] - ses_data$M[ses_data$nace_r2=="P-S"]) / ses_data$T[ses_data$nace_r2=="P-S"] *100
  sector_fpt_list[["B-S_X_O"]] = abs(ses_data$F[ses_data$nace_r2=="B-S_X_O"] - ses_data$M[ses_data$nace_r2=="B-S_X_O"]) / ses_data$T[ses_data$nace_r2=="B-S_X_O"] *100

  sector_fpt = cbind.data.frame(sector_fpt_list) %>% pivot_longer(cols = names(sector_fpt_list))
  colnames(sector_fpt) = c("SECTOR", "FOOTPRINT")
  print(sector_fpt)

  # build nva footprint dataframe

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