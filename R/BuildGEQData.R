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
BuildGEQData=function(Year){
  GEQDisponibility=FetchDataDisponibility("GEQ")
  if((Year %in% GEQDisponibility)==F){
    return("Desired year is unavailable. Please, refer to FetchDataDisponibility function in order to find available years for a given indicator")
  }else{
    ##Build ERE Database
    ERE=FetchDataERE(Year)
    ReferenceTable=as.data.frame(unique(ERE$CNA_PRODUIT))

    ##Build CPEB Database : P1 - P2

    CPEB=FetchDataCPEB(Year)

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
