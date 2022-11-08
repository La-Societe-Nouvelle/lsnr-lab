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
#'  \code{\link{BuildBranchesData}}, \code{\link{BuildDivisionsData}}, \code{\link{FetchDataDisponibility}}.
#' @examples
#' BuildMATData(max(FetchDataDisponibility("MAT"))
#' @export
BuildMATData=function(Year){

  MATDisponibility=FetchDataDisponibility("MAT")
  if((Year %in% MATDisponibility)==F){
    return("Desired year is unavailable. Please, refer to FetchDataDisponibility function in order to find available years for a given indicator")
  }
  else{

    ##Build ERE Database
    ERE=FetchDataERE(Year)
    ReferenceTable=as.data.frame(unique(ERE$CNA_PRODUIT))

    ##Build CPEB Database : P1 - P2

    CPEB=FetchDataCPEB(Year)

    RawMAT=get_eurostat("env_ac_mfa")
    RawMAT=RawMAT[RawMAT$unit=="THS_T" & RawMAT$time==paste0(Year,"-01-01") & RawMAT$material %in% c("MF1","MF2","MF3","MF4","TOTAL") & RawMAT$geo %in% c("FR","EU27_2020"),]
    FRAMAT=as.data.frame(ReferenceTable)
    names(FRAMAT)="id"
    FRAMAT$val[FRAMAT$id=="AZ"]=sum(RawMAT$values[RawMAT$geo=="FR" & RawMAT$material=="MF1" & RawMAT$indic_env=="DE"])*1000/CPEB$B1G[CPEB$CNA_ACTIVITE=="AZ"]
    FRAMAT$val[FRAMAT$id=="BZ"]=sum(RawMAT$values[RawMAT$geo=="FR" & RawMAT$material %in% c("MF2","MF3","MF4") & RawMAT$indic_env=="DE"])*1000/CPEB$B1G[CPEB$CNA_ACTIVITE=="BZ"]
    FRAMAT$val[is.na(FRAMAT$val)]=0

    GDPEU27=get_eurostat("nama_10_a64")
    GDPEU27=GDPEU27[GDPEU27$geo %in% c("EU27_2020","FR") & GDPEU27$na_item=="B1G" & GDPEU27$unit=="CP_MEUR" & GDPEU27$time==paste0(Year,"-01-01") & GDPEU27$nace_r2=="TOTAL",]

    EUMAT27=(RawMAT$values[RawMAT$geo=="EU27_2020" & RawMAT$material=="TOTAL" & RawMAT$indic_env=="DE"]/GDPEU27$values[GDPEU27$geo=="EU27_2020"])/(RawMAT$values[RawMAT$geo=="FR" & RawMAT$material=="TOTAL" & RawMAT$indic_env=="DE"]/GDPEU27$values[GDPEU27$geo=="FR"])

    Source="Insee and Eurostat"
    Unit="G_CPEUR"
    DataMAT=list(FRAMAT,EUMAT27,Source,Unit)
    return(DataMAT)}}
