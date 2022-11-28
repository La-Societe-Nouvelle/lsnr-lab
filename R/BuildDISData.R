#'Build and returns all data required to the DIS indicator computations.
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
#' BuildDISData(max(FetchDataDisponibility("DIS"))
#' @export
BuildDISData=function(Year){

  DISDisponibility=FetchDataDisponibility("DIS")
  if((Year %in% DISDisponibility)==F){
    return("Desired year is unavailable. Please, refer to FetchDataDisponibility function in order to find available years for a given indicator")
  }
  else{

    ##Build ERE Database
    ERE=get_products_aggregates(Year)
    ReferenceTable=as.data.frame(unique(ERE$CNA_PRODUIT))

    ##Build CPEB Database : P1 - P2

    CPEB=get_branches_aggregates(Year)

    RawDIS=get_eurostat("ilc_di12", time_format = "date", filters = list(geo = c("FR","EU28"), time = Year))
    FRADIS=as.data.frame(ReferenceTable)
    names(FRADIS)="id"
    for(i in 1:nrow(FRADIS)){
      FRADIS$val[i]=RawDIS$values[RawDIS$geo=="FR"]
    }
    EU28DIS=RawDIS$values[RawDIS$geo=="EU28"]/RawDIS$values[RawDIS$geo=="FR"]
    Source= "Insee and Eurostat"
    Unit= "P100"
    DataDIS=list(FRADIS,EU28DIS,Source,Unit)
    return(DataDIS)}}
