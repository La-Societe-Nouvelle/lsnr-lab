#'Build and returns all data required to the ECO indicator computations.
#'
#'Returns a `list` made up of value added impacts by French branches, imported products associated coefficient,
#'Data sources and values unit. This data will be used in both BuildBranchesData and BuildDivisionsData functions.
#'
#' @param Year Considered year.
#'
#' @return An object `list` made up of 4 elements : value added impacts by French branches,
#' imported products associated coefficient, data sources and values unit.
#' @seealso \code{\link{BuildGHGData}}, \code{\link{BuildNRGData}},
#'  \code{\link{BuildBranchesData}}, \code{\link{BuildDivisionsData}}, \code{\link{FetchDataDisponibility}}.
#' @examples
#' BuildECOData(max(FetchDataDisponibility("ECO"))
#' @export
BuildECOData=function(Year){

  EcoDisponibility=FetchDataDisponibility("ECO")
  if((Year %in% EcoDisponibility)==F){
    return("Desired year is unavailable. Please, refer to FetchDataDisponibility function in order to find available years for a given indicator")
  }
  else{

    ##Build ERE Database
    ERE=get_products_aggregates(Year)
    ReferenceTable=as.data.frame(unique(ERE$CNA_PRODUIT))

    ##Build CPEB Database : P1 - P2

    CPEB=get_branches_aggregates(Year)

    ECOFR=as.data.frame(ReferenceTable)
     names(ECOFR)="id"
     for(i in 1:nrow(ECOFR)){
       ECOFR$val[i]=100
       #ECOFR$val[i]=(CPEB$B1G[CPEB$CNA_ACTIVITE==ECOFR$id[i]])/CPEB$P1[CPEB$CNA_ACTIVITE==ECOFR$id[i]] ##Si on part d'un ECO B1G = B1G/P1
    }
    ECOIMP=0
    Source="Insee"
    Unit="P100"
ECOData=list(ECOFR,ECOIMP,Source,Unit)
return(ECOData)}}
