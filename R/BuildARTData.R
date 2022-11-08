#'Build and returns all data required to the ART indicator computations.
#'
#'Returns a `list` made up of value added impacts by French branches, imported products associated coefficient,
#'Data sources and values unit. This data will be used in both BuildBranchesData and BuildDivisionsData functions.
#'
#' @param Year Considered Year.
#'
#' @return An object `list` made up of 4 elements : value added impacts by French branches,
#' imported products associated coefficient, data sources and values unit.
#' @seealso \code{\link{BuildECOData}}, \code{\link{BuildGHGData}}, \code{\link{BuildNRGData}},
#'  \code{\link{BuildBranchesData}}, \code{\link{BuildDivisionsData}}, \code{\link{FetchDataDisponibility}}.
#' @examples
#' BuildARTData(max(FetchDataDisponibility("ART"))
#' @export
BuildARTData=function(Year){

  ARTDisponibility=FetchDataDisponibility("ART")
  if((Year %in% ARTDisponibility)==F){
    return("Desired year is unavailable. Please, refer to FetchDataDisponibility function in order to find available years for a given indicator")
  }
  else{

    ##Build ERE Database
    ERE=FetchDataERE(Year)
    ReferenceTable=as.data.frame(unique(ERE$CNA_PRODUIT))

    ##Build CPEB Database : P1 - P2

    CPEB=FetchDataCPEB(Year)

    RAWART=as.data.frame(cbind(CPEB$CNA_ACTIVITE,CPEB$B1G))
    for(i in 1:nrow(RAWART)){
      RAWART$val[i]=as.numeric(RAWART[i,2])*111600/sum(as.numeric(RAWART[,2]))
      RAWART$ComputedValue[i]=100*RAWART$val[i]/as.numeric(RAWART[i,2])}
    FRAART=RAWART[,c(1,4)]
    IMPART=0

    Source="Insee and DGE"
    Unit="P100"
    DataART=list(FRAART,IMPART,Source,Unit)
    return(DataART)}}
