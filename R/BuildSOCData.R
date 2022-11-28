#'Build and returns all data required to the SOC indicator computations.
#'
#'Returns a `list` made up of value added impacts by French branches, imported products associated coefficient,
#'Data sources and values unit. This data will be used in both BuildBranchesData and BuildDivisionsData functions.
#'
#' @param Year Considered year.
#'
#' @return An object `list` made up of 4 elements : value added impacts by French branches,
#' imported products associated coefficient, data sources and values unit.
#' @seealso \code{\link{BuildECOData}}, \code{\link{BuildGHGData}},
#'  \code{\link{BuildBranchesData}}, \code{\link{BuildDivisionsData}}, \code{\link{FetchDataDisponibility}}.
#' @examples
#' BuildSOCData(max(FetchDataDisponibility("SOC"))
#' @export
BuildSOCData=function(Year){
  SOCDisponibility=FetchDataDisponibility("SOC")
  if((Year %in% SOCDisponibility)==F){
    return("Desired year is unavailable. Please, refer to FetchDataDisponibility function in order to find available years for a given indicator")
  }else{
    ##Build ERE Database
    ERE=get_products_aggregates(Year)
    ReferenceTable=as.data.frame(unique(ERE$CNA_PRODUIT))

    ##Build CPEB Database : P1 - P2

    CPEB=get_branches_aggregates(Year)

    SOCFRA=as.data.frame(ReferenceTable)
    names(SOCFRA)="id"
    ESS=as.data.frame(cbind(c(0.045,0.007,0.043,rep(0.007,15),0.019,0.006,0.026,0.013,0.013,0.013,0.0299,0.006,0.006,0.006,0.006,0.057,0.105,0.187,0.117,0.604,0.403519,0.006,0.006),ReferenceTable[order(ReferenceTable),1]))
    for(i in 1:nrow(ESS)){
      ESS$EmploiVA[i]=as.numeric(ESS[i,1])*CPEB$B1G[CPEB$CNA_ACTIVITE==ESS[i,2]]
    }
    for(i in 1:nrow(ESS)){
      ESS$partva[i]=as.numeric(ESS$EmploiVA[i])/sum(as.numeric(ESS$EmploiVA))
    }
    for(i in 1:nrow(SOCFRA)){
      SOCFRA$val[i]=100*as.numeric(ESS[ESS[,2]==SOCFRA$id[i],1])
    }

    #for(i in 1:nrow(SOCFRA)){
    #  SOCFRA$val[i]=(100000*(CPEB$B1G[CPEB$CNA_ACTIVITE==SOCFRA$id[i]])/sum(CPEB$B1G))/CPEB$P1[CPEB$CNA_ACTIVITE==SOCFRA$id[i]]
    #}
    SOCIMP=0
    Source="Insee and 2020 commented Atlas of Social Solidarity Economy"
    Unite="P100"
    DataSOC=list(SOCFRA,SOCIMP,Source,Unite)
    return(DataSOC)}}
