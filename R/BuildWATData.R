#'Build and returns all data required to the WAT indicator computations.
#'
#'Returns a `list` made up of value added impacts by French branches, imported products associated coefficient,
#'Data sources and values unit. This data will be used in both BuildBranchesData and BuildDivisionsData functions.
#'
#' @param Year Considered year.
#'
#' @return An object `list` made up of 4 elements : value added impacts by French branches,
#' imported products associated coefficient, data sources and values unit.
#'
#' @seealso \code{\link{BuildECOData}}, \code{\link{BuildGHGData}}, \code{\link{BuildNRGData}},
#'  \code{\link{BuildBranchesData}}, \code{\link{BuildDivisionsData}}, \code{\link{FetchDataDisponibility}}.
#'
#' @examples
#' BuildWATData(max(FetchDataDisponibility("WAT"))
#' @export
BuildWATData=function(Year){
  WATDisponibility=FetchDataDisponibility("WAT")
  if((Year %in% WATDisponibility)==F){
    return("Desired year is unavailable. Please, refer to FetchDataDisponibility function in order to find available years for a given indicator")
  }else{
    ##Build ERE Database
    ERE=get_products_aggregates(Year)
    ReferenceTable=as.data.frame(unique(ERE$CNA_PRODUIT))

    ##Build CPEB Database : P1 - P2

    CPEB=get_branches_aggregates(Year)

    #Build WAT Database
    RAWWAT=get_eurostat("env_wat_abs",time_format = "num",filters = list(geo = c("FR"), unit = "MIO_M3",time = Year,wat_src = "FRW"))
    RAWWAT$values=1000*RAWWAT$values
    RAWWAT$valintensite[RAWWAT$wat_proc=="ABS_AGR"]=RAWWAT$values[RAWWAT$wat_proc=="ABS_AGR"]/sum(CPEB$B1G[CPEB$CNA_ACTIVITE=="AZ"])
    RAWWAT$valintensite[RAWWAT$wat_proc=="ABS_MIN"]=RAWWAT$values[RAWWAT$wat_proc=="ABS_MIN"]/sum(CPEB$B1G[CPEB$CNA_ACTIVITE=="BZ"])
    RAWWAT$valintensite[RAWWAT$wat_proc=="ABS_IND"]=RAWWAT$values[RAWWAT$wat_proc=="ABS_IND"]/sum(CPEB$B1G[substr(CPEB$CNA_ACTIVITE,1,1) %in% c("C","E")])
    RAWWAT$valintensite[RAWWAT$wat_proc=="ABS_ELC_CL"]=RAWWAT$values[RAWWAT$wat_proc=="ABS_ELC_CL"]/sum(CPEB$B1G[CPEB$CNA_ACTIVITE=="DZ"])
    RAWWAT$valintensite[RAWWAT$wat_proc=="ABS_CON"]=RAWWAT$values[RAWWAT$wat_proc=="ABS_CON"]/sum(CPEB$B1G[CPEB$CNA_ACTIVITE=="FZ"])
    RAWWAT$valintensite[RAWWAT$wat_proc=="ABS_SER"]=RAWWAT$values[RAWWAT$wat_proc=="ABS_SER"]/sum(CPEB$B1G[substr(CPEB$CNA_ACTIVITE,1,1) %in% c("G","H","I","J","K","L","M","N","O","P","Q","R","S","T","U")])


    #Applied consumption coefficient (SDES - Bilan environnemental 2021) to find consumption from initial abstractions : Agriculture = 0.82 ; Industry = 0.07 ; Energy = 0.10 ; Other (potable water) = 0.21
    FRAWAT=as.data.frame(ReferenceTable)
    FRAWAT$val[FRAWAT=="AZ"]=RAWWAT$valintensite[RAWWAT$wat_proc=="ABS_AGR"]*0.82
    FRAWAT$val[FRAWAT[,1]=="BZ"]=RAWWAT$valintensite[RAWWAT$wat_proc=="ABS_MIN"]*0.07
    FRAWAT$val[substr(FRAWAT[,1],1,1) %in% c("C","E")]=RAWWAT$valintensite[RAWWAT$wat_proc=="ABS_IND"]
    FRAWAT$val[substr(FRAWAT[,1],1,1) %in% c("C")]=FRAWAT$val[substr(FRAWAT[,1],1,1) %in% c("C")]*0.07
    FRAWAT$val[substr(FRAWAT[,1],1,1) %in% c("E")]=FRAWAT$val[substr(FRAWAT[,1],1,1) %in% c("E")]*0.21
    FRAWAT$val[FRAWAT[,1]=="DZ"]=RAWWAT$valintensite[RAWWAT$wat_proc=="ABS_ELC_CL"]*0.1
    FRAWAT$val[FRAWAT[,1]=="FZ"]=RAWWAT$valintensite[RAWWAT$wat_proc=="ABS_CON"]*0.21
    FRAWAT$val[substr(FRAWAT[,1],1,1) %in% c("G","H","I","J","K","L","M","N","O","P","Q","R","S","T","U")]=RAWWAT$valintensite[RAWWAT$wat_proc=="ABS_SER"]*0.21

    IMPWAT=RAWWAT$values[RAWWAT$geo=="FR" & RAWWAT$unit=="MIO_M3" & RAWWAT$time==Year & RAWWAT$wat_src=="FRW" & RAWWAT$wat_proc=="ABST"]/RAWWAT$values[RAWWAT$geo=="FR" & RAWWAT$unit=="MIO_M3" & RAWWAT$time==Year & RAWWAT$wat_src=="FRW" & RAWWAT$wat_proc=="ABST"]

    Source="Insee - Eurostat - SDES (French Ecological Transition Ministry)"
    Unit="L_CPEUR"
    WATData=list(FRAWAT,IMPWAT,Source,Unit)
    return(WATData)}}
