#'Build and returns all data required to the WAS indicator computations.
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
#' BuildWASData(max(FetchDataDisponibility("WAS"))
#' @export
BuildWASData=function(Year){
  WASDisponibility=FetchDataDisponibility("WAS")
  if((Year %in% WASDisponibility)==F){
    return("Desired year is unavailable. Please, refer to FetchDataDisponibility function in order to find available years for a given indicator")
  }else{
    ##Build ERE Database
    ERE=FetchDataERE(Year)
    ReferenceTable=as.data.frame(unique(ERE$CNA_PRODUIT))

    ##Build CPEB Database : P1 - P2

    CPEB=FetchDataCPEB(Year)

    RawWAS=get_eurostat("env_wasgen") #filters = list(geo = "FR",unit = "T", hazard = "HAZ_NHAZ", time = Year))
    RawWAS=RawWAS[RawWAS$waste=="TOTAL" & (RawWAS$geo=="FR" | RawWAS$geo=="EU28") & RawWAS$unit=="T" & RawWAS$hazard=="HAZ_NHAZ" & RawWAS$time==paste0(Year,"-01-01"),]
    TableTransition=read.csv("https://raw.githubusercontent.com/La-Societe-Nouvelle/LaSocieteNouvelle-defautdata/master/DefaultData-LSN/donnees/insee_branches.csv",sep=",",encoding="UTF-8",header = F,col.names = c("libelle","branche","nace"))
    TableTransition$nace[TableTransition$nace==""]=NA
    for(i in 1:nrow(RawWAS)){
      RawWAS$nace[i]=TableTransition$branche[TableTransition$nace==RawWAS$nace_r2[i]]
      if(is.na(RawWAS$nace[i])){RawWAS$nace[i]=0}
    }
    WASFRA=as.data.frame(ReferenceTable)
    names(WASFRA)="id"
    WASFRA$val[substr(WASFRA$id,1,1) %in% c("G","H","I","J","K","L","M","N","O","P","Q","R","S","T","U","X")]=RawWAS$values[RawWAS$nace_r2=="G-U_X_G4677" & RawWAS$geo=="FR"]+RawWAS$values[RawWAS$nace_r2=="G4677" & RawWAS$geo=="FR"]
    for(i in 1:nrow(WASFRA)){
      WASFRA$val[i]=ifelse(WASFRA$id[i] %in% RawWAS$nace,RawWAS$values[RawWAS$nace==WASFRA$id[i] & RawWAS$geo=="FR"],WASFRA$val[i])
      WASFRA$val[i]=ifelse(is.na(WASFRA$val[i]),sum(RawWAS$values[substr(RawWAS$nace,1,1)=="C" & (WASFRA$id[i] %in% RawWAS$nace)==FALSE & RawWAS$geo=="FR"]),WASFRA$val[i])
      WASFRA$B1G[i]=CPEB$B1G[CPEB$CNA_ACTIVITE==WASFRA$id[i]]
    }
    WASFRA$ComputedValue[substr(WASFRA$id,1,1) %in% c("G","H","I","J","K","L","M","N","O","P","Q","R","S","T","U","X")]=(RawWAS$values[RawWAS$nace_r2=="G-U_X_G4677" & RawWAS$geo=="FR"]+RawWAS$values[RawWAS$nace_r2=="G4677" & RawWAS$geo=="FR"])/sum(WASFRA$B1G[substr(WASFRA$id,1,1) %in% c("G","H","I","J","K","L","M","N","O","P","Q","R","S","T","U","X")])
    for(i in 1:nrow(WASFRA)){
      WASFRA$ComputedValue[i]=ifelse(WASFRA$id[i] %in% RawWAS$nace,WASFRA$val[i]/WASFRA$B1G[i],WASFRA$ComputedValue[i])
      WASFRA$ComputedValue[i]=ifelse(is.na(WASFRA$ComputedValue[i]),sum(RawWAS$values[substr(RawWAS$nace,1,1)=="C" & (WASFRA$id[i] %in% RawWAS$nace)==FALSE])/sum(WASFRA$B1G[substr(WASFRA$id,1,1)=="C" & (WASFRA$id[i] %in% RawWAS$nace)==FALSE]),WASFRA$ComputedValue[i]) #attention : sum de l'ensemble des C : faire soustraction pour WASFRA$val et WASFRA$ComputedValue
    }

    GDPEU28=get_eurostat("nama_10_a64")
    GDPEU28=GDPEU28[GDPEU28$geo %in% c("EU28","FR") & GDPEU28$na_item=="B1G" & GDPEU28$unit=="CP_MEUR" & GDPEU28$time==paste0(Year,"-01-01") & GDPEU28$nace_r2=="TOTAL",]

    EU28WAS=((RawWAS$values[RawWAS$nace_r2=="TOTAL_HH" & RawWAS$geo=="EU28"]-RawWAS$values[RawWAS$nace_r2=="EP_HH" & RawWAS$geo=="EU28"])/GDPEU28$values[GDPEU28$geo=="EU28"])/((RawWAS$values[RawWAS$nace_r2=="TOTAL_HH" & RawWAS$geo=="FR"]-RawWAS$values[RawWAS$nace_r2=="EP_HH" & RawWAS$geo=="FR"])/GDPEU28$values[GDPEU28$geo=="FR"])
    Source="Insee and Eurostat"
    Unit="G_CPEUR"
    DataWAS=list(WASFRA[,c(1,4)],EU28WAS,Source,Unit)
    return(DataWAS)}}
