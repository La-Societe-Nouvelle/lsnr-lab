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
#'  \code{\link{BuildBranchesData}}, \code{\link{BuildDivisionsData}}, \code{\link{FetchDataDisponibility}}.
#' @examples
#' BuildGHGData(max(FetchDataDisponibility("GHG"))
#' @export
BuildGHGData=function(Year){

  GHGDisponibility=FetchDataDisponibility("GHG")
  if((Year %in% GHGDisponibility)==F){
    return("Desired year is unavailable. Please, refer to FetchDataDisponibility function in order to find available years for a given indicator")
  }
  else{

    ##Build ERE Database
    ERE=FetchDataERE(Year)
    ReferenceTable=as.data.frame(unique(ERE$CNA_PRODUIT))

    ##Build CPEB Database : P1 - P2

    CPEB=FetchDataCPEB(Year)

    include=c("A","B","C","C10-C12","C13-C15","C16","C17","C18","C19","C20","C21","C22","C23","C24","C25","C26","C27","C28","C29","C30","C31_C32","C33","D","E","F","G","H","I","J","J58","J59_J60","J61","J62_J63","K","L","L68A","M","M69_M70","M71","M72","M73","M74_M75","N","O","P","Q","Q86","Q87_Q88","R","S","T","TOTAL")
    GHGFRA=rbind(get_eurostat("env_ac_ainah_r2",time_format = "num", filters=list(unit="T",geo="FR",time=Year,airpol="GHG",nace_r2=include[1:26])),get_eurostat("env_ac_ainah_r2",time_format = "num", filters=list(unit="T",geo="FR",time=Year,airpol="GHG",nace_r2=include[27:52])))

    ##Matching

    for(i in 1:nrow(GHGFRA)){
      GHGFRA$nace[i]=ifelse(nchar(GHGFRA$nace_r2[i]) %in% c(1,3,7),TransitionFunction(GHGFRA$nace_r2[i],"Insee"),NA)
    }

    #National values building up
    GHGFRACalcul=as.data.frame(ReferenceTable)
    for(i in 1:nrow(GHGFRACalcul)){
      GHGFRACalcul$RawValue[i]=sum(GHGFRA$values[GHGFRA$nace==GHGFRACalcul[i,1]],na.rm = T)}
    for(i in 1:nrow(GHGFRACalcul)){
      GHGFRACalcul$Value[i]=GHGFRACalcul$RawValue[i]/CPEB$B1G[CPEB$CNA_ACTIVITE==GHGFRACalcul[i,1]]}

    #World/France coefficient computation
    WorldGHG=WDI(indicator = "EN.ATM.CO2E.KD.GD",country=c("FR","1W","EU"),start = Year, end = Year)
    WorldGHGCalcul=WorldGHG$EN.ATM.CO2E.KD.GD[WorldGHG$iso2c=="1W"]/WorldGHG$EN.ATM.CO2E.KD.GD[WorldGHG$iso2c=="FR"]

    Source="Insee - Eurostat - Banque Mondiale"
    Unit="GCO2E_CPEUR"



RawDivisionsData=as.data.frame(get_eurostat("env_ac_ainah_r2"))
RawDivisionsData=RawDivisionsData[RawDivisionsData$geo=="FR" & RawDivisionsData$time==paste0(Year,"-01-01") & RawDivisionsData$unit=="T" & RawDivisionsData$airpol=="GHG",]
RawDivisionsData=RawDivisionsData[nchar(RawDivisionsData$nace_r2)%in%c(3,7),]
RawDivisionsData=RawDivisionsData[-which(RawDivisionsData$nace_r2=="HH_HEAT"),]
RawDivisionsData$nace_r2[nchar(RawDivisionsData$nace_r2)==3]=substr(RawDivisionsData$nace_r2[nchar(RawDivisionsData$nace_r2)==3],2,3)
nc=ncol(RawDivisionsData)
for(i in which(nchar(RawDivisionsData$nace_r2)==7)){
  divisions=as.numeric(substr(RawDivisionsData$nace_r2[i],2,3)):as.numeric(substr(RawDivisionsData$nace_r2[i],6,7))
  for(x in 2:length(divisions)){RawDivisionsData[i,nc+x-1]=divisions[x]}
  RawDivisionsData$nace_r2[i]=substr(RawDivisionsData$nace_r2[i],2,3)
}
ReferenceDivisions=read.csv("https://raw.githubusercontent.com/La-Societe-Nouvelle/LaSocieteNouvelle-defautdata/master/DefaultData-LSN/donnees/CorrespondancesDivisions.csv",sep = ";")
ReferenceDivisions$Numero[nchar(ReferenceDivisions$Numero)==1]=paste0("0",ReferenceDivisions$Numero[nchar(ReferenceDivisions$Numero)==1])
EstimationIntensiteBranche=ReferenceDivisions$Numero[ReferenceDivisions$Numero%in%c(as.character(RawDivisionsData$nace_r2),unique(unlist(RawDivisionsData[,(nc+1):ncol(RawDivisionsData)]))[!is.na(unique(unlist(RawDivisionsData[,(nc+1):ncol(RawDivisionsData)])))])==F]
EstimationIntensiteDivision=ReferenceDivisions$Numero[(ReferenceDivisions$Numero%in%EstimationIntensiteBranche)==F]

CPEBDivisionsDataRaw=get_insee_dataset("CNA-2014-CPEB",startPeriod = Year,endPeriod = Year,filter="A...VAL.....BRUT")
CPEBDivisionsDataRaw=CPEBDivisionsDataRaw[grepl("A88",CPEBDivisionsDataRaw$CNA_ACTIVITE),] %>% select(OBS_VALUE, CNA_ACTIVITE,OPERATION)
CPEBDivisionsDataRaw$CNA_ACTIVITE=str_remove(CPEBDivisionsDataRaw$CNA_ACTIVITE,"A88-")
CPEBDivisionsData=pivot_wider(CPEBDivisionsDataRaw,names_from = OPERATION,values_from = OBS_VALUE)

Matching=read.csv("https://raw.githubusercontent.com/La-Societe-Nouvelle/LaSocieteNouvelle-defautdata/master/DefaultData-LSN/donnees/TableFonctionTransition.csv",sep=";",header=T,col.names = c("Numero.de.la.division","Libelle.de.la.division","Code.de.la.branche.associee","Code.OCDE"))
Matching$Numero.de.la.division[nchar(Matching$Numero.de.la.division)==1]=paste0("0",Matching$Numero.de.la.division[nchar(Matching$Numero.de.la.division)==1])
IntensiteB1GDivisions=as.data.frame(cbind(ReferenceDivisions$Numero,c(CPEBDivisionsData$B1G[order(CPEBDivisionsData$CNA_ACTIVITE)],NA)))
for(i in which(IntensiteB1GDivisions[,1]%in%EstimationIntensiteBranche)){IntensiteB1GDivisions[i,3]=GHGFRACalcul$Value[GHGFRACalcul[,1]==Matching$Code.de.la.branche.associee[Matching$Numero.de.la.division==IntensiteB1GDivisions[i,1]]]
}

for(i in which(IntensiteB1GDivisions[,1]%in%EstimationIntensiteDivision)){
  VecDivisions=vector()
  for(x in c(2,7:ncol(RawDivisionsData))){VecDivisions=ifelse(all(RawDivisionsData[,x]%in%IntensiteB1GDivisions[i,1]==F),VecDivisions,grep(IntensiteB1GDivisions[i,1],RawDivisionsData[,x]))}
  Divisions=as.character(RawDivisionsData[VecDivisions,c(2,7:ncol(RawDivisionsData))][is.na(RawDivisionsData[VecDivisions,c(2,7:ncol(RawDivisionsData))])==F])
  IntensiteB1GDivisions[i,3]=RawDivisionsData$values[VecDivisions]/sum(as.numeric(IntensiteB1GDivisions[IntensiteB1GDivisions[,1]%in%Divisions,2]))
}

GHGData=list(GHGFRACalcul[,-2],WorldGHGCalcul,Source,Unit,IntensiteB1GDivisions[,c(1,3)])
return(GHGData)
}}
