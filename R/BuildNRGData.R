#'Build and returns all data required to the NRG indicator computations.
#'
#'Returns a `list` made up of value added impacts by French branches, imported products associated coefficient,
#'Data sources, values unit and value added impacts by French divisions. This data will be used in both BuildBranchesData and BuildDivisionsData functions.
#'
#' @param Year Considered year.
#'
#' @return An object `list` made up of 5 elements : value added impacts by French branches,
#' imported products associated coefficient, data sources, values unit and value added impacts by French divisions.
#' @seealso \code{\link{BuildECOData}}, \code{\link{BuildGHGData}},
#'  \code{\link{BuildBranchesData}}, \code{\link{BuildDivisionsData}}, \code{\link{FetchDataDisponibility}}.
#' @examples
#' BuildNRGData(max(FetchDataDisponibility("NRG"))
#' @export
BuildNRGData=function(Year){
  NRGDisponibility=FetchDataDisponibility("NRG")
  if((Year %in% NRGDisponibility)==F){
    return("Desired year is unavailable. Please, refer to FetchDataDisponibility function in order to find available years for a given indicator")
  }else{
  ##Build ERE Database
  ERE=FetchDataERE(Year)
  ReferenceTable=as.data.frame(unique(ERE$CNA_PRODUIT))

  ##Build CPEB Database : P1 - P2

  CPEB=FetchDataCPEB(Year)
  include=c("A","A01","A02","A03","B","C","C10-C12","C13-C15","C16","C17","C18","C19","C20","C21","C22","C23","C24","C25","C26","C27","C28","C29","C30","C31_C32","C33","CH_INV_PA","D","E","E36","E37-E39","ENV","F","G","G45","G46","G47","H","H49","H50","H51","H52","H53","HH", "HH_HEAT","HH_OTH","HH_TRA","I","J","J58","J59_J60","J61","J62_J63","K","K64","K65","K66","L","L68A","M","M69_M70","M71","M72","M73","M74_M75","N","N77","N78","N79","N80-N82","NRG_FLOW","O","P","Q","Q86","Q87_Q88","R","R90-R92","R93","ROW_ACT","S","S94","S95","S96","SD_SU","T","TOTAL","U")
  DataNRG=rbind(get_eurostat("env_ac_pefasu",time_format = "num",filters=list(stk_flow="SUP",time=Year,unit="TJ",geo=c("EU27_2020","FR"),prod_nrg="R30",nace_r2=include[1:49])),get_eurostat("env_ac_pefasu",time_format = "num",filters=list(stk_flow="SUP",time=Year,unit="TJ",geo=c("EU27_2020","FR"),prod_nrg="R30",nace_r2=include[50:87])))
  DataNRG$values=1000*DataNRG$values

  ##Matching
  TablePassage=read.csv("https://raw.githubusercontent.com/La-Societe-Nouvelle/LaSocieteNouvelle-defautdata/master/DefaultData-LSN/donnees/insee_branches.csv",sep=",",encoding = "UTF-8",header = F,col.names = c("libelle","branche","nace"))
  TablePassage$nace[TablePassage$nace==""]=NA


  for(i in 1:nrow(DataNRG)){
    DataNRG$nace[i]=TablePassage$branche[TablePassage$nace==DataNRG$nace_r2[i]]
    if(is.na(DataNRG$nace[i])){DataNRG$nace[i]=0}
  }
  NRGFRA=as.data.frame(ReferenceTable)
  names(NRGFRA)="id"
  for(i in 1:nrow(NRGFRA)){
    NRGFRA$val[i]=ifelse(NRGFRA$id[i] %in% DataNRG$nace,DataNRG$values[DataNRG$nace==NRGFRA$id[i] & DataNRG$geo=="FR"],0)
    NRGFRA$B1G[i]=CPEB$B1G[CPEB$CNA_ACTIVITE==NRGFRA$id[i]]
  }
  NRGFRA$val[NRGFRA$id=="JA"]=sum(DataNRG$values[DataNRG$nace_r2 %in% c("J58","J59_J60") & DataNRG$geo=="FR"])
  NRGFRA$val[NRGFRA$id=="MA"]=sum(DataNRG$values[DataNRG$nace_r2 %in% c("M69_M70","M71") & DataNRG$geo=="FR"])
  NRGFRA$val[NRGFRA$id=="MC"]=sum(DataNRG$values[DataNRG$nace_r2 %in% c("M73","M74_M75") & DataNRG$geo=="FR"])
  NRGFRA$val[NRGFRA$id=="CC"]=sum(DataNRG$values[DataNRG$nace_r2 %in% c("C16","C17","C18") & DataNRG$geo=="FR"])
  NRGFRA$val[NRGFRA$id=="CH"]=sum(DataNRG$values[DataNRG$nace_r2 %in% c("C24","C25") & DataNRG$geo=="FR"])
  NRGFRA$val[NRGFRA$id=="CL"]=sum(DataNRG$values[DataNRG$nace_r2 %in% c("C29","C30") & DataNRG$geo=="FR"])
  NRGFRA$val[NRGFRA$id=="CM"]=sum(DataNRG$values[DataNRG$nace_r2 %in% c("C31_C32","C33") & DataNRG$geo=="FR"])
  NRGFRA$val[NRGFRA$id=="CG"]=sum(DataNRG$values[DataNRG$nace_r2 %in% c("C22","C23") & DataNRG$geo=="FR"])

  ##Sectorial intensities computations
  for(i in 1:nrow(NRGFRA)){
    NRGFRA$cval[i]=NRGFRA$val[i]/NRGFRA$B1G[i]
  }

  GDPEU27=get_eurostat("nama_10_a64")
  GDPEU27=GDPEU27[GDPEU27$geo %in% c("EU27_2020","FR") & GDPEU27$na_item=="B1G" & GDPEU27$unit=="CP_MEUR" & GDPEU27$time==paste0(Year,"-01-01") & GDPEU27$nace_r2=="TOTAL",]

  NRGEU27=(DataNRG$values[DataNRG$nace_r2=="TOTAL" & DataNRG$geo=="EU27_2020"]/GDPEU27$values[GDPEU27$geo=="EU27_2020"])/(DataNRG$values[DataNRG$nace_r2=="TOTAL" & DataNRG$geo=="FR"]/GDPEU27$values[GDPEU27$geo=="FR"])

  Source="Insee and Eurostat"


RawDivisionsData=as.data.frame(get_eurostat("env_ac_pefasu"))
RawDivisionsData=RawDivisionsData[RawDivisionsData$geo=="FR" & RawDivisionsData$time=="2018-01-01" & RawDivisionsData$unit=="TJ" & RawDivisionsData$stk_flow=="SUP" & RawDivisionsData$prod_nrg=="R30",]
RawDivisionsData=RawDivisionsData[nchar(RawDivisionsData$nace_r2)%in%c(3,7),]
RawDivisionsData=RawDivisionsData[-which(RawDivisionsData$nace_r2%in%c("HH_HEAT","ENV","ROW_ACT")),]
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
for(i in which(IntensiteB1GDivisions[,1]%in%EstimationIntensiteBranche)){IntensiteB1GDivisions[i,3]=NRGFRA$cval[NRGFRA[,1]==Matching$Code.de.la.branche.associee[Matching$Numero.de.la.division==IntensiteB1GDivisions[i,1]]]
}
options(scipen=999)
for(i in which(IntensiteB1GDivisions[,1]%in%EstimationIntensiteDivision)){
  VecDivisions=vector()
  for(x in c(2,8:ncol(RawDivisionsData))){VecDivisions=ifelse(all(RawDivisionsData[,x]%in%IntensiteB1GDivisions[i,1]==F),VecDivisions,grep(IntensiteB1GDivisions[i,1],RawDivisionsData[,x]))}
  Divisions=as.character(RawDivisionsData[VecDivisions,c(2,7:ncol(RawDivisionsData))][is.na(RawDivisionsData[VecDivisions,c(2,8:ncol(RawDivisionsData))])==F])
  IntensiteB1GDivisions[i,3]=RawDivisionsData$values[VecDivisions]*1000/sum(as.numeric(IntensiteB1GDivisions[IntensiteB1GDivisions[,1]%in%Divisions,2]))
}
Unit="KJ_CPEUR"
  NRGData=list(NRGFRA[,c(1,4)],NRGEU27,Source,Unit,IntensiteB1GDivisions[,c(1,3)])
  return(NRGData)
  }}
