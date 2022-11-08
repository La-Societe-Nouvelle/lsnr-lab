#'Compute default values of societal footprints by NACE branches (38)
#'
#'Unlike BuildBranchesData function, this derived model take into
#'account fixed capital consumption.
#'
#'This function returns a table summarizing final default values
#'of the chosen indicator on the selected year.
#'
#' @param Year Considered year.
#' @param Indicator Considered indicator.
#'
#' @return A `data.frame` object containing final default values by economic activities branches.
#' @seealso \code{\link{BuildDivisionsData}}, \code{\link{FetchDataDisponibility}}, \code{\link{BuildBranchesData}}.
#' @examples
#' BuildBranchesDataV2("GHG",2018)
#' @export

BuildBranchesData=function(Indicator,Year){

options(warn = -1)
    Disponibility=FetchDataDisponibility(Indicator)
  if((Year %in% Disponibility)==F){
    Year1=Year
    Year=Disponibility[which.min(abs(Year1-Disponibility))]
  }else{
    Year1=Year}
  #Fetch all economic and financial raw data needed in order to complete computations process

CCF=FetchDataCFC(Year)
ERE=FetchDataERE(Year)
TEI=FetchDataTEI(Year)
for(i in 1:nrow(TEI)){for(x in 1:ncol(TEI)){TEI[i,x]=ifelse(is.na(TEI[i,x]),0,TEI[i,x])}}
CPEB=FetchDataCPEB(Year)
TESS=FetchDataTESS(Year)

#Build BranchesData database, on which final computations will operate

BranchesData=as.data.frame(ERE$CNA_PRODUIT[order(ERE$CNA_PRODUIT)])
names(BranchesData)=c("id")
for(i in 1:nrow(BranchesData)){
  BranchesData$CCF[i]=ifelse(BranchesData$id[i]%in%CCF$CNA_ACTIVITE,CCF$TOTAL[CCF$CNA_ACTIVITE==BranchesData$id[i]],0)
  BranchesData$B1N[i]=CPEB$B1G[CPEB$CNA_ACTIVITE==BranchesData$id[i]]-BranchesData$CCF[i] #B1N = B1G-P51c(CCF)
  BranchesData$P2[i]=CPEB$P2[CPEB$CNA_ACTIVITE==BranchesData$id[i]]
  BranchesData$P1[i]=CPEB$P1[CPEB$CNA_ACTIVITE==BranchesData$id[i]]
}
BranchesData$P2[BranchesData$id=="TZ"]=0

#Call indicator-specific data
IndicatorData=lapply(Year,paste0("Build",toupper(Indicator),"Data"))
BranchesValues=as.data.frame(ERE$CNA_PRODUIT[order(ERE$CNA_PRODUIT)])
names(BranchesValues)="id"
if(Indicator%in%c("GHG","HAZ","MAT","NRG","WAS","WAT")){for(i in 1:nrow(BranchesValues)){
  BranchesValues[i,2]=IndicatorData[[1]][[1]][IndicatorData[[1]][[1]][,1]==BranchesValues$id[i],2]*CPEB$B1G[CPEB$CNA_ACTIVITE==BranchesValues$id[i]]/
    (BranchesData$B1N[BranchesData$id==BranchesValues$id[i]])
}}else{for(i in 1:nrow(BranchesValues)){BranchesValues[i,2]=IndicatorData[[1]][[1]][IndicatorData[[1]][[1]][,1]==BranchesValues$id[i],2]}}

ImportsCoefficient=IndicatorData[[1]][[2]]

#La variation des stocks (P52) correspond à la valeur des entrées en stock diminuée de la valeur des sorties de stocks et des pertes courantes sur stocks.
#Les stocks comprennent les matières premières et fournitures, les travaux en cours, les biens finis et les biens destinés à la revente.

#Product values deduction, taking inter-branch transfers into account.

ProductValues=as.data.frame(BranchesData$id)
for(i in 1:nrow(ProductValues)){
  tr=t(as.numeric(TESS[TESS[,1]==ProductValues[i,1],-1]))
  ProductValues$ij[i]=sum(tr*t(BranchesValues[order(BranchesValues$id),2]))
}
ImportsValues=as.data.frame(cbind(ProductValues[,1],as.numeric(ProductValues[,2]*ImportsCoefficient)))

#First iteration

for(i in 1:nrow(BranchesData)){
  BranchesData[i,6]=BranchesValues[BranchesValues$id==BranchesData$id[i],2]
  BranchesData[i,7]=(ProductValues$ij[ProductValues[,1]==BranchesData$id[i]]*ERE$P1[ERE$CNA_PRODUIT==BranchesData$id[i]]+(as.numeric(ImportsValues[ImportsValues[,1]==BranchesData$id[i],2]))*ERE$P7[ERE$CNA_PRODUIT==BranchesData$id[i]])/ERE$total[ERE$CNA_PRODUIT==BranchesData$id[i]]}
for(i in 1:nrow(BranchesData)){
  BranchesData[i,8]=sum(TEI[order(TEI$CNA_PRODUIT),names(TEI)==BranchesData$id[i]]*BranchesData[,6])
}
for(i in 1:nrow(BranchesData)){
  BranchesData[i,9]=ifelse(BranchesData$id[i] %in% CCF$CNA_ACTIVITE,(sum(CCF$MB[CCF$CNA_ACTIVITE==BranchesData$id[i]]*BranchesData[BranchesData$id=="MB",7])+
    sum(CCF$CH[CCF$CNA_ACTIVITE==BranchesData$id[i]]*BranchesData[BranchesData$id=="CH",7])+
    sum(CCF$CL[CCF$CNA_ACTIVITE==BranchesData$id[i]]*BranchesData[BranchesData$id=="CL",7])+
    sum(CCF$CI[CCF$CNA_ACTIVITE==BranchesData$id[i]]*BranchesData[BranchesData$id=="CI",7])+
    sum(CCF$CK[CCF$CNA_ACTIVITE==BranchesData$id[i]]*BranchesData[BranchesData$id=="CK",7])+
    sum(CCF$FZ[CCF$CNA_ACTIVITE==BranchesData$id[i]]*BranchesData[BranchesData$id=="FZ",7]))/BranchesData$CCF[i],0)
}

for(i in 1:nrow(BranchesData)){
  BranchesData[i,10]=(BranchesData$B1N[i]*BranchesData[i,6]+BranchesData$P2[i]*BranchesData[i,8]+BranchesData$CCF[i]*BranchesData[i,9])/BranchesData$P1[i]
}

names(BranchesData)[6:10]=c(paste0(Indicator,"B1G1"),paste0(Indicator,"TRESS1"),"IC1","CCF1",paste0(Indicator,"PRD1"))

#First iteration process
for(s in 1:20){
  for(i in 1:nrow(BranchesData)){
    Lambda=1
    BranchesData[i,7+4*s]=sum(t(as.numeric(TESS[TESS[,1]==BranchesData[i,1],-1]))*(BranchesData[,6+4*s])*(ERE$P1[ERE$CNA_PRODUIT==BranchesData$id[i]]+Lambda*ERE$P7[ERE$CNA_PRODUIT==BranchesData$id[i]]))/ERE$total[ERE$CNA_PRODUIT==BranchesData$id[i]]
  }
  for(i in 1:nrow(BranchesData)){
    BranchesData[i,8+4*s]=sum(TEI[order(TEI$CNA_PRODUIT),names(TEI)==BranchesData$id[i]]*BranchesData[,7+4*s])
    BranchesData[i,9+4*s]=ifelse(BranchesData$id[i] %in% CCF$CNA_ACTIVITE,(sum(CCF$MB[CCF$CNA_ACTIVITE==BranchesData$id[i]]*BranchesData[BranchesData$id=="MB",7+4*s])+
                                                                             sum(CCF$CH[CCF$CNA_ACTIVITE==BranchesData$id[i]]*BranchesData[BranchesData$id=="CH",7+4*s])+
                                                                             sum(CCF$CL[CCF$CNA_ACTIVITE==BranchesData$id[i]]*BranchesData[BranchesData$id=="CL",7+4*s])+
                                                                             sum(CCF$CI[CCF$CNA_ACTIVITE==BranchesData$id[i]]*BranchesData[BranchesData$id=="CI",7+4*s])+
                                                                             sum(CCF$CK[CCF$CNA_ACTIVITE==BranchesData$id[i]]*BranchesData[BranchesData$id=="CK",7+4*s])+
                                                                             sum(CCF$FZ[CCF$CNA_ACTIVITE==BranchesData$id[i]]*BranchesData[BranchesData$id=="FZ",7+4*s]))/BranchesData$CCF[i],0)

    BranchesData[i,10+4*s]=(BranchesData$B1N[i]*BranchesData[i,6]+BranchesData$P2[i]*BranchesData[i,8+4*s]+BranchesData$CCF[i]*BranchesData[i,9+4*s])/BranchesData$P1[i]
    names(BranchesData)[(7+4*s):(10+4*s)]=c(paste0(Indicator,"TRESS",s+1),paste0("IC",s+1),paste0("CCF",s+1),paste0(Indicator,"PRD",s+1))
  }
  if(all(abs(BranchesData[,10+4*s]-BranchesData[,10+4*(s-1)])<=(0.01*BranchesData[,10+4*(s-1)]))){break} #Stop iterations process when all PRD variations <= 0.01
}
#Second iteration process
ref=s
for(s in (ref+1):(ref+20)){
  for(i in 1:nrow(BranchesData)){
    BranchesData[i,7+4*s]=sum(t(as.numeric(TESS[TESS[,1]==BranchesData[i,1],-1]))*BranchesData[,6+4*s]*ERE$P1[ERE$CNA_PRODUIT==BranchesData$id[i]]+t(as.numeric(TESS[TESS[,1]==BranchesData[i,1],-1]))*BranchesData[,10+4*ref]*ImportsCoefficient*ERE$P7[ERE$CNA_PRODUIT==BranchesData$id[i]])/ERE$total[ERE$CNA_PRODUIT==BranchesData$id[i]]
  }
  for(i in 1:nrow(BranchesData)){
    BranchesData[i,8+4*s]=sum(TEI[order(TEI$CNA_PRODUIT),names(TEI)==BranchesData$id[i]]*BranchesData[,7+4*s])
    BranchesData[i,9+4*s]=ifelse(BranchesData$id[i] %in% CCF$CNA_ACTIVITE,(sum(CCF$MB[CCF$CNA_ACTIVITE==BranchesData$id[i]]*BranchesData[BranchesData$id=="MB",7+4*s])+
                                                                             sum(CCF$CH[CCF$CNA_ACTIVITE==BranchesData$id[i]]*BranchesData[BranchesData$id=="CH",7+4*s])+
                                                                             sum(CCF$CL[CCF$CNA_ACTIVITE==BranchesData$id[i]]*BranchesData[BranchesData$id=="CL",7+4*s])+
                                                                             sum(CCF$CI[CCF$CNA_ACTIVITE==BranchesData$id[i]]*BranchesData[BranchesData$id=="CI",7+4*s])+
                                                                             sum(CCF$CK[CCF$CNA_ACTIVITE==BranchesData$id[i]]*BranchesData[BranchesData$id=="CK",7+4*s])+
                                                                             sum(CCF$FZ[CCF$CNA_ACTIVITE==BranchesData$id[i]]*BranchesData[BranchesData$id=="FZ",7+4*s]))/BranchesData$CCF[i],0)

    BranchesData[i,10+4*s]=(BranchesData$B1N[i]*BranchesData[i,6]+BranchesData$P2[i]*BranchesData[i,8+4*s]+BranchesData$CCF[i]*BranchesData[i,9+4*s])/BranchesData$P1[i]
    names(BranchesData)[(7+4*s):(10+4*s)]=c(paste0(Indicator,"TRESS",s+1),paste0("IC",s+1),paste0("CCF",s+1),paste0(Indicator,"PRD",s+1))
  }
  if(all(abs(BranchesData[,10+4*s]-BranchesData[,10+4*(s-1)])<=(0.01*BranchesData[,14+4*(s-1)]))){break} #Stop iterations process when all PRD variations <= 0.01
}

names(BranchesData)[c(6,(ncol(BranchesData)-3):(ncol(BranchesData)))]=paste0(c("GVA","TRESS","IC","CFC","PRD"),"_",Indicator)
names(BranchesData)[names(BranchesData)=="id"]="branch"

output=cbind(BranchesData[order(BranchesData$branch),c(1,6,(ncol(BranchesData)-3):(ncol(BranchesData)))],rep(IndicatorData[[1]][[3]],37),rep(IndicatorData[[1]][[4]],37))
names(output)[(ncol(output)-1):(ncol(output))]=c("Source","Unit")
if(Indicator%in%c("GHG","HAZ","MAT","NRG","WAS","WAT")){ #If the unit is monetary intensity, then corrections apply
  for(c in 2:6){
    for(r in 1:nrow(output)){
      output[r,c]=output[r,c]/(Deflator(1,Year1,Year))
    }
  }}

return(output)}
