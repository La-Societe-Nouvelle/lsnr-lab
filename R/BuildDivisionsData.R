#'Compute default values of societal footprints by NACE divisions (88).
#'
#'#'Unlike BuildDivisionsData function, this derived model take into
#'account fixed capital consumption.
#'
#'This function returns a table summarizing final default values
#'of the chosen indicator on the selected year.
#'
#' @param Year Considered year.
#' @param Indicator Considered indicator.
#'
#' @return A `data.frame` object containing final default values by economic activities divisions.
#' @seealso \code{\link{BuildBranchesData}}, \code{\link{FetchDataDisponibility}}.
#' @examples
#' BuildDivisionsDataV2("GHG",2018)
#' @export
BuildDivisionsData=function(Indicator,Year){
  Disponibility=FetchDataDisponibility(Indicator)
  if((Year %in% Disponibility)==F){
    Year1=Year
    Year=Disponibility[which.min(abs(Year1-Disponibility))]
  }else{
    Year1=Year}

    #Call default values by 38 branches
    BranchesData=BuildBranchesData(toupper(Indicator),Year)
    DivData=lapply(Year,paste0("Build",Indicator,"Data"))
    #Call production and business data by 88 branches
    CPEBDivisionsDataRaw=get_insee_dataset("CNA-2014-CPEB",startPeriod = Year,endPeriod = Year,filter="A...VAL.....BRUT")
    CPEBDivisionsDataRaw=CPEBDivisionsDataRaw[grepl("A88",CPEBDivisionsDataRaw$CNA_ACTIVITE),] %>% select(OBS_VALUE, CNA_ACTIVITE,OPERATION)
    CPEBDivisionsDataRaw$CNA_ACTIVITE=str_remove(CPEBDivisionsDataRaw$CNA_ACTIVITE,"A88-")
    CPEBDivisionsData=pivot_wider(CPEBDivisionsDataRaw,names_from = OPERATION,values_from = OBS_VALUE)
    CCF=FetchDataCFC(Year)
    CPEBBranches=FetchDataCPEB(Year)
    for(i in 1:nrow(BranchesData)){BranchesData$TCCF[i]=ifelse(BranchesData$branch[i]%in%CCF$CNA_ACTIVITE,CCF$TOTAL[CCF$CNA_ACTIVITE==BranchesData$branch[i]]/CPEBBranches$B1G[CPEBBranches$CNA_ACTIVITE==BranchesData$branch[i]],0)}

    #Matching between 38 branches and 88 divisions
    MatchingLong=read.csv("https://raw.githubusercontent.com/La-Societe-Nouvelle/LaSocieteNouvelle-defautdata/master/DefaultData-LSN/donnees/CorrespondancesDivisions.csv",sep=";",header=T)
    MatchingShort=as.data.frame(unique(MatchingLong$Nace))
    nc=ncol(CPEBDivisionsData)
    for(i in 1:nrow(CPEBDivisionsData)){
      CPEBDivisionsData[i,(nc+1)]=MatchingLong$Nace[MatchingLong$Numero==as.numeric(CPEBDivisionsData$CNA_ACTIVITE[i])]
    }

    BranchesData[order(BranchesData$branch),(ncol(BranchesData)+1)]=MatchingShort
    names(BranchesData)[ncol(BranchesData)]=names(CPEBDivisionsData)[ncol(CPEBDivisionsData)]="nace"

    #Apply branches intensities to financial data by division
    #Hypothesis for building division CCF : constant rate of CCF/B1G through all divisions in a branch

    for(i in 1:nrow(CPEBDivisionsData)){
      CPEBDivisionsData$IC[i]=BranchesData[BranchesData$nace==CPEBDivisionsData$nace[i],4]
      CPEBDivisionsData$NVA[i]=BranchesData[BranchesData$nace==CPEBDivisionsData$nace[i],2]
      CPEBDivisionsData$TRESS[i]=BranchesData[BranchesData$nace==CPEBDivisionsData$nace[i],3]
      CPEBDivisionsData$CCF[i]=BranchesData[BranchesData$nace==CPEBDivisionsData$nace[i],5]
      CPEBDivisionsData$NCCF[i]=BranchesData$TCCF[BranchesData$nace==CPEBDivisionsData$nace[i]]*CPEBDivisionsData$B1G[i]
      CPEBDivisionsData$B1N[i]=CPEBDivisionsData$B1G[i]-CPEBDivisionsData$NCCF[i]
    }

    if(lengths(DivData)==5){
      for(i in 1:nrow(CPEBDivisionsData)){CPEBDivisionsData$NVA[i]=DivData[[1]][[5]][DivData[[1]][[5]][,1]==CPEBDivisionsData$CNA_ACTIVITE[i],2]} #when direct emissions are available by divisions, this line fetch and use it.
    }
    if(Indicator%in%c("GHG","HAZ","MAT","NRG","WAS","WAT")){for(i in 1:nrow(CPEBDivisionsData)){CPEBDivisionsData$NVA[i]=CPEBDivisionsData$NVA[i]*CPEBDivisionsData$B1G[i]/CPEBDivisionsData$B1N[i]}}
    nc=ncol(CPEBDivisionsData)+1
    for(i in 1:nrow(CPEBDivisionsData)){
      CPEBDivisionsData[i,nc]=(CPEBDivisionsData$B1N[i]*CPEBDivisionsData$NVA[i]+CPEBDivisionsData$NCCF[i]*CPEBDivisionsData$CCF[i]+CPEBDivisionsData$IC[i]*CPEBDivisionsData$P2[i])/(CPEBDivisionsData$P1[i])}
    names(CPEBDivisionsData)[nc]="PRD"
    CPEBDivisionsData[,(ncol(CPEBDivisionsData)+1)]=rep(BranchesData[1,7],nrow(CPEBDivisionsData))
    CPEBDivisionsData[,(ncol(CPEBDivisionsData)+1)]=rep(BranchesData[1,8],nrow(CPEBDivisionsData))
    names(CPEBDivisionsData)[(ncol(CPEBDivisionsData)-1):(ncol(CPEBDivisionsData))]=c("Source","Unit")
    for(i in c(14:17,20)){
      for(j in 1:nrow(CPEBDivisionsData)){
        CPEBDivisionsData[j,i]=ifelse(is.na(CPEBDivisionsData[j,i]),0,CPEBDivisionsData[j,i])
      }
    }
    if(Indicator%in%c("GHG","HAZ","MAT","NRG","WAS","WAT")){ #If the unit is monetary intensity, then corrections applu
    for(c in c(14:17,20)){
      for(r in 1:nrow(CPEBDivisionsData)){
        CPEBDivisionsData[r,c]=CPEBDivisionsData[r,c]/(Deflator(1,Year1,Year))
      }
    }}

    output=CPEBDivisionsData[order(CPEBDivisionsData$CNA_ACTIVITE),c(1,14:17,20:22)]
    names(output)[1:6]=c("division",paste0(names(output)[2:6],"_",Indicator))
    return(output)
  }
