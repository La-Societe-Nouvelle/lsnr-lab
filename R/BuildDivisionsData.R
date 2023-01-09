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

source('R/BuildBranchesData.R')

BuildDivisionsData = function(indicator,year)
{
  print(paste0("Start building data for indicator ",indicator," for year ",year))

  #Fetch all economic and financial raw data needed in order to complete computations process


  #Fetch branches fpt
  fpt_branches = buildBranchesData(indicator,year)


    #Call default values by 38 branches
    BranchesData=buildBranchesData(toupper(indicator),year)
    DivData=lapply(year,paste0("Build",indicator,"Data"))
    #Call production and business data by 88 branches

    divisions_aggregates = get_divisions_aggregates(year)
    cfc_matrix = get_cfc_matrix(year)
    CCF=get_cfc_matrix(year)
    CPEBBranches=get_branches_aggregates(year)
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
    if(indicator%in%c("GHG","HAZ","MAT","NRG","WAS","WAT")){for(i in 1:nrow(CPEBDivisionsData)){CPEBDivisionsData$NVA[i]=CPEBDivisionsData$NVA[i]*CPEBDivisionsData$B1G[i]/CPEBDivisionsData$B1N[i]}}
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
    if(indicator%in%c("GHG","HAZ","MAT","NRG","WAS","WAT")){ #If the unit is monetary intensity, then corrections applu
    for(c in c(14:17,20)){
      for(r in 1:nrow(CPEBDivisionsData)){
        CPEBDivisionsData[r,c]=CPEBDivisionsData[r,c]/(Deflator(1,Year1,year))
      }
    }}

    output=CPEBDivisionsData[order(CPEBDivisionsData$CNA_ACTIVITE),c(1,14:17,20:22)]
    names(output)[1:6]=c("division",paste0(names(output)[2:6],"_",indicator))
    return(output)
  }

build_divisions_fpt = function(indicator,year)
{
  wd = getwd()
  path = paste0(wd,"/lib/","Divisions.csv")

  divisions = read.csv(path, header=T, sep=";")

  # 
  # divisions_aggregates = get_divisions_aggregates(year)

  # build branches data
  fpt_branches = buildBranchesData(toupper(Indicator), year)

  # get nva data
  nva_fpt = get_divisions_nva_fpt(indicator,year)

  # divisions fpt
  fpt_divisions = get_empty_divisions_fpt(divisions)

  for(i in 1:nrow(fpt_divisions)) 
  {
    fpt_divisions$NVA_FPT[i] = nva_fpt$FOOTPRINT[i]
    fpt_divisions$IC_FPT[i]  = fpt_branches$IC_FPT[i]
    fpt_divisions$PRD_FPT[i] = (nva_fpt$FOOTPRINT[i]*divisions_aggregates$NVA[i] + fpt_branches$IC_FPT[i]*divisions_aggregates$IC[i] + fpt_divisions$CFC_FPT[i]*divisions_aggregates$CFC[i]) / divisions_aggregates$PRD[i]
  }

  return(fpt_divisions)
}

get_empty_divisions_fpt = function(divisions)
{
  fpt_divisions = as.data.frame(divisions$CODE)
  names(fpt_divisions)="DIV"
  fpt_divisions[,c('NVA_FPT','IC_FPT','CFC_FPT','PRD_FPT')] = c(0,0,0,0)
  return(fpt_divisions)
}