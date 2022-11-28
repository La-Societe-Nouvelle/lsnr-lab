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

source('R/FetchDataDisponibility.R')
source('R/DataBuilder.R')


buildBranchesData = function(indicator, year)
{

  options(warn = -1)

  Disponibility = FetchDataDisponibility(indicator)

  if ((year %in% Disponibility)==F) {
    Year1=year
    Year=Disponibility[which.min(abs(Year1-Disponibility))]
  } else {
    Year1=year
  }

  #Fetch all economic and financial raw data needed in order to complete computations process

  print("---------- Load data ----------")

  print("Products aggregates...")
  products_aggregates = get_products_aggregates(year)
  print("data loaded")

  print("Branches aggregates...")
  branches_aggregates = get_branches_aggregates(year)
  print("data loaded")

  print("Intermediates consumptions matrix...")
  ic_matrix = get_ic_matrix(year)
  print("data loaded")

  print("Consumptions of fixed capital matrix...")
  cfc_matrix = get_cfc_matrix(year)
  print("data loaded")

  print("Transfers matrix...")
  tr_matrix = FetchDataTESS(year)
  print("data loaded")

  print("---------- All data loaded ----------")

  wd = getwd()
  path = paste0(wd,"/lib/","Branches.csv")

  branches = read.csv(path, header=T, sep=";")

  #Call indicator-specific data
  # Columns :
  #   - BRANCH
  #   - FOOTPRINT
  #   - IMPORT COEF ( -> get from another way)
  
  nva_fpt = get_branches_nva_fpt(indicator,year)

  fpt_branches = get_empty_branches_fpt(branches)
  fpt_products = get_empty_products_fpt(branches)

  #
  print("---------- Initiatilisations ----------")

  print('init branches footprints')

  for(i in 1:nrow(branches)) 
  {
    fpt_branches$NVA_FPT[i] = nva_fpt$FOOTPRINT[nva_fpt$CNA_ACTIVITE==fpt_branches$BRANCH[i]]
    fpt_branches$PRD_FPT[i] = as.numeric(nva_fpt$FOOTPRINT[nva_fpt$CNA_ACTIVITE==fpt_branches$BRANCH[i]])
  }
  
  #La variation des stocks (P52) correspond à la valeur des entrées en stock diminuée de la valeur des sorties de stocks et des pertes courantes sur stocks.
  #Les stocks comprennent les matières premières et fournitures, les travaux en cours, les biens finis et les biens destinés à la revente.

  #Product values deduction, taking inter-branch transfers into account.

  nbIterations = 5
  results = c()

  append(results,fpt_branches)

  # First iteration process

  prev_fpt_branches = fpt_branches;
  prev_fpt_products = fpt_products;

  isResultsStables = F
  while (!isResultsStables)
  {
    print(paste0('ITERATION N°',iteration))

    fpt_branches = get_empty_branches_fpt(branches)
    fpt_products = get_empty_products_fpt(branches)

    # ---------- UPDATE PRODUCTS FOOTPRINTS ----------

    # RESS
    print('update RESS')
    next_ress_fpt = update_ress_fpt(prev_fpt_branches,tr_matrix)
    fpt_products$RESS_FPT = next_ress_fpt

    # IMP
    print('update IMP')
    fpt_products$IMP_FPT = fpt_products$RESS_FPT

    # TRESS
    print('update TRESS')
    next_tress_fpt = update_tress_fpt(fpt_products,products_aggregates)
    fpt_products$TRESS_FPT = next_tress_fpt

    # ---------- UPDATE BRANCHES FOOTPRINTS ----------

    # NVA
    print('update NVA')
    next_nva_fpt = update_nva_fpt(prev_fpt_branches)
    fpt_branches$NVA_FPT = next_nva_fpt

    # IC
    print('update IC')
    next_ic_fpt = update_ic_fpt(fpt_products,ic_matrix)
    fpt_branches$IC_FPT = next_ic_fpt

    # CFC
    print('update CFC')
    next_cfc_fpt = update_cfc_fpt(fpt_products,ic_matrix)
    fpt_branches$CFC_FPT = next_cfc_fpt

    # PRD
    print('update PRD')
    next_prd_fpt = update_prd_fpt(fpt_branches,branches_aggregates)
    fpt_branches$PRD_FPT = next_prd_fpt
    print(fpt_branches)

    isResultsStables = checkResultsStables(prev_branches_fpt,branches_fpt)
  }

  #Second iteration process

  imp_coef = get_branches_imp_coef(indicator,year)
  next_imp_fpt = update_imp_fpt(fpt_products,imp_coef)
  fpt_products$IMP_FPT = next_imp_fpt

  prev_fpt_branches = fpt_branches;
  prev_fpt_products = fpt_products;

  isResultsStables = F
  while (!isResultsStables)
  {
    print(paste0('ITERATION N°',iteration))

    fpt_branches = get_empty_branches_fpt(branches)
    fpt_products = get_empty_products_fpt(branches)

    # ---------- UPDATE PRODUCTS FOOTPRINTS ----------

    # RESS
    print('update RESS')
    next_ress_fpt = update_ress_fpt(prev_fpt_branches,tr_matrix)
    fpt_products$RESS_FPT = next_ress_fpt

    # IMP
    print('update IMP')
    fpt_products$IMP_FPT = prev_fpt_products$IMP_FPT

    # TRESS
    print('update TRESS')
    next_tress_fpt = update_tress_fpt(fpt_products,products_aggregates)
    fpt_products$TRESS_FPT = next_tress_fpt

    # ---------- UPDATE BRANCHES FOOTPRINTS ----------

    # NVA
    print('update NVA')
    next_nva_fpt = update_nva_fpt(prev_fpt_branches)
    fpt_branches$NVA_FPT = next_nva_fpt

    # IC
    print('update IC')
    next_ic_fpt = update_ic_fpt(fpt_products,ic_matrix)
    fpt_branches$IC_FPT = next_ic_fpt

    # CFC
    print('update CFC')
    next_cfc_fpt = update_cfc_fpt(fpt_products,ic_matrix)
    fpt_branches$CFC_FPT = next_cfc_fpt

    # PRD
    print('update PRD')
    next_prd_fpt = update_prd_fpt(fpt_branches,branches_aggregates)
    fpt_branches$PRD_FPT = next_prd_fpt
    print(fpt_branches)

    isResultsStables = checkResultsStables(prev_branches_fpt,branches_fpt)
  }

  names(branchesData)[c(6,(ncol(branchesData)-3):(ncol(branchesData)))]=paste0(c("GVA","TRESS","IC","CFC","PRD"),"_",indicator)
  names(branchesData)[names(branchesData)=="id"]="branch"

  output=cbind(branchesData[order(branchesData$branch),c(1,6,(ncol(branchesData)-3):(ncol(branchesData)))],rep(nva_fpt[[1]][[3]],37),rep(nva_fpt[[1]][[4]],37))
  names(output)[(ncol(output)-1):(ncol(output))]=c("Source","Unit")
  if(indicator%in%c("GHG","HAZ","MAT","NRG","WAS","WAT")){ #If the unit is monetary intensity, then corrections apply
    for(c in 2:6){
      for(r in 1:nrow(output)){
        output[r,c]=output[r,c]/(Deflator(1,Year1,year))
      }
    }}

  return(output)
}

#########################################################################################################################
################################################## FOOTPRINTS FORMULAS ##################################################

update_nva_fpt = function(prev_branches_fpt)
{
  next_fpt = c(0)
  for (branch in 1:nrow(prev_branches_fpt)) {
    next_fpt[branch] = as.numeric(prev_branches_fpt$NVA_FPT[branch])
  }
  return(next_fpt)
}

update_ress_fpt = function(prev_branches_fpt,data_TESS)
{
  next_fpt = c(0)
  for (i in 1:nrow(prev_branches_fpt)) {
    branch = prev_branches_fpt$BRANCH[i]
    next_fpt[i] = sum(data_TESS[i,-1] * prev_branches_fpt$PRD_FPT)
  }
  return(next_fpt)
}

update_imp_fpt = function(products_fpt,imp_coef)
{
  next_fpt = c(0)
  for (i in 1:nrow(products_fpt)) {
    next_fpt[i] = products_fpt$RESS_FPT[i]*imp_coef
  }
  return(next_fpt)
}

update_tress_fpt = function(products_fpt,data_ERE)
{
  next_fpt = c(0)
  for (i in 1:nrow(products_fpt)) {
    next_fpt[i] = (products_fpt$RESS_FPT[i]*data_ERE$P1[i] + products_fpt$IMP_FPT[i]*data_ERE$P7[i]) / data_ERE$TOTAL[i]
  }
  return(next_fpt)
}

update_ic_fpt = function(products_fpt, ic_matrix) 
{
  next_fpt = c(0)
  for(i in 1:nrow(products_fpt)) {
    branch = products_fpt$PRODUCT[i]
    next_fpt[i] = sum(ic_matrix[,branch] * products_fpt$TRESS_FPT)
  }
  return(next_fpt)
}

update_cfc_fpt = function(products_fpt, cfc_matrix) 
{
  next_fpt = c(0)
  for(i in 1:nrow(products_fpt)) {
    branch = products_fpt$PRODUCT[i]
    next_fpt[i] = sum(cfc_matrix[i,-1] * products_fpt$TRESS_FPT)
  }
  return(next_fpt)
}

update_prd_fpt = function(branches_fpt, branches_aggregates) 
{
  next_fpt = c(0)
  for(i in 1:nrow(branches_fpt)) {
    next_fpt[i] = (branches_fpt$NVA_FPT[i]*branches_aggregates$NVA[i] 
                 + branches_fpt$IC_FPT[i]*branches_aggregates$IC[i] 
                 + branches_fpt$CFC_FPT[i]*branches_aggregates$CFC[i]) / branches_aggregates$PRD[i]
  }
  return(next_fpt)
}

#########################################################################################################################

checkResultsStables = function(prev_branches_fpt,branches_fpt)
{
  stable = T
  for(i in 1:nrow(branches_fpt)) {
    gap = abs(prev_branches_fpt$PRD_FPT[i]-branches_fpt$PRD_FPT[i]) >= branches_fpt$PRD_FPT[i]*0.1
    if (gap) stable = F
  }
  return(stable)
}

get_empty_branches_fpt = function(branches)
{
  fpt_branches = as.data.frame(branches$CODE)
  names(fpt_branches)="BRANCH"
  fpt_branches[,c('NVA_FPT','IC_FPT','CFC_FPT','PRD_FPT')] = c(0,0,0,0)
  return(fpt_branches)
}

get_empty_products_fpt = function(products)
{
  fpt_products = as.data.frame(products$CODE)
  names(fpt_products)="PRODUCT"
  fpt_products[,c('RESS_FPT','IMP_FPT','TRESS_FPT')] = c(0,0,0)
  return(fpt_products)
}