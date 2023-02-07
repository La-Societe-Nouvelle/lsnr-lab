#' Compute macroeconomic values of societal footprints by NACE branches (38)
#'
#' @details This function aims to compute French branch societal footprints by non-financial dimension by year.
#' It involves, on one hand, a macroeconomic input-output modelization of the French economy and its interactions with
#' the rest of the world, based on INSEE IOTs, and requires, on the other hand, direct impact data from institutional sources.
#'
#' @importFrom tidyr pivot_longer
#' @importFrom stringr str_remove
#' @importFrom dplyr %>%
#' @importFrom dplyr mutate
#'
#' @param year year of requested data.
#' @param indicator requested non-financial dimension.
#' @param verbose TRUE by default, FALSE refers to the silent mode
#'
#' @return A table of macroeconomic footprint values by economic activities branch.
#'
#' @seealso \code{\link{build_divisions_fpt}}, \code{\link{get_indicator_list}}.
#'
#' @examples
#' build_branches_fpt("ART",2015)
#' build_branches_fpt("ECO",2019)
#' build_branches_fpt("GEQ",2019)
#' build_branches_fpt("GHG",2019)
#' build_branches_fpt("HAZ",2019)
#' build_branches_fpt("IDR",2019)
#' build_branches_fpt("KNW",2015)
#' build_branches_fpt("MAT",2019)
#' build_branches_fpt("NRG",2019)
#' build_branches_fpt("SOC",2015)
#' build_branches_fpt("WAS",2018)
#' build_branches_fpt("WAT",2019)
#' @export

build_branches_fpt = function(indicator, year, verbose = T)
{
  if(verbose == F)
  {
    rec_fl = tempfile(fileext = ".txt")
    sink(rec_fl,type = c("output", "message")) #Record console output in a temp file
  }
  indicator = tolower(as.character(indicator))

  print(paste0("Start building data for indicator ",indicator," for year ",year))

  options(warn = -1)

  branches = lsnr:::Branches

  print("---------- Impacts data loading ----------")

  # Call indicator-specific data
  # Columns :
  #   - BRANCH
  #   - FOOTPRINT
  #   - GROSS_IMPACT

  nva_fpt = suppressMessages(lsnr:::get_branches_nva_fpt(indicator,year))

  fpt_branches = get_empty_branches_fpt(branches)
  fpt_products = get_empty_products_fpt(branches)


  #Fetch all economic and financial raw data needed in order to complete computations process

  print("---------- Financial data loading ----------")

  print("Products aggregates...")
  products_aggregates = suppressMessages(lsnr:::get_products_aggregates(year))

  print("Branches aggregates...")
  branches_aggregates = suppressMessages(get_branches_aggregates(year))

  print("Intermediates consumptions matrix...")
  ic_matrix = suppressMessages(get_ic_matrix(year))

  print("Consumptions of fixed capital matrix...")
  cfc_matrix = suppressMessages(get_cfc_matrix(year))

  print("Transfers matrix...")
  tr_matrix = suppressMessages(get_transfers_matrix(year))

  #
  print("---------- Initiatilisations ----------")

  print('init branches footprints')

  for(i in 1:nrow(branches))
  {
    fpt_branches$NVA_FPT[i] = nva_fpt$FOOTPRINT[nva_fpt$BRANCH==fpt_branches$BRANCH[i]]
    fpt_branches$PRD_FPT[i] = as.numeric(nva_fpt$FOOTPRINT[nva_fpt$BRANCH==fpt_branches$BRANCH[i]])
  }

  #La variation des stocks (P52) correspond à la valeur des entrées en stock diminuée de la valeur des sorties de stocks et des pertes courantes sur stocks.
  #Les stocks comprennent les matières premières et fournitures, les travaux en cours, les biens finis et les biens destinés à la revente.

  #Product values deduction, taking inter-branch transfers into account.

  maxIterations = 10
  nbIterations = 0
  results = c()

  append(results,fpt_branches)

  # First iteration process

  isResultsStables = F
  while (!isResultsStables && nbIterations<maxIterations)
  {
    nbIterations= nbIterations+1
    print(paste0("[LOOP 1] Iteration n°",nbIterations))

    prev_fpt_branches = fpt_branches;
    prev_fpt_products = fpt_products;

    fpt_branches = get_empty_branches_fpt(branches)
    fpt_products = get_empty_products_fpt(branches)

    # ---------- UPDATE PRODUCTS FOOTPRINTS ----------

    # RESS
    next_ress_fpt = update_ress_fpt(prev_fpt_branches,tr_matrix)
    fpt_products$RESS_FPT = next_ress_fpt

    # IMP
    fpt_products$IMP_FPT = fpt_products$RESS_FPT

    # TRESS
    next_tress_fpt = update_tress_fpt(fpt_products,products_aggregates)
    fpt_products$TRESS_FPT = next_tress_fpt

    # ---------- UPDATE BRANCHES FOOTPRINTS ----------

    # NVA
    next_nva_fpt = update_nva_fpt(prev_fpt_branches)
    fpt_branches$NVA_FPT = next_nva_fpt

    # IC
    next_ic_fpt = update_ic_fpt(fpt_products,ic_matrix)
    fpt_branches$IC_FPT = next_ic_fpt

    # CFC
    next_cfc_fpt = update_cfc_fpt(fpt_products,ic_matrix)
    fpt_branches$CFC_FPT = next_cfc_fpt

    # PRD
    next_prd_fpt = update_prd_fpt(fpt_branches,branches_aggregates)
    fpt_branches$PRD_FPT = next_prd_fpt
    #print(fpt_branches)

    isResultsStables = checkResultsStables(prev_fpt_branches,fpt_branches) && nbIterations > 1
  }

  #Second iteration process

  imp_coef = get_branches_imp_coef(indicator,year)
  next_imp_fpt = update_imp_fpt(fpt_products,imp_coef)
  fpt_products$IMP_FPT = next_imp_fpt

  isResultsStables = F
  nbIterations = 0

  while (!isResultsStables && nbIterations<maxIterations)
  {
    nbIterations = nbIterations+1
    print(paste0("[LOOP 2] Iteration n°",nbIterations))

    prev_fpt_branches = fpt_branches;
    prev_fpt_products = fpt_products;

    fpt_branches = get_empty_branches_fpt(branches)
    fpt_products = get_empty_products_fpt(branches)

    # ---------- UPDATE PRODUCTS FOOTPRINTS ----------

    # RESS
    next_ress_fpt = update_ress_fpt(prev_fpt_branches,tr_matrix)
    fpt_products$RESS_FPT = next_ress_fpt

    # IMP
    fpt_products$IMP_FPT = prev_fpt_products$IMP_FPT

    # TRESS
    next_tress_fpt = update_tress_fpt(fpt_products,products_aggregates)
    fpt_products$TRESS_FPT = next_tress_fpt

    # ---------- UPDATE BRANCHES FOOTPRINTS ----------

    # NVA
    next_nva_fpt = update_nva_fpt(prev_fpt_branches)
    fpt_branches$NVA_FPT = next_nva_fpt

    # IC
    next_ic_fpt = update_ic_fpt(fpt_products,ic_matrix)
    fpt_branches$IC_FPT = next_ic_fpt

    # CFC
    next_cfc_fpt = update_cfc_fpt(fpt_products,ic_matrix)
    fpt_branches$CFC_FPT = next_cfc_fpt

    # PRD
    next_prd_fpt = update_prd_fpt(fpt_branches,branches_aggregates)
    fpt_branches$PRD_FPT = next_prd_fpt
    #print(fpt_branches)

    isResultsStables = checkResultsStables(prev_fpt_branches,fpt_branches)
  }

  # Output -> concat branches / products fpt & apply deflation (x)

  output = fpt_branches
  for (i in 1:nrow(branches))
  {
    output$RESS_FPT[i] = fpt_products$RESS_FPT[i]
    output$IMP_FPT[i] = fpt_products$IMP_FPT[i]
    output$TRESS_FPT[i] = fpt_products$TRESS_FPT[i]
  }

  output_2 = output %>%
    pivot_longer(!BRANCH, names_to = "AGGREGATE", values_to = "VALUE") %>%
    mutate(AGGREGATE = str_remove(AGGREGATE,"_FPT"))

  indic_metadata = lsnr:::IndicatorsMetadata

  output_2$YEAR = year
  output_2$UNIT = indic_metadata$UNIT[indic_metadata$CODE==toupper(indicator)]
  output_2$INDIC = toupper(indicator)

  if(verbose == F)
  {
    sink() ; closeAllConnections() #Stop recording
    unlink(rec_fl) #Delete console ouput
  }

  return(output_2)
}

#########################################################################################################################
################################################## FOOTPRINTS FORMULAS ##################################################

update_nva_fpt = function(prev_branches_fpt)
{
  next_fpt = c(0)
  for (branch in 1:nrow(prev_branches_fpt)) {
    next_fpt[branch] = round(as.numeric(prev_branches_fpt$NVA_FPT[branch]), digits = 6)
  }
  return(next_fpt)
}

update_ress_fpt = function(prev_branches_fpt,tr_matrix)
{
  next_fpt = c(0)
  for (i in 1:nrow(prev_branches_fpt)) {
    branch = prev_branches_fpt$BRANCH[i]
    next_fpt[i] = round(sum(tr_matrix[,branch] * prev_branches_fpt$PRD_FPT), digits = 6)
  }
  return(next_fpt)
}

update_imp_fpt = function(fpt_products,imp_coef)
{
  next_fpt = c(0)
  for (i in 1:nrow(fpt_products)) {
    next_fpt[i] = round(fpt_products$RESS_FPT[i]*imp_coef, digits = 6)
  }
  return(next_fpt)
}

update_tress_fpt = function(products_fpt,products_aggregates)
{
  next_fpt = c(0)
  for (i in 1:nrow(products_fpt)) {
    next_fpt[i] = round((products_fpt$RESS_FPT[i]*products_aggregates$RESS[i] + products_fpt$IMP_FPT[i]*products_aggregates$IMP[i]) / products_aggregates$TRESS[i], digits = 6)
  }
  return(next_fpt)
}

update_ic_fpt = function(products_fpt, ic_matrix)
{
  next_fpt = c(0)
  for(i in 1:nrow(products_fpt)) {
    branch = products_fpt$PRODUCT[i]
    next_fpt[i] = round(sum(ic_matrix[,branch] * products_fpt$TRESS_FPT), digits = 6)
  }
  return(next_fpt)
}

update_cfc_fpt = function(products_fpt, cfc_matrix)
{
  next_fpt = c(0)
  for(i in 1:nrow(products_fpt)) {
    branch = products_fpt$PRODUCT[i]
    next_fpt[i] = round(sum(cfc_matrix[,branch] * products_fpt$TRESS_FPT), digits = 6)
  }
  return(next_fpt)
}

update_prd_fpt = function(fpt_branches, branches_aggregates)
{
  next_fpt = c(0)
  for(i in 1:nrow(fpt_branches)) {
    next_fpt[i] = round((fpt_branches$NVA_FPT[i]*branches_aggregates$NVA[i]
                 + fpt_branches$IC_FPT[i]*branches_aggregates$IC[i]
                 + fpt_branches$CFC_FPT[i]*branches_aggregates$CFC[i]) / branches_aggregates$PRD[i], digits = 6)
  }
  return(next_fpt)
}

#########################################################################################################################

checkResultsStables = function(prev_branches_fpt,branches_fpt)
{
  stable = T
  max_gap = 0
  for(i in 1:nrow(branches_fpt))
  {
    if (branches_fpt$PRD_FPT[i]>0) {
      gap = abs(prev_branches_fpt$PRD_FPT[i]-branches_fpt$PRD_FPT[i]) / branches_fpt$PRD_FPT[i]
    } else {
      gap = 0
    }
    if (gap > max_gap) {
      max_gap = gap
    }
    if (gap > 0.01) stable = F
  }
  print(paste0("  variation max : ",round(max_gap*100, digits = 1)," %"))
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
