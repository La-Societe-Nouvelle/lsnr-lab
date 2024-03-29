#' @importFrom insee get_insee_dataset
#' @importFrom dplyr %>%
#' @importFrom dplyr arrange
#' @importFrom dplyr add_row
#' @importFrom dplyr case_when
#' @importFrom dplyr select
#' @importFrom dplyr mutate
#' @importFrom dplyr arrange
#' @importFrom dplyr filter
#' @importFrom dplyr rename
#' @importFrom dplyr rename_with
#' @importFrom dplyr relocate
#' @importFrom insee get_insee_dataset
#' @importFrom tidyr pivot_wider
#' @importFrom tidyr replace_na
#' @importFrom stringr str_remove
#' @importFrom tibble add_column
#' @importFrom tibble rownames_to_column
#' @importFrom lsnstat lsnstat_macrodata
#'
#' @noRd

# List of aggregates
#   P1        Production
#   P2        Consommations intermédiaires
#   ERERESS   Total des ressources en produits
#   P7        Importations de biens et de services
#   D31       Subventions sur les produits
#   D21       Impôts sur les produits
#   P91       Marge de transport
#   P92       Marges commerciales totales
#   P8        Correction CAF/FAB
#   P5        Formation brute de capital

# List of assets
#     AN11    Actifs fixes
# (!) AN111   Logements                                                                 -> FZ Construction
#     AN112   Autres bâtiments et ouvrages de génie civil (batîments hors résidentiel)  -> FZ Construction
#     AN113   Machines et équipements                                                   -> AN1131 & AN1132
# (!) AN1131  Matériel de transport                                                     -> CL Fabrication de matériels de transport
# (!) AN1132  Equipement TIC                                                            -> CI Fabrication de produits informatiques, électroniques et optiques
# (!) AN1139  Autres machines et équipement                                             -> CK Fabrication de machines et équipements n.c.a.
#     AN114   Systèmes d'armes                                                          -> CH Métallurgie et fabrication de produits métalliques (25.40Z Fabrication d'armes et de munitions)
# (!) AN115   Ressources biologiques cultivées                                          -> AZ Agriculture, sylviculture et pêche
#     AN117   Droits de propriété intellectuelle                                        -> MB Recherche-développement scientifique

# (!) Pour ces catégories, le détail de la consommation de CF est disponible au format secteur institutionnel.
# Ainsi, on assigne le résidu de consommation de capital fixe (11-112-114-117) au prorata de la composition de l'encours d'actifs fixes en 111, 113 et 115.

#########################################################################################################################
################################################## PRODUCTS AGGREGATES ##################################################

get_products_aggregates = function(year)
{
  insee:::read_dataset_metadata("CNA-2014-ERE")

  # fetch data
  filter = "A..VAL.P1+P7+P8..VALEUR_ABSOLUE.FE.EUROS_COURANTS.BRUT"

  insee_ere_data = get_insee_dataset(
    "CNA-2014-ERE",
    startPeriod = year,
    endPeriod = year,
    filter = filter
  )

  if(nrow(insee_ere_data) == 0 | length(unique(insee_ere_data$OPERATION)) != 3)
  {
    if(year <= 2030 & year > 2020)
    {
      products_aggregates = get_forecasted_data(year,"product")

      return(products_aggregates)
    }else
    {
    stop(paste0("Données économiques des produits indisponibles pour l'année ", year))
    }

  }

  # format dataframe
  ere_data = insee_ere_data %>%
    filter(substr(CNA_PRODUIT,1,3)=="A38") %>%
    select(CNA_PRODUIT, OPERATION, OBS_VALUE) %>%
    pivot_wider(names_from = OPERATION, values_from = OBS_VALUE) %>%
    mutate(CNA_PRODUIT = str_remove(CNA_PRODUIT,"A38-")) %>%
    arrange(CNA_PRODUIT) %>%
    replace(is.na(.),0)

  # build products aggregates frame
  products_aggregates = data.frame(CNA_PRODUIT = as.character(ere_data$CNA_PRODUIT))
  for(i in 1:nrow(ere_data))
  {
    products_aggregates$RESS[i] = ere_data$P1[i]
    products_aggregates$IMP[i] = ere_data$P7[i] + ere_data$P8[i]
    products_aggregates$TRESS[i] = ere_data$P1[i] + ere_data$P7[i] + ere_data$P8[i]
  }
  names(products_aggregates)[names(products_aggregates) == 'CNA_PRODUIT'] = 'PRODUCT'

  return(products_aggregates)

}

#########################################################################################################################
################################################## BRANCHES AGGREGATES ##################################################

get_branches_aggregates = function(year)
{
  # fetch data (CPEB)
  insee_cpeb_data = get_insee_dataset(
    "CNA-2014-CPEB",
    startPeriod = year,
    endPeriod = year,
    filter = "A...VAL.....BRUT"
  )

  if(nrow(insee_cpeb_data) == 0)
  {
    if(year <= 2030 & year > 2021)
    {

      branches_aggregates = get_forecasted_data(year,"branch")

      return(branches_aggregates)
    }else
    {
    stop(paste0("Comptes de production et d'exploitation des branches indisponibles pour l'année ", year))
    }
  }

  cpeb_data = insee_cpeb_data %>%
    filter(substr(CNA_ACTIVITE,1,3)=="A38") %>%
    filter(OPERATION %in% c('P1','P2','B1G')) %>%
    select(CNA_ACTIVITE, OPERATION, OBS_VALUE) %>%
    pivot_wider(names_from = OPERATION, values_from = OBS_VALUE) %>%
    mutate(CNA_ACTIVITE = str_remove(CNA_ACTIVITE,"A38-")) %>%
    arrange(CNA_ACTIVITE)

  # fetch data (PAT NAF)
  insee_ccf_data = get_insee_dataset(
    "CNA-2014-PAT-NF",
    startPeriod = year,
    endPeriod = year,
    filter="A....VAL.AN11...CCF.."
  )

  if(nrow(insee_ccf_data) == 0)
  {
    if(year <= 2030 & year > 2021)
    {

      branches_aggregates = get_forecasted_data(year,"branch")

      return(branches_aggregates)
    }else
    {
      stop(paste0("Données des consommations de capital fixe des branches indisponibles pour l'année ", year))
    }
  }

  ccf_data = insee_ccf_data %>%
    filter(substr(CNA_ACTIVITE,1,3)=="A38") %>%
    select(CNA_ACTIVITE, OPERATION, OBS_VALUE) %>%
    pivot_wider(names_from = OPERATION, values_from = OBS_VALUE) %>%
    mutate(CNA_ACTIVITE = str_remove(CNA_ACTIVITE,"A38-")) %>%
    arrange(CNA_ACTIVITE)

  # Build branches aggregates frame
  branches_aggregates = as.data.frame(cpeb_data$CNA_ACTIVITE)
  names(branches_aggregates)=c("BRANCH")

  for (i in 1:nrow(branches_aggregates))
  {
    branch = branches_aggregates$BRANCH[i]
    branches_aggregates$PRD[i] = cpeb_data$P1[i]
    branches_aggregates$IC[i]  = cpeb_data$P2[i]
    branches_aggregates$CFC[i] = ccf_data$AN11[i]
    branches_aggregates$NVA[i] = cpeb_data$B1G[i] - ccf_data$AN11[i]
  }

  # temp correction
  branches_aggregates$IC[37] = 0
  branches_aggregates$CFC[37] = 0
  branches_aggregates$NVA[37] = 0

  return(branches_aggregates)
}

##########################################################################################################################
################################################## DIVISIONS AGGREGATES ##################################################

get_divisions_aggregates = function(year)
{

  # fetch and format data (CPEB)
  insee_cpeb_data = try(get_insee_dataset(
    "CNA-2014-CPEB",
    startPeriod = year,
    endPeriod = year,
    filter = "A...VAL.....BRUT"
  ) %>%
    filter(substr(CNA_ACTIVITE,1,3)=="A88" & OPERATION %in% c("P2","P1","B1G")) %>%
    mutate(CNA_ACTIVITE = substr(CNA_ACTIVITE,5,6)) %>%
    select(CNA_ACTIVITE,OPERATION,OBS_VALUE) %>%
    pivot_wider(names_from = OPERATION,values_from = OBS_VALUE) %>%
    arrange(CNA_ACTIVITE) %>%
    rename(GVA = B1G, PRD = P1, IC = P2) %>%
    mutate(GVA = case_when(is.na(GVA) ~ PRD - IC,
                           T ~ GVA),
           PRD = case_when(is.na(PRD) ~ IC + GVA,
                           T ~ PRD),
           IC = case_when(is.na(IC) ~ PRD - GVA,
                           T ~ IC),
           CFC = 0,
           NVA = 0)%>%
    relocate(c("CNA_ACTIVITE","PRD","IC","GVA")),silent = T)

  divisions = lsnr:::Divisions

  if(nrow(insee_cpeb_data) != (nrow(divisions)-1) || inherits(insee_cpeb_data,"try-error"))
  {

    if(year <= 2030 & year > 2020)
    {

      cpeb_data = get_forecasted_data(year,"division")

      return(cpeb_data)
    }else{

    stop(paste0("Comptes de production et d'exploitation des divisions indisponibles pour l'année ", year))
    }
  }


  if(all(divisions$DIVISION[-nrow(divisions)] %in% insee_cpeb_data$CNA_ACTIVITE) == F){

    add = data.frame(CNA_ACTIVITE = divisions$DIVISION[-nrow(divisions)][divisions$DIVISION[-nrow(divisions)] %in% insee_cpeb_data$CNA_ACTIVITE == F],
                     PRD = 0,
                     IC = 0,
                     GVA = 0,
                     CFC = 0,
                     NVA = 0)

    insee_cpeb_data = insee_cpeb_data %>% add_row(add) %>% arrange(CNA_ACTIVITE)

  }


  branches_aggregates = get_branches_aggregates(year)

  #Assign CFC amount for division by extending CFC rate of the branch

  for(i in unique(branches_aggregates$BRANCH)){

    r = which(branches_aggregates$BRANCH == i)
    divs = which(insee_cpeb_data$CNA_ACTIVITE %in% divisions$DIVISION[divisions$BRANCH == i])

    ccf_rate = branches_aggregates$CFC[r] / (branches_aggregates$CFC[r] + branches_aggregates$NVA[r])
    ccf_rate = ifelse(is.nan(ccf_rate),0,ccf_rate)

    insee_cpeb_data$CFC[divs] = round(insee_cpeb_data$GVA[divs] * ccf_rate,1)
    insee_cpeb_data$NVA[divs] = round(insee_cpeb_data$GVA[divs] * (1 - ccf_rate),1)
  }

  return(insee_cpeb_data)
}

###############################################################################################################
################################################## IC MATRIX ##################################################

# Composition of intermediate consumptions (columns) by products (rows)

get_ic_matrix = function(year)
{

  # fetch data
  insee_tei_data = get_insee_dataset(
    "CNA-2014-TEI",
    startPeriod = year,
    endPeriod = year
  )

  partial_ic_indicator = all(grepl("NNTOTAL",paste0(insee_tei_data$CNA_ACTIVITE,insee_tei_data$CNA_PRODUIT)))  #If NNTOTAL is detected at each row, it means this is a partial data publication


  if(nrow(insee_tei_data) == 0 | partial_ic_indicator)
  {

    if(year <= 2030 & year > 2020)
    {

      ic_matrix = get_forecasted_data(year,"ic")

      return(ic_matrix)
    }

    stop(paste0("Tableau des entrées intermédiaires indisponible pour l'année ", year))
  }

  tei_data = insee_tei_data %>%
    filter(substr(CNA_ACTIVITE,1,3)=="A38") %>%
    filter(substr(CNA_PRODUIT,1,3)=="A38") %>%
    select(CNA_ACTIVITE, CNA_PRODUIT, OBS_VALUE) %>%
    mutate(CNA_ACTIVITE = str_remove(CNA_ACTIVITE,"A38-")) %>%
    mutate(CNA_PRODUIT = str_remove(CNA_PRODUIT,"A38-")) %>%
    pivot_wider(names_from = CNA_ACTIVITE, values_from = OBS_VALUE) %>%
    arrange(CNA_PRODUIT) %>%
    relocate(any_of(c("CNA_PRODUIT",lsnr:::Branches$CODE)))

  # build ic matrix
  ic_matrix = data.frame(BRANCH = lsnr:::Branches$CODE)

  match_vector = match(tei_data$CNA_PRODUIT,ic_matrix$BRANCH)

  for (j in 1:nrow(ic_matrix))
  {
    product = ic_matrix$BRANCH[j]

    if(product == "TZ"){
      ic_matrix[,product] = 0
    }else{
      ic_matrix[match_vector,product] = tei_data[,product]
      ic_matrix[,product] = replace_na(ic_matrix[,product],0)
    }
  }

  for(j in 2:(ncol(ic_matrix)-1)){
    ic_matrix[,j] = ic_matrix[,j] / sum(ic_matrix[,j],na.rm=T)
  }

  return(ic_matrix)
}

get_reversed_ic_matrix = function(year)
{

  # fetch data
  insee_tei_data = get_insee_dataset(
    "CNA-2014-TEI",
    startPeriod = year,
    endPeriod = year
  )

  if(nrow(insee_tei_data) == 0)
  {
    if(year <= 2030 & year > 2020)
    {

      ic_matrix = get_forecasted_data(year,"ric")

      return(ic_matrix)
    }

    stop(paste0("Tableau des entrées intermédiaires indisponible pour l'année ", year))
  }

  tei_data = insee_tei_data %>%
    filter(substr(CNA_ACTIVITE,1,3)=="A38" | CNA_ACTIVITE == "NNTOTAL") %>%
    filter(substr(CNA_PRODUIT,1,3)=="A38" | CNA_PRODUIT == "NNTOTAL") %>%
    select(CNA_ACTIVITE, CNA_PRODUIT, OBS_VALUE) %>%
    mutate(CNA_ACTIVITE = str_remove(CNA_ACTIVITE,"A38-")) %>%
    mutate(CNA_PRODUIT = str_remove(CNA_PRODUIT,"A38-")) %>%
    pivot_wider(names_from = CNA_ACTIVITE, values_from = OBS_VALUE) %>% # columns -> branches (i.e. rows -> products)
    arrange(CNA_PRODUIT)

  # build reversed ic matrix
  ic_matrix = as.data.frame(tei_data[,1]) %>% filter(CNA_PRODUIT != "NNTOTAL")
  for (j in 1:nrow(ic_matrix)) # column
  {
    branch = ic_matrix$CNA_PRODUIT[j] # use CNA_PRODUIT column for code
    ic_matrix[,branch] = c(0)
    for (i in 1:36)
    {
      ic_matrix[i,branch] = tei_data[i,branch] / tei_data$NNTOTAL[i]
    }
  }
  names(ic_matrix)[names(ic_matrix) == 'CNA_PRODUIT'] = 'PRODUCT'

  return(ic_matrix)
}

################################################################################################################
################################################## CFC MATRIX ##################################################

# Composition of intermediate consumptions (columns) by products (rows)

get_cfc_matrix = function (year)
{
  # fetch data (CCF)
  insee_ccf_data = get_insee_dataset(
    "CNA-2014-PAT-NF",
    startPeriod = year,
    endPeriod = year,
    filter="A....VAL....CCF.."
  )

  if(nrow(insee_ccf_data) == 0)
  {
    if(year <= 2030 & year > 2020)
    {
      cfc_matrix = get_forecasted_data(year,"cfc")
      return(cfc_matrix)
    }
    stop(paste0("Données des consommations de capital fixe des branches indisponibles pour l'année ", year))
  }

  ccf_data = insee_ccf_data %>%
    filter(substr(CNA_ACTIVITE,1,3)=="A38") %>%
    mutate(CNA_ACTIVITE = str_remove(CNA_ACTIVITE,"A38-")) %>%
    mutate(OPERATION = str_remove(OPERATION,"_SEC10|SEC10")) %>%
    select(CNA_ACTIVITE, OPERATION, OBS_VALUE) %>%
    pivot_wider(names_from = OPERATION, values_from = OBS_VALUE) %>%
    arrange(CNA_ACTIVITE) %>%
    replace(is.na(.),0) %>%
    mutate(AN1173 = AN117 - AN1171 - AN1174) %>%
    select(!"AN117")

  # fetch data (ENC)
  insee_enc_data = get_insee_dataset(
    "CNA-2014-PAT-NF",
    startPeriod = year,
    endPeriod = year,
    filter="A....VAL....ENC..BRUT"
  )

  if(nrow(insee_enc_data) == 0)
  {
    stop(paste0("Données des encours de capital fixe des branches indisponibles pour l'année ", year))
  }

  enc_data = insee_enc_data %>%
    filter(substr(CNA_ACTIVITE,1,3)=="A38") %>%
    mutate(CNA_ACTIVITE = str_remove(CNA_ACTIVITE,"A38-")) %>%
    mutate(OPERATION = str_remove(OPERATION,"_SEC10|SEC10")) %>%
    select(CNA_ACTIVITE, OPERATION, OBS_VALUE) %>%
    pivot_wider(names_from = OPERATION, values_from = OBS_VALUE) %>%
    arrange(CNA_ACTIVITE) %>%
    replace(is.na(.),0) %>%
    mutate()

  # build cfc matrix
  cfc_matrix = data.frame(BRANCH = as.character(unlist(ccf_data[,1])))

  for (j in 1:nrow(cfc_matrix)) # column -> branch
  {
    branch = cfc_matrix$BRANCH[j]

    cfc_matrix[,branch] = c(0)

    cfc_branch = ccf_data$AN11[j]
    cfc_branch_balance = ccf_data$AN11[j]- ccf_data$AN1121[j] - ccf_data$AN1122[j] - ccf_data$AN114[j] - ccf_data$AN1171[j] - ccf_data$AN1173[j]- ccf_data$AN1174[j]
    total_enc = enc_data$AN111[j] + enc_data$AN1131[j] + enc_data$AN1132[j] + enc_data$AN1139[j] + enc_data$AN115[j]

    # cfc by product (SEC10)
    cfc_branch_an111 = (enc_data$AN111[j] / total_enc) * cfc_branch_balance  # part of branch cfc balance
    cfc_branch_an112 = ccf_data$AN1121[j] + ccf_data$AN1122[j]
    cfc_branch_an1131 = (enc_data$AN1131[j] / total_enc) * cfc_branch_balance  # part of branch cfc balance
    cfc_branch_an1132 = (enc_data$AN1132[j] / total_enc) * cfc_branch_balance  # part of branch cfc balance
    cfc_branch_an1139 = (enc_data$AN1139[j] / total_enc) * cfc_branch_balance  # part of branch cfc balance
    cfc_branch_an114 = ccf_data$AN114[j]
    cfc_branch_an115 = (enc_data$AN115[j] / total_enc) * cfc_branch_balance  # part of branch cfc balance
    cfc_branch_an1171 = ccf_data$AN1171[j]
    cfc_branch_an1173 = ccf_data$AN1173[j]
    cfc_branch_an1174 = ccf_data$AN1174[j]


    # cfc coef by product (A38)
    cfc_matrix[cfc_matrix$BRANCH=="AZ",branch] = cfc_branch_an115 / cfc_branch
    cfc_matrix[cfc_matrix$BRANCH=="FZ",branch] = (cfc_branch_an111+cfc_branch_an112) / cfc_branch
    cfc_matrix[cfc_matrix$BRANCH=="CH",branch] = cfc_branch_an114 / cfc_branch
    cfc_matrix[cfc_matrix$BRANCH=="CI",branch] = cfc_branch_an1132 / cfc_branch
    cfc_matrix[cfc_matrix$BRANCH=="CK",branch] = cfc_branch_an1139 / cfc_branch
    cfc_matrix[cfc_matrix$BRANCH=="CL",branch] = cfc_branch_an1131 / cfc_branch
    cfc_matrix[cfc_matrix$BRANCH=="MB",branch] = cfc_branch_an1171 / cfc_branch
    cfc_matrix[cfc_matrix$BRANCH=="JC",branch] = cfc_branch_an1173 / cfc_branch
    cfc_matrix[cfc_matrix$BRANCH=="RZ",branch] = cfc_branch_an1174 / cfc_branch

  }
  cfc_matrix$TZ = c(0)
  cfc_matrix = rbind(cfc_matrix,0)
  cfc_matrix$BRANCH[37] = 'TZ'

  return(cfc_matrix)
}

######################################################################################################################
################################################## TRANSFERS MATRIX ##################################################

# Composition of product domestic ressources (columns) by activities production (rows)

get_transfers_matrix = function(year)
{
  dom = try(list(lsnr:::tess_2010_dom,
       lsnr:::tess_2011_dom,
       lsnr:::tess_2012_dom,
       lsnr:::tess_2013_dom,
       lsnr:::tess_2015_dom,
       lsnr:::tess_2016_dom,
       lsnr:::tess_2017_dom,
       lsnr:::tess_2018_dom,
       lsnr:::tess_2019_dom
       )[[which(c(2010:2013,2015:2019) == year)]],silent=T)

  if(inherits(dom,"try-error"))
  {
    if(year <= 2030 & year >= 2000)
    {

      tr_matrix = get_forecasted_data(year, "tr")

      return(tr_matrix)
    }

    stop(paste0("Tableau entrées-sorties symétrique domestique indisponible pour l'année ", year))
  }

  branches = lsnr:::Branches

  names(dom)=dom[8,]
  dom = dom[-c(1:7),-1]

  tr_matrix = as.data.frame(apply(dom,2,function(x)gsub('\\s+', '',x)))[c(3:39),c(3:39)]

  tr_matrix = as.data.frame(lapply(tr_matrix, as.numeric))

  tr_matrix[is.na(tr_matrix)] = 0

  tr_matrix = (tr_matrix / rowSums(tr_matrix)) %>% add_column(PRODUCT = branches$CODE,.before = "AZ")

  return(tr_matrix)
}

######################################################################################################################
################################################## FORECASTED DATA ##################################################

get_forecasted_data = function(year,type){
  y = year
if(type == "tr"){

  forecasted_tr = lsnstat_macrodata("na_tess",filters = paste0("year=",y)) %>% rename_with(tolower)

  tr_matrix = forecasted_tr %>% filter(year == y) %>% rename(branch = activity) %>% select('branch','product','value') %>% pivot_wider(names_from = branch, values_from = "value") %>% rename(PRODUCT = product)

  for(i in 2:ncol(tr_matrix)){
    tr_matrix[,i] = tr_matrix[,i] / sum(tr_matrix[,i],na.rm = T)
  }

  data = tr_matrix
}
if(type == "ic"){

  forecasted_tei = lsnstat_macrodata("na_tei",filters = paste0("year=",y)) %>% rename_with(tolower)

  ic_matrix = forecasted_tei %>% filter(year == y) %>% select(!c("lastupdate","flag","year","unit","lastupload","classification")) %>% rename(branch = activity) %>% arrange(branch,product) %>% pivot_wider(names_from = "branch",values_from = "value") %>% rename(PRODUCT = product)

  for(x in 2:ncol(ic_matrix)){
    ic_matrix[,x] = ic_matrix[,x] / sum(ic_matrix[,x])
    ic_matrix[,x] = replace_na(ic_matrix[,x],list(0))

    ic_matrix[is.nan(unlist(ic_matrix[,x])),x] = 0
  }
  data = ic_matrix
}

  if(type == "ric"){

    forecasted_tei = lsnstat_macrodata("na_tei",filters = paste0("year=",y)) %>% rename_with(tolower)

    ic_matrix = forecasted_tei %>% filter(year == y) %>% select(!c("lastupdate","flag","year","unit","lastupload","classification")) %>% rename(branch = activity) %>% arrange(branch,product) %>% pivot_wider(names_from = "branch",values_from = "value") %>% rename(PRODUCT = product)

    for(x in 1:nrow(ic_matrix)){
      ic_matrix[x,-1] = ic_matrix[x,-1] / sum(ic_matrix[x,-1])
      ic_matrix[x,-1] = replace_na(ic_matrix[x,-1],list(0))

      ic_matrix[x,c(F,is.nan(unlist(ic_matrix[x,-1])))] = 0
    }
    data = ic_matrix
  }
if(type == "cfc"){
  list_branch = lsnr:::Branches$CODE

  cfc = lsnstat_macrodata("na_pat_nf",filters = paste0("classification=A38&year=",y)) %>% rename_with(tolower)

  cfc_matrix = setNames(matrix(0,nrow = 37,ncol = 37)%>%
                          `rownames<-`(list_branch) %>%
                          as.data.frame(),list_branch)

  for(i in 1:ncol(cfc_matrix)){
    product_list = cfc$product[cfc$activity == names(cfc_matrix)[i]]
    for(j in 1:nrow(cfc_matrix)){
      val = cfc$value[cfc$activity == names(cfc_matrix)[i] & cfc$product == row.names(cfc_matrix)[j]]
      if(length(val)>0){
        cfc_matrix[j,i] = val}
    }
  }

  data = cfc_matrix %>% rownames_to_column(var = "BRANCH")

}
if(type == "branch"){

  forecasted_branch = lsnstat_macrodata(dataset = "na_cpeb",filters = paste0("classification=A38&year=",y)) %>% rename_with(tolower)

  data = forecasted_branch %>% filter(activity != "TOTAL" & aggregate != "B1G") %>% select(value,activity,aggregate) %>%
    pivot_wider(names_from = aggregate,values_from = value) %>% rename(BRANCH = activity, NVA = B1N, PRD = P1, IC = P2) %>% arrange(BRANCH)

}
if(type == "division"){
  forecasted_div = lsnstat_macrodata(dataset = "na_cpeb",filters = paste0("classification=A88&year=",y)) %>% rename_with(tolower)

  data = forecasted_div %>% select(value,activity,aggregate) %>%
    pivot_wider(names_from = aggregate,values_from = value) %>% rename(NVA = B1N, PRD = P1, IC = P2,GVA = B1G, CNA_ACTIVITE = activity) %>% arrange(CNA_ACTIVITE)
}
if(type == "product"){

  forecasted_products = lsnstat_macrodata(dataset = "na_ere",filters = paste0("classification=A38&year=",y)) %>% rename_with(tolower)

  data = forecasted_products %>%
    filter(aggregate %in% c("P1","P7") & product != "TOTAL") %>%
    mutate(value = replace_na(as.numeric(value,0))) %>%
    select(product,aggregate,value) %>%
    arrange(product) %>%
    pivot_wider(names_from = aggregate,values_from = value) %>%
    rename(IMP = P7, RESS = P1, PRODUCT = product) %>%
    mutate(IMP = replace_na(IMP,0)) %>%
    mutate(TRESS = IMP + RESS)
}


return(data)
}
