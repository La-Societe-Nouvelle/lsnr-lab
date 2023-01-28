#'Fetch data frame associated to supply-use equilibrium
#'of annual national accounts from Insee's API.
#'
#' @param Year Considered year.
#'
#' @return A `data.frame` object containing annual Insee supply-use equilibrium data.
#' @importFrom dplyr %>%
#' @importFrom dplyr select
#' @importFrom insee get_insee_dataset
#' @importFrom tidyr pivot_wider
#' @importFrom stringr str_remove
#' @seealso \code{\link{BuildDivisionsData}}, \code{\link{BuildBranchesData}}.
#' @examples
#' FetchDataERE(2018)
#' @export

# List of aggregates
#   P1        Production
#   P2        Consommations intermédiaires
#   ERERESS   Total des ressources en produits
#   P7        Importations de biens et de services
#   D31       Subventions sur les produits
#   D21       Impôts sur les produits
#   P91       Marge de transport
#   P92       Marges commerciales totales
#   P8        Correction CAF/CAB
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

  # fetch data
  filters =  c("A..VAL.P1..VALEUR_ABSOLUE.FE.EUROS_COURANTS.BRUT",
                "A..VAL.P7..VALEUR_ABSOLUE.FE.EUROS_COURANTS.BRUT",
                "A..VAL.P8..VALEUR_ABSOLUE.FE.EUROS_COURANTS.BRUT")
  datalist =  vector("list", length = length(filters))
  for (i in 1:length(filters))
  {
    insee_data = get_insee_dataset(
      "CNA-2014-ERE",
      startPeriod = year,
      endPeriod = year,
      filter = filters[i]
    )
    datalist[[i]] = insee_data
  }
  insee_ere_data = do.call(rbind,datalist)

  # format dataframe
  ere_data = insee_ere_data %>%
    filter(substr(CNA_PRODUIT,1,3)=="A38") %>%
    select(CNA_PRODUIT, OPERATION, OBS_VALUE) %>%
    pivot_wider(names_from = OPERATION, values_from = OBS_VALUE) %>%
    mutate(CNA_PRODUIT = str_remove(CNA_PRODUIT,"A38-")) %>%
    arrange(CNA_PRODUIT) %>%
    replace(is.na(.),0)

  # build products aggregates frame
  products_aggregates = as.data.frame(ere_data$CNA_PRODUIT)
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
  
  # fetch data (CPEB)
  insee_cpeb_data = get_insee_dataset(
    "CNA-2014-CPEB",
    startPeriod = year,
    endPeriod = year,
    filter = "A...VAL.....BRUT"
  )

  cpeb_data = insee_cpeb_data %>%
    filter(substr(CNA_ACTIVITE,1,3)=="A88") %>%
    filter(OPERATION %in% c('P1','P2','B1G')) %>%
    select(CNA_ACTIVITE, OPERATION, OBS_VALUE) %>%
    pivot_wider(names_from = OPERATION, values_from = OBS_VALUE) %>%
    mutate(CNA_ACTIVITE = str_remove(CNA_ACTIVITE,"A88-")) %>%
    arrange(CNA_ACTIVITE)

  # fetch data (PAT NAF)
  insee_ccf_data = get_insee_dataset(
    "CNA-2014-PAT-NF",
    startPeriod = year,
    endPeriod = year,
    filter="A....VAL.AN11...CCF.."
  )

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

  tei_data = insee_tei_data %>%
    filter(substr(CNA_ACTIVITE,1,3)=="A38") %>% 
    filter(substr(CNA_PRODUIT,1,3)=="A38") %>% 
    select(CNA_ACTIVITE, CNA_PRODUIT, OBS_VALUE) %>%
    mutate(CNA_ACTIVITE = str_remove(CNA_ACTIVITE,"A38-")) %>%
    mutate(CNA_PRODUIT = str_remove(CNA_PRODUIT,"A38-")) %>%
    pivot_wider(names_from = CNA_ACTIVITE, values_from = OBS_VALUE) %>%
    arrange(CNA_PRODUIT)

  # build ic matrix
  ic_matrix = as.data.frame(tei_data[,1])
  for (j in 1:nrow(ic_matrix)) 
  {
    product = ic_matrix$CNA_PRODUIT[j]
    ic_matrix[,product] = c(0)
    for (i in 1:36) 
    {
      if (sum(tei_data[,product]) > 0) {
        ic_matrix[i,product] = round(tei_data[i,product] / sum(tei_data[,product]), digits = 6)
      }
    }
  }
  names(ic_matrix)[names(ic_matrix) == 'CNA_PRODUIT'] = 'BRANCH'

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

  ccf_data = insee_ccf_data %>%
    filter(substr(CNA_ACTIVITE,1,3)=="A38") %>%
    mutate(CNA_ACTIVITE = str_remove(CNA_ACTIVITE,"A38-")) %>%
    mutate(OPERATION = str_remove(OPERATION,"_SEC10")) %>%
    select(CNA_ACTIVITE, OPERATION, OBS_VALUE) %>%
    pivot_wider(names_from = OPERATION, values_from = OBS_VALUE) %>%
    arrange(CNA_ACTIVITE) %>%
    replace(is.na(.),0)
  
  print(ccf_data)
  # fetch data (ENC)
  insee_enc_data = get_insee_dataset(
    "CNA-2014-PAT-NF",
    startPeriod = year,
    endPeriod = year,
    filter="A....VAL....ENC..BRUT"
  )
  
  enc_data = insee_enc_data %>% 
    filter(substr(CNA_ACTIVITE,1,3)=="A38") %>%
    mutate(CNA_ACTIVITE = str_remove(CNA_ACTIVITE,"A38-")) %>%
    mutate(OPERATION = str_remove(OPERATION,"_SEC10")) %>%
    select(CNA_ACTIVITE, OPERATION, OBS_VALUE) %>%
    pivot_wider(names_from = OPERATION, values_from = OBS_VALUE) %>%
    arrange(CNA_ACTIVITE) %>%
    replace(is.na(.),0)
  
  # build cfc matrix
  cfc_matrix = as.data.frame(ccf_data[,1])
  print("there")

  for (j in 1:nrow(cfc_matrix)) # column -> branch
  {
    branch = cfc_matrix$CNA_ACTIVITE[j]

    cfc_matrix[,branch] = c(0)

    cfc_branch = ccf_data$AN11[j]
    cfc_branch_balance = ccf_data$AN11[j]- ccf_data$AN1121[j] - ccf_data$AN1122[j] - ccf_data$AN114[j] - ccf_data$AN117[j]
    total_enc = enc_data$AN111[j] + enc_data$AN1131[j] + enc_data$AN1132[j] + enc_data$AN1139[j] + enc_data$AN115[j]

    # cfc by product (SEC10)
    cfc_branch_an111 = (enc_data$AN111[j] / total_enc) * cfc_branch_balance  # part of branch cfc balance
    cfc_branch_an112 = ccf_data$AN1121[j] + ccf_data$AN1122[j]
    cfc_branch_an1131 = (enc_data$AN1131[j] / total_enc) * cfc_branch_balance  # part of branch cfc balance
    cfc_branch_an1132 = (enc_data$AN1132[j] / total_enc) * cfc_branch_balance  # part of branch cfc balance
    cfc_branch_an1139 = (enc_data$AN1139[j] / total_enc) * cfc_branch_balance  # part of branch cfc balance
    cfc_branch_an114 = ccf_data$AN114[j]
    cfc_branch_an115 = (enc_data$AN115[j] / total_enc) * cfc_branch_balance  # part of branch cfc balance
    cfc_branch_an117 = ccf_data$AN117[j]
    
    # cfc coef by product (A38)
    cfc_matrix[cfc_matrix$CNA_ACTIVITE=="AZ",branch] = cfc_branch_an115 / cfc_branch
    cfc_matrix[cfc_matrix$CNA_ACTIVITE=="FZ",branch] = (cfc_branch_an111+cfc_branch_an112) / cfc_branch
    cfc_matrix[cfc_matrix$CNA_ACTIVITE=="CH",branch] = cfc_branch_an114 / cfc_branch
    cfc_matrix[cfc_matrix$CNA_ACTIVITE=="CI",branch] = cfc_branch_an1132 / cfc_branch
    cfc_matrix[cfc_matrix$CNA_ACTIVITE=="CK",branch] = cfc_branch_an1139 / cfc_branch
    cfc_matrix[cfc_matrix$CNA_ACTIVITE=="CL",branch] = cfc_branch_an1131 / cfc_branch
    cfc_matrix[cfc_matrix$CNA_ACTIVITE=="MB",branch] = cfc_branch_an117 / cfc_branch
  }

  cfc_matrix$TZ = c(0)
  cfc_matrix = rbind(cfc_matrix,0)
  cfc_matrix$CNA_ACTIVITE[37] = 'TZ'
  names(cfc_matrix)[names(cfc_matrix) == 'CNA_ACTIVITE'] = 'BRANCH'

  return(cfc_matrix)
}

######################################################################################################################
################################################## TRANSFERS MATRIX ##################################################

# Composition of product domestic ressources (columns) by activities production (rows)

get_transfers_matrix = function(year)
{
  dom = read.csv(paste0("https://raw.githubusercontent.com/La-Societe-Nouvelle/LaSocieteNouvelle-defautdata/master/DefaultData-LSN/donnees/tess_",year,"_dom.csv"),sep=";")
  names(dom)=dom[8,]
  dom = dom[-c(1:7),-1]
  cdom = as.data.frame(apply(dom,2,function(x)gsub('\\s+', '',x)))

  #print(sapply(cdom, typeof))
  branches = read.csv(paste0(wd,"/lib/Branches.csv"), header=T, sep=";")

  tr_matrix = as.data.frame(branches$CODE)
  names(tr_matrix) = "PRODUCT"

  row_offset = 2
  col_offset = 2

  for(j in 1:nrow(branches)) # product (TEES)
  {
    branch = branches$CODE[j]
    tr_matrix[,branch] = c(0)
    for(i in 1:nrow(branches)) # branch (TEES)
    {
      if (as.numeric(cdom[row_offset+j,40])!=0 && !is.na(as.numeric(cdom[row_offset+j,col_offset+i]))) {
        tr_matrix[i,branch] = as.numeric(cdom[row_offset+j,col_offset+i]) / as.numeric(cdom[row_offset+j,40])
      }
    }
  }

  return(tr_matrix)
}
