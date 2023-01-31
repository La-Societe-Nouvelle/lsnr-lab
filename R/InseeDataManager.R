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
#   P8        Correction CAF/CAB
#   P5        Formation brute de capital

# List of assets
#     AN11    Actifs fixes
# (!) AN111   Logements                                                                 -> FZ Construction
#     AN112   Autres bâtiments et ouvrages de génie civil (batîments hors résidentiel)  -> FZ Construction
#     AN113   Machines et équipements                                                   -> AN1131 & AN1132
# (!) AN1131  Matériel de transport                                                     -> CL Fabrication de matériels de transport
# (!) AN1132  Equipement TIC                                                            -> CI Fabrication de produits informatiques, électroniques et optiques
#     AN114   Systèmes d'armes                                                          -> CH Métallurgie et fabrication de produits métalliques (25.40Z Fabrication d'armes et de munitions)
#     AN115   Ressources biologiques cultivées                                          -> AZ Agriculture, sylviculture et pêche
#     AN117   Droits de propriété intellectuelle                                        -> MB Recherche-développement scientifique
# (!) AN1139  Autres machines et équipement                                             -> CK Fabrication de machines et équipements n.c.a.

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
    select(CNA_ACTIVITE, CNA_PRODUIT, OBS_VALUE) %>%
    mutate(CNA_ACTIVITE = str_remove(CNA_ACTIVITE,"A38-")) %>%
    mutate(CNA_PRODUIT = str_remove(CNA_PRODUIT,"A38-")) %>%
    pivot_wider(names_from = CNA_PRODUIT, values_from = OBS_VALUE) %>%
    arrange(CNA_ACTIVITE)

  # build ic matrix
  ic_matrix = as.data.frame(tei_data[,1])
  for (j in 1:nrow(ic_matrix))
  {
    product = ic_matrix$CNA_ACTIVITE[j]
    ic_matrix[,product] = c(0)
    for (i in 1:36)
    {
      ic_matrix[i,product] = tei_data[i,product] / tei_data$NNTOTAL[i]
    }
  }
  names(ic_matrix)[names(ic_matrix) == 'CNA_ACTIVITE'] = 'BRANCH'

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
    select(CNA_ACTIVITE, OPERATION, OBS_VALUE) %>%
    pivot_wider(names_from = OPERATION, values_from = OBS_VALUE) %>%
    mutate(CNA_ACTIVITE = str_remove(CNA_ACTIVITE,"A38-")) %>%
    arrange(CNA_ACTIVITE) %>%
    replace(is.na(.),0) %>%
    mutate(AN1173_SEC10 = AN117_SEC10 - AN1171_SEC10 - AN1174_SEC10) %>%
    select(!"AN117_SEC10")

  # fetch data (ENC)
  insee_enc_data = get_insee_dataset(
    "CNA-2014-PAT-NF",
    startPeriod = year,
    endPeriod = year,
    filter="A....VAL....ENC..BRUT"
  )

  enc_data = insee_enc_data %>%
    filter(substr(CNA_ACTIVITE,1,3)=="A38") %>%
    filter(OPERATION %in% c("AN1131_SEC10","AN1132_SEC10","AN1139_SEC10","AN111_SEC10","AN115_SEC10")) %>%
    select(CNA_ACTIVITE, OPERATION, OBS_VALUE) %>%
    pivot_wider(names_from = OPERATION, values_from = OBS_VALUE) %>%
    mutate(CNA_ACTIVITE = str_remove(CNA_ACTIVITE,"A38-")) %>%
    arrange(CNA_ACTIVITE) %>%
    replace(is.na(.),0)

  # build cfc matrix
  cfc_matrix = as.data.frame(ccf_data[,1])
  for (j in 1:nrow(cfc_matrix))
  {
    product = cfc_matrix$CNA_ACTIVITE[j]
    cfc_matrix[,product] = c(0)

    for (i in 1:36)
    {
      # CH <- AN114
      if (product=="CH") {
        cfc_matrix[i,product] = ccf_data$AN114_SEC10[i] / ccf_data$AN11[i]
        if (is.na(cfc_matrix[i,product])) cfc_matrix[i,product] = 0
      }
      # CI <- AN1132
      if (product=="CI") {
        total_enc = enc_data$AN1131_SEC10[i] + enc_data$AN1132_SEC10[i] + enc_data$AN1139_SEC10[i] + enc_data$AN111_SEC10[i] + enc_data$AN115_SEC10[i]
        ccf_rest = ccf_data$AN11[i] - ccf_data$AN1121_SEC10[i] - ccf_data$AN1122_SEC10[i] - (ccf_data$AN1171_SEC10[i] + ccf_data$AN1173_SEC10[i] + ccf_data$AN1174_SEC10[i]) - ccf_data$AN114_SEC10[i]
        cfc_matrix[i,product] = (enc_data$AN1132_SEC10[i] / total_enc) * ccf_rest / ccf_data$AN11[i]
      }
      # CK <- AN1139
      if (product=="CK") {
        total_enc = enc_data$AN1131_SEC10[i] + enc_data$AN1132_SEC10[i] + enc_data$AN1139_SEC10[i] + enc_data$AN111_SEC10[i] + enc_data$AN115_SEC10[i]
        ccf_rest = ccf_data$AN11[i] - ccf_data$AN1121_SEC10[i] - ccf_data$AN1122_SEC10[i] - (ccf_data$AN1171_SEC10[i] + ccf_data$AN1173_SEC10[i] + ccf_data$AN1174_SEC10[i]) - ccf_data$AN114_SEC10[i]
        cfc_matrix[i,product] = (enc_data$AN1139_SEC10[i] / total_enc) * ccf_rest / ccf_data$AN11[i]
      }
      # CL <- AN131
      if (product=="CL") {
        total_enc = enc_data$AN1131_SEC10[i] + enc_data$AN1132_SEC10[i] + enc_data$AN1139_SEC10[i] + enc_data$AN111_SEC10[i] + enc_data$AN115_SEC10[i]
        ccf_rest = ccf_data$AN11[i] - ccf_data$AN1121_SEC10[i] - ccf_data$AN1122_SEC10[i] - (ccf_data$AN1171_SEC10[i] + ccf_data$AN1173_SEC10[i] + ccf_data$AN1174_SEC10[i]) - ccf_data$AN114_SEC10[i]
        cfc_matrix[i,product] = (enc_data$AN1131_SEC10[i] / total_enc) * ccf_rest / ccf_data$AN11[i]
      }
      # FZ <- AN111, AN1121, AN1122
      if (product=="FZ") {
        total_enc = enc_data$AN1131_SEC10[i] + enc_data$AN1132_SEC10[i] + enc_data$AN1139_SEC10[i] + enc_data$AN111_SEC10[i] + enc_data$AN115_SEC10[i]
        ccf_rest = ccf_data$AN11[i] - ccf_data$AN1121_SEC10[i] - ccf_data$AN1122_SEC10[i] - (ccf_data$AN1171_SEC10[i] + ccf_data$AN1173_SEC10[i] + ccf_data$AN1174_SEC10[i]) - ccf_data$AN114_SEC10[i]
        cfc_matrix[i,product] = ccf_data$AN1121_SEC10[i] / ccf_data$AN11[i]
                              + ccf_data$AN1122_SEC10[i] / ccf_data$AN11[i]
                              + (enc_data$AN111_SEC10[i] / total_enc) * ccf_rest / ccf_data$AN11[i]
      }
      # MB <- AN1171
      if (product=="MB") {
        cfc_matrix[i,product] = ccf_data$AN1171_SEC10[i] / ccf_data$AN11[i]
      }

      # JC <- AN1173
      if (product=="JC") {
        cfc_matrix[i,product] = ccf_data$AN1173_SEC10[i] / ccf_data$AN11[i]
      }

      # MB <- AN1174
      if (product=="MB") {
        cfc_matrix[i,product] = ccf_data$AN1174_SEC10[i] / ccf_data$AN11[i]
      }
    }
  }
  cfc_matrix$TZ = c(0)
  cfc_matrix = rbind(cfc_matrix,0)
  cfc_matrix$CNA_ACTIVITE[37] = 'TZ'
  names(cfc_matrix)[names(cfc_matrix) == 'CNA_ACTIVITE'] = 'BRANCH'

  return(cfc_matrix)
}

######################################################################################################################
################################################## TRANSFERS MATRIX ##################################################

get_transfers_matrix = function(year)
{

  dom = read.csv(paste0("https://raw.githubusercontent.com/La-Societe-Nouvelle/LaSocieteNouvelle-defautdata/master/DefaultData-LSN/donnees/tess_",year,"_dom.csv"),sep=";")
  names(dom)=dom[8,]
  dom = dom[-c(1:7),-1]
  cdom = as.data.frame(apply(dom,2,function(x)gsub('\\s+', '',x)))

  #print(sapply(cdom, typeof))
  branches = lsnr:::Branches

  tr_matrix = as.data.frame(branches$CODE)
  names(tr_matrix) = "PRODUCT"

  row_offset = 2
  col_offset = 2

  for(j in 1:nrow(branches))
  {
    branch = branches$CODE[j]
    tr_matrix[,branch] = c(0)

    for(i in 1:nrow(branches))
    {
      if (as.numeric(cdom[row_offset+i,40])!=0 && !is.na(as.numeric(cdom[row_offset+i,col_offset+j]))) {
        tr_matrix[i,branch] = as.numeric(cdom[row_offset+i,col_offset+j]) / as.numeric(cdom[row_offset+i,40])
      }
    }
  }

  return(tr_matrix)
}
