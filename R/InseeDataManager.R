#' @importFrom insee get_insee_dataset
#' @importFrom dplyr %>%
#' @importFrom dplyr select
#' @importFrom dplyr mutate
#' @importFrom dplyr arrange
#' @importFrom dplyr filter
#' @importFrom insee get_insee_dataset
#' @importFrom tidyr pivot_wider
#' @importFrom stringr str_remove
#' @importFrom tibble add_column
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

  if(any(lapply(datalist,nrow) == 0))
  {
    stop(paste0("Données économiques des produits indisponibles pour l'année ", year))
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
    stop(paste0("Comptes de production et d'exploitation des branches indisponibles pour l'année ", year))
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
    stop(paste0("Données des consommations de capital fixe des branches indisponibles pour l'année ", year))
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

  # fetch data (CPEB)
  insee_cpeb_data = get_insee_dataset(
    "CNA-2014-CPEB",
    startPeriod = year,
    endPeriod = year,
    filter = "A...VAL.....BRUT"
  )

  if(nrow(insee_cpeb_data) == 0)
  {
    stop(paste0("Comptes de production et d'exploitation des divisions indisponibles pour l'année ", year))
  }

  cpeb_data = insee_cpeb_data %>%
    filter(substr(CNA_ACTIVITE,1,3)=="A88") %>%
    filter(OPERATION %in% c('P1','P2','B1G')) %>%
    select(CNA_ACTIVITE, OPERATION, OBS_VALUE) %>%
    pivot_wider(names_from = OPERATION, values_from = OBS_VALUE) %>%
    mutate(CNA_ACTIVITE = str_remove(CNA_ACTIVITE,"A88-")) %>%
    arrange(CNA_ACTIVITE) %>%
    mutate(CFC = 0,
           NVA = 0)


  divisions = lsnr:::Divisions
  branches_aggregates = get_branches_aggregates(year)

  #Assign CFC amount for division by extending CFC rate of the branch

  for(i in unique(branches_aggregates$BRANCH)){

    r = which(branches_aggregates$BRANCH == i)
    divs = which(cpeb_data$CNA_ACTIVITE %in% divisions$DIVISION[divisions$BRANCH == i])

    ccf_rate = branches_aggregates$CFC[r] / (branches_aggregates$CFC[r] + branches_aggregates$NVA[r])
    ccf_rate = ifelse(is.nan(ccf_rate),0,ccf_rate)

    cpeb_data$CFC[divs] = round(cpeb_data$B1G[divs] * ccf_rate,1)
    cpeb_data$NVA[divs] = round(cpeb_data$B1G[divs] * (1 - ccf_rate),1)
  }

  names(cpeb_data)[which(names(cpeb_data) %in% c("P1","P2","B1G"))] = c("PRD","IC","GVA")

  return(cpeb_data)
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

  if(nrow(insee_tei_data) == 0)
  {
    stop(paste0("Tableau des entrées intermédiaires indisponible pour l'année ", year))
  }

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
      if (sum(tei_data[,product],na.rm = T) > 0) {
        ic_matrix[i,product] = round(tei_data[i,product] / sum(tei_data[,product],na.rm = T), digits = 6)
        if(is.nan(ic_matrix[i,product]) || is.na(ic_matrix[i,product]) || is.null(ic_matrix[i,product]))
        {
          ic_matrix[i,product] = 0
        }
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

  if(nrow(insee_tei_data) == 0)
  {
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
    stop(paste0("Données des consommations de capital fixe des branches indisponibles pour l'année ", year))
  }

  ccf_data = insee_ccf_data %>%
    filter(substr(CNA_ACTIVITE,1,3)=="A38") %>%
    mutate(CNA_ACTIVITE = str_remove(CNA_ACTIVITE,"A38-")) %>%
    mutate(OPERATION = str_remove(OPERATION,"_SEC10")) %>%
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
    mutate(OPERATION = str_remove(OPERATION,"_SEC10")) %>%
    select(CNA_ACTIVITE, OPERATION, OBS_VALUE) %>%
    pivot_wider(names_from = OPERATION, values_from = OBS_VALUE) %>%
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

      # RZ <- AN1174
      if (product=="RZ") {
        cfc_matrix[i,product] = ccf_data$AN1174_SEC10[i] / ccf_data$AN11[i]
      }
    }

  for (j in 1:nrow(cfc_matrix)) # column -> branch
  {
    branch = cfc_matrix$CNA_ACTIVITE[j]

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
    cfc_matrix[cfc_matrix$CNA_ACTIVITE=="AZ",branch] = cfc_branch_an115 / cfc_branch
    cfc_matrix[cfc_matrix$CNA_ACTIVITE=="FZ",branch] = (cfc_branch_an111+cfc_branch_an112) / cfc_branch
    cfc_matrix[cfc_matrix$CNA_ACTIVITE=="CH",branch] = cfc_branch_an114 / cfc_branch
    cfc_matrix[cfc_matrix$CNA_ACTIVITE=="CI",branch] = cfc_branch_an1132 / cfc_branch
    cfc_matrix[cfc_matrix$CNA_ACTIVITE=="CK",branch] = cfc_branch_an1139 / cfc_branch
    cfc_matrix[cfc_matrix$CNA_ACTIVITE=="CL",branch] = cfc_branch_an1131 / cfc_branch
    cfc_matrix[cfc_matrix$CNA_ACTIVITE=="MB",branch] = cfc_branch_an1171 / cfc_branch
    cfc_matrix[cfc_matrix$CNA_ACTIVITE=="JC",branch] = cfc_branch_an1171 / cfc_branch
    cfc_matrix[cfc_matrix$CNA_ACTIVITE=="RZ",branch] = cfc_branch_an1171 / cfc_branch

  }

  cfc_matrix$TZ = c(0)
  cfc_matrix = rbind(cfc_matrix,0)
  cfc_matrix$CNA_ACTIVITE[37] = 'TZ'
  names(cfc_matrix)[names(cfc_matrix) == 'CNA_ACTIVITE'] = 'BRANCH'

  return(cfc_matrix)
  }
  return(cfc_matrix)
}

######################################################################################################################
################################################## TRANSFERS MATRIX ##################################################

# Composition of product domestic ressources (columns) by activities production (rows)

get_transfers_matrix = function(year)
{
  dom = list(lsnr:::tess_2010_dom,
       lsnr:::tess_2011_dom,
       lsnr:::tess_2012_dom,
       lsnr:::tess_2013_dom,
       lsnr:::tess_2015_dom,
       lsnr:::tess_2016_dom,
       lsnr:::tess_2017_dom,
       lsnr:::tess_2018_dom,
       lsnr:::tess_2019_dom
       )[[which(c(2010:2013,2015:2019) == year)]]

  if(class(dom) == "try-error")
  {
    stop(paste0("Tableau entrées-sorties symétrique domestique indisponible pour l'année ", year))
  }

  branches = lsnr:::Branches

  names(dom)=dom[8,]
  dom = dom[-c(1:7),-1]

  tr_matrix = as.data.frame(apply(dom,2,function(x)gsub('\\s+', '',x)))[c(3:39),c(3:39)]

  tr_matrix = as.data.frame(lapply(tr_matrix, as.numeric))

  tr_matrix = (tr_matrix / rowSums(tr_matrix)) %>% add_column(PRODUCT = branches$CODE,.before = "AZ")

  return(tr_matrix)
}
