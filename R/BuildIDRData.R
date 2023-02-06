#' @importFrom curl curl_download
#' @importFrom stringr str_remove
#' @importFrom stringr str_locate_all
#' @importFrom stringr str_detect
#' @importFrom data.table fread
#'
#' @noRd

build_branches_nva_fpt_idr = function(selectedYear)
{
  if(selectedYear %in% c(2016:2020) == F)
  {
    stop("Les bases tous salariés de l'Insee ne sont disponibles que pour les années 2016 à 2020")
  }

  temp_folder = substr(tempdir(),1,unlist(str_locate_all(tempdir(),"\\\\"))[length(unlist(str_locate_all(tempdir(),"\\\\")))]-1)

  list_files = list.files(temp_folder,recursive = T,all.files = T,full.names = T)

  if(sum(grepl(paste0("idr",selectedYear,".csv"),list_files))==1)
  {
    fl = list_files[which(grepl(paste0("idr",selectedYear,".csv"),list_files))]
    DataIDR = read.csv(fl,header = T,encoding = 'latin1',sep = ";")
  }
  else
  {
    links=data.frame(link=c("https://www.insee.fr/fr/statistiques/fichier/4994559/dads2016_gf_salaries16_csv.zip",
                            "https://www.insee.fr/fr/statistiques/fichier/4994589/dads2017_gf_salaries17_csv.zip",
                            "https://www.insee.fr/fr/statistiques/fichier/5366604/dads2018_gf_salaries18_csv.zip",
                            "https://www.insee.fr/fr/statistiques/fichier/5542536/FD_SALAAN_2019_csv.zip",
                            "https://www.insee.fr/fr/statistiques/fichier/6524154/FD_SALAAN_2020_csv.zip"),
                     year=c(2016,2017,2018,2019,2020))

    dir.create(tempdir(),showWarnings = F)
    tpd=tempdir()

    print("Downloading : Insee 'Base tous salariés'...")

    curl_download(links$link[links$year==selectedYear],paste0(tpd,"/sal.zip"),quiet=T)
    unzip(paste0(tpd,"/sal.zip"),exdir = paste0(tpd,"/unzipped_files/"))

    fl=paste0(tpd,"/unzipped_files/",list.files(paste0(tpd,"/unzipped_files/"))[str_detect(list.files(paste0(tpd,"/unzipped_files/")),"FD_SALAAN")])

    #Matching with continuous bracket system (take mean/median of each bracket)

    print("IDR NVA FPT calculations...")

    neto_code=list(0:200,200:499,500:999,1000:1499,1500:1999,2000:2999,3000:3999,4000:5999,6000:7999,8000:9999,10000:11999,12000:13999,14000:15999,16000:17999,18000:19999,20000:21999,
                   22000:23999,24000:25999,26000:27999,28000:29999,30000:34999,35000:39999,40000:49999,50000:200000)

    for(x in 1:length(neto_code)){neto_code[[x]]=median(neto_code[[x]])}

    match_neto=matrix(c(0:23,unlist(neto_code)),24,2)

    #NBHEUR_TOT: Nombre d'heures salariées total
    #TRNNETO: Rémunération nette globale, en tranches
    #A38: Activité (NAF rév2) en nomenclature agrégée (38 postes)

    #Assign continuous previously defined bracket
    bts=as.data.frame(fread(fl) %>% select(A38,TRNNETO,NBHEUR_TOT,SEXE)  %>% mutate(NETO=0))
    for(x in unique(bts$TRNNETO)){
      ls=which(bts$TRNNETO==x)
      bts$NETO[ls]=match_neto[match_neto[,1]==x,2]
    }

    bts=bts %>% mutate(`Rémunérations horaires`=NETO/NBHEUR_TOT) %>% filter(`Rémunérations horaires`!="Inf")

    branches_aggregates = get_branches_aggregates(selectedYear)

    DataIDR=data.frame(BRANCH = branches_aggregates$BRANCH,
                       NVA = branches_aggregates$NVA,
                       GROSS_IMPACT = 0,
                       UNIT_GROSS_IMPACT = "IND",
                       FOOTPRINT=0,
                       UNIT_FOOTPRINT = "IND")

    for(x in 1:nrow(DataIDR)){
      values=bts[bts$A38==DataIDR$BRANCH[x],]
      DataIDR$FOOTPRINT[x] = DataIDR$GROSS_IMPACT[x] = quantile(values$`Rémunérations horaires`,prob=0.9,na.rm=T)/quantile(values$`Rémunérations horaires`,prob=0.1,na.rm=T)
    }

    unlink(tpd,recursive = T)
    dir.create(tempdir())

    write.table(DataIDR,file = paste0(tempdir(),"/idr",selectedYear,".csv"), quote = T, fileEncoding = 'latin1',sep = ";",row.names = F)

  }

  return(DataIDR)

}

build_divisions_nva_fpt_idr = function(selectedYear)
{
  # get divisions aggregates -------------------------- #

  divisions_aggregates = get_divisions_aggregates(selectedYear)

  # fetch data --------------------------------------- #

  temp_folder = substr(tempdir(),1,unlist(str_locate_all(tempdir(),"\\\\"))[length(unlist(str_locate_all(tempdir(),"\\\\")))]-1)

  list_files = list.files(temp_folder,recursive = T,all.files = T,full.names = T)

  if(sum(grepl(paste0("idr",selectedYear,".csv"),list_files))==1)
  {
    fl = list_files[which(grepl(paste0("idr",selectedYear,".csv"),list_files))]
    branches_fpt = read.csv(fl,header = T,encoding = 'latin1',sep = ";")
  }
  else
  {
    branches_fpt = build_branches_nva_fpt_idr(selectedYear)
  }

  # build nva fpt dataframe -------------------------- #

  nva_fpt_data = data.frame(DIVISION = as.character(divisions_aggregates$CNA_ACTIVITE),
                            NVA = as.numeric(divisions_aggregates$NVA),
                            GROSS_IMPACT = 0,
                            UNIT_GROSS_IMPACT = "IND",
                            FOOTPRINT = 0,
                            UNIT_FOOTPRINT = "IND"
  )

  for(i in 1:nrow(nva_fpt_data))
  {
    branch = lsnr:::Divisions$BRANCH[lsnr:::Divisions$DIVISION == nva_fpt_data$DIVISION[i]]
    nva_fpt_data$FOOTPRINT[i] = nva_fpt_data$GROSS_IMPACT[i] = branches_fpt$FOOTPRINT[branches_fpt$BRANCH == branch]
  }

  return(nva_fpt_data)
  # -------------------------------------------------- #
}

get_branches_imp_coef_idr = function(selectedYear)
{
  return(1)
}
