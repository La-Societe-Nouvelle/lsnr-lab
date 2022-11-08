#'Fetch data frame associated to branches consumption of fixed capital
#'of annual national accounts from Insee's API.
#'
#' @param Year Considered year.
#'
#' @return A `data.frame` object containing annual Insee supply-use equilibrium data.
#' @importFrom dplyr %>%
#' @importFrom dplyr select
#' @importFrom dplyr filter
#' @importFrom insee get_insee_dataset
#' @importFrom tidyr pivot_wider
#' @importFrom stringr str_detect
#' @importFrom stringr str_remove
#' @seealso \code{\link{BuildDivisionsData}}, \code{\link{BuildBranchesData}}.
#' @examples
#' FetchDataCCF(2018)
#' @export
FetchDataCFC=function(Year){
  path=paste0("C:/Users/Joris/OneDrive - La Société Nouvelle/Documents/Travaux statistiques/Dossier tampon/CCF/",Year,".csv")
  if(abs(difftime(file.info(path)$ctime,Sys.time(),units="days"))>7|(file.exists(path)==F)){
  RCCF=get_insee_dataset("CNA-2014-PAT-NF",filter="A........CCF..") %>% filter(PRIX_REF=="VAL" & substr(CNA_ACTIVITE,1,3)=="A38" & TIME_PERIOD==Year & (OPERATION %in% c("AN1171_SEC10","AN1174_SEC10"))==F) %>% select(TIME_PERIOD,OBS_VALUE,CNA_ACTIVITE,TITLE_FR,OPERATION)
  #select current euros values for the considered year
  CCF=cbind(pivot_wider(RCCF[,-(ncol(RCCF)-1)],names_from = OPERATION,values_from = OBS_VALUE),0,0,0,0)
  names(CCF)[names(CCF)=="AN117_SEC10"]="MB"
  names(CCF)[names(CCF)=="AN114_SEC10"]="CH"
  names(CCF)[names(CCF)=="AN11"]="TOTAL"
  CCF$FZ=CCF$AN1121_SEC10+CCF$AN1122_SEC10
  CCF=CCF[,-which(names(CCF)%in%c("AN1121_SEC10","AN1122_SEC10"))]
  names(CCF)[(ncol(CCF)-4):(ncol(CCF)-1)]=c("CL","CI","CK","Logement FZ")
  #AN11 = Actifs fixes
  #AN112 = Autres bâtiments et ouvrages de génie civil (batîments hors résidentiel) - FZ Construction
  #AN114 = Systèmes d'armes - CH Métallurgie et fabrication de produits métalliques = 25.40Z : Fabrication d'armes et de munitions
  #AN117 = Droits de propriété intellectuelle - MB Recherche-développement scientifique

  #AN111 = Logements FZ Construction
  #AN113 = Machines et équipements
    #AN1131 = Matériel de transport - CL Fabrication de matériels de transport
    #AN1132 = Equipement TIC - CI Fabrication de produits informatiques, électroniques et optiques
    #AN1139 = Autres machines et équipement - CK Fabrication de machines et équipements n.c.a.
  #AN115 = Ressources biologiques cultivées - AZ Agriculture, sylviculture et pêche
  #Pour ces catégories, le détail de la consommation de CF est disponible au format secteur institutionnel. Ainsi, on assigne le résidu de consommation de capital fixe
  #(11-112-114-117) au prorata de la composition de l'encours d'actifs fixes en 111, 113 et 115.
  RENCCF=get_insee_dataset("CNA-2014-PAT-NF",filter="A....VAL....ENC..BRUT") %>% filter(PRIX_REF=="VAL" & substr(CNA_ACTIVITE,1,3)=="A38" & TIME_PERIOD==Year & OPERATION %in% c("AN1131_SEC10","AN1132_SEC10","AN1139_SEC10","AN111_SEC10","AN115_SEC10")) %>% select(TIME_PERIOD,OBS_VALUE,CNA_ACTIVITE,TITLE_FR,OPERATION)
  ENCCF=pivot_wider(RENCCF[,-(ncol(RENCCF)-1)],names_from = OPERATION,values_from = OBS_VALUE)
  for(r in 1:nrow(ENCCF)){for(c in 1:ncol(ENCCF)){ENCCF[r,c]=ifelse(is.na(ENCCF[r,c]),0,ENCCF[r,c])}}
  for(r in 1:nrow(CCF)){for(c in 1:ncol(CCF)){CCF[r,c]=ifelse(is.na(CCF[r,c]),0,CCF[r,c])}}
  for(i in 1:nrow(CCF)){
    r=which(ENCCF$CNA_ACTIVITE==CCF$CNA_ACTIVITE[i])
    CCF$CL[i]=(ENCCF$AN1131_SEC10[r])/(ENCCF$AN1131_SEC10[r]+ENCCF$AN1132_SEC10[r]+ENCCF$AN1139_SEC10[r]+ENCCF$AN111_SEC10[r]+ENCCF$AN115_SEC10[r])*(CCF$TOTAL[i]-CCF$FZ[i]-CCF$MB[i]-CCF$CH[i])
    CCF$CI[i]=(ENCCF$AN1132_SEC10[r])/(ENCCF$AN1131_SEC10[r]+ENCCF$AN1132_SEC10[r]+ENCCF$AN1139_SEC10[r]+ENCCF$AN111_SEC10[r]+ENCCF$AN115_SEC10[r])*(CCF$TOTAL[i]-CCF$FZ[i]-CCF$MB[i]-CCF$CH[i])
    CCF$CK[i]=(ENCCF$AN1139_SEC10[r])/(ENCCF$AN1131_SEC10[r]+ENCCF$AN1132_SEC10[r]+ENCCF$AN1139_SEC10[r]+ENCCF$AN111_SEC10[r]+ENCCF$AN115_SEC10[r])*(CCF$TOTAL[i]-CCF$FZ[i]-CCF$MB[i]-CCF$CH[i])
    CCF$`Logement FZ`[i]=(ENCCF$AN111_SEC10[r])/(ENCCF$AN1131_SEC10[r]+ENCCF$AN1132_SEC10[r]+ENCCF$AN1139_SEC10[r]+ENCCF$AN111_SEC10[r]+ENCCF$AN115_SEC10[r])*(CCF$TOTAL[i]-CCF$FZ[i]-CCF$MB[i]-CCF$CH[i])
  }
  CCF$FZ=CCF$FZ+CCF$`Logement FZ`
  CCF=CCF[,-which(names(CCF)%in%c("Logement FZ"))]
  CCF$CNA_ACTIVITE=str_remove(CCF$CNA_ACTIVITE,"A38-")
  #division details (*88) are unavailable
  write.table(CCF,path,sep=";",quote = T,row.names = F)
  }else{CCF=read.csv(path,header=T,sep=";")}
  return(CCF)
}
