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

FetchDataERE=function(Year){
  path=paste0("C:/Users/Joris/OneDrive - La Société Nouvelle/Documents/Travaux statistiques/Dossier tampon/ERE/",Year,".csv")
  if(abs(difftime(file.info(path)$ctime,Sys.time(),units="days"))>7|(file.exists(path)==F)){

  p1=get_insee_dataset("CNA-2014-ERE",startPeriod = Year,endPeriod = Year,filter = "A..VAL.P1..VALEUR_ABSOLUE.FE.EUROS_COURANTS.BRUT")
  p2=get_insee_dataset("CNA-2014-ERE",startPeriod = Year,endPeriod = Year,filter = "A..VAL.P2..VALEUR_ABSOLUE.FE.EUROS_COURANTS.BRUT")
  ereress=get_insee_dataset("CNA-2014-ERE",startPeriod = Year,endPeriod = Year, filter = "A..VAL.ERERESS..VALEUR_ABSOLUE.FE.EUROS_COURANTS.BRUT")
  p7=get_insee_dataset("CNA-2014-ERE",startPeriod = Year,endPeriod = Year,filter = "A..VAL.P7..VALEUR_ABSOLUE.FE.EUROS_COURANTS.BRUT")
  d31=get_insee_dataset("CNA-2014-ERE",startPeriod = Year,endPeriod = Year,filter = "A..VAL.D31..VALEUR_ABSOLUE.FE.EUROS_COURANTS.BRUT")
  d21=get_insee_dataset("CNA-2014-ERE",startPeriod = Year,endPeriod = Year,filter = "A..VAL.D21..VALEUR_ABSOLUE.FE.EUROS_COURANTS.BRUT")
  p91=get_insee_dataset("CNA-2014-ERE",startPeriod = Year,endPeriod = Year,filter = "A..VAL.P91..VALEUR_ABSOLUE.FE.EUROS_COURANTS.BRUT")
  p92=get_insee_dataset("CNA-2014-ERE",startPeriod = Year,endPeriod = Year,filter = "A..VAL.P92..VALEUR_ABSOLUE.FE.EUROS_COURANTS.BRUT")
  p8=get_insee_dataset("CNA-2014-ERE",startPeriod = Year,endPeriod = Year,filter = "A..VAL.P8..VALEUR_ABSOLUE.FE.EUROS_COURANTS.BRUT")
  p5=get_insee_dataset("CNA-2014-ERE",startPeriod = Year,endPeriod = Year,filter = "A..VAL.P5..VALEUR_ABSOLUE.FE.EUROS_COURANTS.BRUT")


  ere=rbind(p1,p2,ereress,p7,d31,d21,p91,p92,p8,p5)
  ere=ere[grepl("A38",ere$CNA_PRODUIT),] %>% select(OBS_VALUE,CNA_PRODUIT,OPERATION)
  cere=(pivot_wider(ere,names_from = OPERATION,values_from = OBS_VALUE))
  cere[is.na(cere)]=0
  nr=nrow(cere)
  nc=ncol(cere)
  cere$CNA_PRODUIT=str_remove(cere$CNA_PRODUIT,"A38-")
  cere=as.data.frame(cere)
  for(i in 1:nrow(cere)){
    cere$total[i] = cere$ERERESS[i]-(cere$D31[i]+cere$D21[i]+cere$P91[i]+cere$P92[i])
    cere$p7hc[i] = cere$P7[i]
    cere$P7[i] = cere$P7[i]+cere$P8[i]
    }
  write.table(cere,path,sep=";",quote = T,row.names = F)
  }else{cere=read.csv(path,header=T,sep=";")}
  return(cere)
}
