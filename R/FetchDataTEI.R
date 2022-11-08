#'Fetch data frame associated to intermediate use tables
#'of annual national accounts from Insee's API.
#'
#' @param Year Considered year.
#'
#' @return A `data.frame` object containing annual Insee intermediate use tables data.
#' @importFrom dplyr %>%
#' @importFrom dplyr select
#' @importFrom insee get_insee_dataset
#' @importFrom tidyr pivot_wider
#' @importFrom stringr str_remove
#' @seealso \code{\link{BuildDivisionsData}}, \code{\link{BuildBranchesData}}.
#' @examples
#' FetchDataTEI(2018)
#' @export

FetchDataTEI=function(Year){
  path=paste0("C:/Users/Joris/OneDrive - La Société Nouvelle/Documents/Travaux statistiques/Dossier tampon/TEI/",Year,".csv")
  if(abs(difftime(file.info(path)$ctime,Sys.time(),units="days"))>7|(file.exists(path)==F)){

  tei=get_insee_dataset("CNA-2014-TEI",startPeriod = Year,endPeriod = Year)
  tei=tei[grepl("A38",tei$CNA_ACTIVITE),] %>% select(OBS_VALUE, CNA_ACTIVITE,CNA_PRODUIT)
  tei$CNA_ACTIVITE=str_remove(tei$CNA_ACTIVITE,"A38-")
  tei$CNA_PRODUIT=str_remove(tei$CNA_PRODUIT,"A38-")
  ctei=pivot_wider(tei,names_from = CNA_ACTIVITE,values_from = OBS_VALUE)
  nc=ncol(ctei)
  nr=nrow(ctei)
  ##Calculation of kij
  options(scipen=999)
  for(i in 2:nc){
    for(x in c(1:11,13:nr)){
      ctei[x,i]=ctei[x,i]/ctei[ctei$CNA_PRODUIT=="NNTOTAL",i]}}
  ###row 12 being NNTOTAL
  cctei=ctei[-12,]
  cctei$TZ=rep(0,nrow(cctei))

  write.table(cctei,path,sep=";",quote = T,row.names = F)
}else{cctei=read.csv(path,header=T,sep=";")}

  return(cctei)
}
