#'Fetch data frame associated to production and business accounts
#'of annual national accounts from Insee's API.
#'
#' @param Year Considered year.
#'
#' @return A `data.frame` object containing annual Insee production and business accounts data.
#' @importFrom dplyr %>%
#' @importFrom dplyr select
#' @importFrom insee get_insee_dataset
#' @importFrom tidyr pivot_wider
#' @importFrom stringr str_remove
#'
#' @seealso \code{\link{BuildDivisionsData}}, \code{\link{BuildBranchesData}}.
#'
#' @examples
#' FetchDataCPEB(2018)
#' @export
FetchDataCPEB=function(Year){
  path=paste0("C:/Users/Joris/OneDrive - La Société Nouvelle/Documents/Travaux statistiques/Dossier tampon/CPEB/",Year,".csv")
  if(abs(difftime(file.info(path)$ctime,Sys.time(),units="days"))>7|(file.exists(path)==F)){
  cpeb=get_insee_dataset("CNA-2014-CPEB",startPeriod = Year,endPeriod = Year,filter = "A...VAL.....BRUT")
  cpeb=cpeb[grepl("A38",cpeb$CNA_ACTIVITE),] %>% select(OBS_VALUE, CNA_ACTIVITE,OPERATION)
  cpeb$CNA_ACTIVITE=str_remove(cpeb$CNA_ACTIVITE,"A38-")
  ccpeb=pivot_wider(cpeb,names_from = OPERATION,values_from = OBS_VALUE)
  write.table(ccpeb,path,sep=";",quote = T,row.names = F)
  }else{ccpeb=read.csv(path,header=T,sep=";")}
  return(ccpeb)
}
