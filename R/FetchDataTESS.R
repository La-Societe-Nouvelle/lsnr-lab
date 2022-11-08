#'Fetch data frame associated to domestic symetric input-output tables
#'of Insee annual national accounts.
#'
#' @param Year Considered year.
#'
#' @return A `data.frame` object containing annual Insee domestic symetric input-output tables data.
#' @importFrom utils read.csv
#' @seealso \code{\link{BuildDivisionsData}}, \code{\link{BuildBranchesData}}.
#' @examples
#' FetchDataTESS(2018)
#' @export
FetchDataTESS=function(Year){
  path=paste0("C:/Users/Joris/OneDrive - La Société Nouvelle/Documents/Travaux statistiques/Dossier tampon/TESS/",Year,".csv")
  if(abs(difftime(file.info(path)$ctime,Sys.time(),units="days"))>7|(file.exists(path)==F)){

  dom=read.csv(paste0("https://raw.githubusercontent.com/La-Societe-Nouvelle/LaSocieteNouvelle-defautdata/master/DefaultData-LSN/donnees/tess_",Year,"_dom.csv"),sep=";")
  names(dom)=dom[8,]
  dom=dom[-c(1:7),-1]
  cdom=as.data.frame(apply(dom,2,function(x)gsub('\\s+', '',x)))
  for(x in 3:39){
    for(i in 3:39){
      cdom[i,x]=ifelse(as.numeric(cdom[i,40])==0,0,as.numeric(cdom[i,x])/as.numeric(cdom[i,40]))
    }}
  ccdom=cdom[-c(1:2),c(2:39)]
  for(i in 1:nrow(ccdom)){
    for(x in 1:ncol(ccdom)){
      if(is.na(ccdom[i,x])){ccdom[i,x]=0}
    }
  }
  write.table(ccdom,path,sep=";",quote = T,row.names = F)
}else{ccdom=read.csv(path,header=T,sep=";")}
  return(ccdom)
}
