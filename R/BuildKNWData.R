#'Build and returns all data required to the KNW indicator computations.
#'
#'Returns a `list` made up of value added impacts by French branches, imported products associated coefficient,
#'Data sources and values unit. This data will be used in both BuildBranchesData and BuildDivisionsData functions.
#'
#' @param Year Considered year.
#'
#' @importFrom eurostat get_eurostat
#' @return An object `list` made up of 4 elements : value added impacts by French branches,
#' imported products associated coefficient, data sources and values unit.
#' @seealso \code{\link{BuildECOData}}, \code{\link{BuildGHGData}},
#'  \code{\link{BuildBranchesData}}, \code{\link{BuildDivisionsData}}, \code{\link{FetchDataDisponibility}}.
#' @examples
#' BuildKNWData(max(FetchDataDisponibility("KNW"))
#' @export
BuildKNWData=function(Year){
  KNWDisponibility=FetchDataDisponibility("KNW")
  if((Year %in% KNWDisponibility)==F){
    return("Desired year is unavailable. Please, refer to FetchDataDisponibility function in order to find available years for a given indicator")
  }else{
    ##Build ERE Database
    ERE=get_products_aggregates(Year)
    ReferenceTable=as.data.frame(unique(ERE$CNA_PRODUIT))

    ##Build CPEB Database : P1 - P2

    CPEB=get_branches_aggregates(Year)

RawKNW=get_eurostat("trng_cvt_16n2",time_format = "num",filters = list(geo = c("FR","EU28"), cost = "TOTAL",time=Year))

FRAKNW=ReferenceTable
names(FRAKNW)="NACE"
FRAKNW$BNACE=substr(FRAKNW$NACE,0,1)
EU28KNW=as.data.frame(ReferenceTable)
names(EU28KNW)="NACE"
EU28KNW$BNACE=substr(FRAKNW$NACE,0,1)

FRAKNW$val[FRAKNW$BNACE %in% c("B","C","D","E")]=RawKNW$values[RawKNW$nace_r2=="B-E" & RawKNW$geo=="FR"]
FRAKNW$val[FRAKNW$BNACE %in% c("L","M","N","R","S")]=RawKNW$values[RawKNW$nace_r2=="L-N_R_S" & RawKNW$geo=="FR"]
FRAKNW$val[FRAKNW$BNACE %in% c("J","K")]=RawKNW$values[RawKNW$nace_r2=="J_K" & RawKNW$geo=="FR"]
FRAKNW$val[FRAKNW$BNACE %in% c("F")]=RawKNW$values[RawKNW$nace_r2=="J_K" & RawKNW$geo=="FR"]
FRAKNW$val[is.na(FRAKNW$val)==T]=RawKNW$values[RawKNW$nace_r2=="TOTAL" & RawKNW$geo=="FR"]
FRAKNW$val=FRAKNW$val/100
for(i in 1:nrow(FRAKNW)){
  FRAKNW$val[i]=100*FRAKNW$val[i]*CPEB$D1[CPEB$CNA_ACTIVITE==FRAKNW$NACE[i]]/CPEB$B1G[CPEB$CNA_ACTIVITE==FRAKNW$NACE[i]]
}


EU28KNW=RawKNW$values[RawKNW$nace_r2=="TOTAL" & RawKNW$geo=="EU28"]/RawKNW$values[RawKNW$nace_r2=="TOTAL" & RawKNW$geo=="FR"]

Source="Insee and Eurostat"
Unit="P100"

Matching=read.csv("https://raw.githubusercontent.com/La-Societe-Nouvelle/LaSocieteNouvelle-defautdata/master/DefaultData-LSN/donnees/TableFonctionTransition.csv",sep=";",header=T,col.names = c("Numero.de.la.division","Libelle.de.la.division","Code.de.la.branche.associee","Code.OCDE"))
Matching$Numero.de.la.division[nchar(Matching$Numero.de.la.division)==1]=paste0("0",Matching$Numero.de.la.division[nchar(Matching$Numero.de.la.division)==1])

DivKNW=as.data.frame(Matching[-88,c(1,3)])
for(i in 1:nrow(DivKNW)){
  DivKNW$value[i]=FRAKNW$val[FRAKNW$NACE==DivKNW$Code.de.la.branche.associee[i]]
}
DivKNW$value[DivKNW$Numero.de.la.division%in%c("72","85")]=100

KNWData=list(FRAKNW[,-2],EU28KNW,Source,Unit,DivKNW[,c(1,3)])
return(KNWData)}}
