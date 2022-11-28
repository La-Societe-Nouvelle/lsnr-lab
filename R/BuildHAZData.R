#'Build and returns all data required to the HAZ indicator computations.
#'
#'Returns a `list` made up of value added impacts by French branches, imported products associated coefficient,
#'Data sources and values unit. This data will be used in both BuildBranchesData and BuildDivisionsData functions.
#'
#' @param Year Considered Year.
#'
#' @return An object `list` made up of 4 elements : value added impacts by French branches,
#' imported products associated coefficient, data sources and values unit.
#' @seealso \code{\link{BuildECOData}}, \code{\link{BuildGHGData}},
#'  \code{\link{BuildBranchesData}}, \code{\link{BuildDivisionsData}}, \code{\link{FetchDataDisponibility}}.
#' @examples
#' BuildHAZData(max(FetchDataDisponibility("HAZ"))
#' @export
BuildHAZData=function(Year){

  HAZDisponibility=FetchDataDisponibility("HAZ")
  if((Year %in% HAZDisponibility)==F){
    return("Desired year is unavailable. Please, refer to FetchDataDisponibility function in order to find available years for a given indicator")
  }
  else{

    ##Build ERE Database
    ERE=get_products_aggregates(Year)
    ReferenceTable=as.data.frame(unique(ERE$CNA_PRODUIT))

    ##Build CPEB Database : P1 - P2

    CPEB=get_branches_aggregates(Year)

    RawHAZ=get_eurostat("env_chmhaz", time_format = "date", filters = list(hazard = "HAZ", time = Year, indic_env ="CONS"))
    GDPEU27=get_eurostat("nama_10_a64")
    GDPEU27=GDPEU27[GDPEU27$geo %in% c("EU27_2020","FR") & GDPEU27$na_item=="B1G" & GDPEU27$unit=="CP_MEUR" & GDPEU27$time==paste0(Year,"-01-01") & GDPEU27$nace_r2=="TOTAL",]

    FRAHAZ=as.data.frame(ReferenceTable)
    names(FRAHAZ)="id"
    for(i in 1:nrow(FRAHAZ)){
      FRAHAZ$val[i]=RawHAZ$values[RawHAZ$geo=="EU27_2020"]*1000000/GDPEU27$values[GDPEU27$geo=="EU27_2020"]
    }

    EU27HAZ=(RawHAZ$values[RawHAZ$geo=="EU27_2020"]*1000000/GDPEU27$values[GDPEU27$geo=="EU27_2020"])/(RawHAZ$values[RawHAZ$geo=="EU27_2020"]*1000000/GDPEU27$values[GDPEU27$geo=="EU27_2020"])

    Source="Insee and Eurostat"
    Unit="G_CPEUR"
    DataHAZ=list(FRAHAZ,EU27HAZ,Source,Unit)
    return(DataHAZ)}}
