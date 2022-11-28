#'Build and returns all data required to the ART indicator computations.
#'
#'Returns a `list` made up of value added impacts by French branches, imported products associated coefficient,
#'Data sources and values unit. This data will be used in both BuildBranchesData and BuildDivisionsData functions.
#'
#' @param Year Considered Year.
#'
#' @return An object `list` made up of 4 elements : value added impacts by French branches,
#' imported products associated coefficient, data sources and values unit.
#' @seealso \code{\link{BuildECOData}}, \code{\link{BuildGHGData}}, \code{\link{BuildNRGData}},
#'  \code{\link{BuildBranchesData}}, \code{\link{BuildDivisionsData}}, \code{\link{FetchDataDisponibility}}.
#' @examples
#' BuildARTData(max(FetchDataDisponibility("ART"))
#' @export

source('R/InseeDataManager.R')

buildARTDataOld = function(year) 
{
  print("Build ART Data")
  availableYears = FetchDataDisponibility("ART")
  if((year %in% availableYears)) 
  {
    ##Build ERE Database
    ERE=get_products_aggregates(year)
    ReferenceTable=as.data.frame(unique(ERE$CNA_PRODUIT))

    ##Build CPEB Database : P1 - P2

    CPEB=get_branches_aggregates(year)

    RAWART=as.data.frame(cbind(CPEB$CNA_ACTIVITE,CPEB$B1G))
    for(i in 1:nrow(RAWART)) {
      RAWART$val[i]=as.numeric(RAWART[i,2])*111600/sum(as.numeric(RAWART[,2]))
      RAWART$ComputedValue[i]=100*RAWART$val[i]/as.numeric(RAWART[i,2])
    }
    
    FRAART=RAWART[,c(1,4)]
    #IMPART=0

    #DataART=list(FRAART,IMPART,Source,Unit)
    return(FRAART)
  } 
  else {
    return("Desired year is unavailable. Please, refer to FetchDataDisponibility function in order to find available years for a given indicator")
  }
}

buildARTData = function(year) 
{
  ## Get Insee data
  insee_data_ere = get_products_aggregates(year)
  insee_data_cpeb = getInseeDataCPEB(year)

  branches = insee_data_cpeb$CNA_ACTIVITE

  art_data = as.data.frame(cbind(insee_data_cpeb$CNA_ACTIVITE, insee_data_cpeb$B1G, NA, NA, NA))
  colnames(art_data) = c("CNA_ACTIVITE", "B1G", "FOOTPRINT", "UNIT_FOOTPRINT", "GROSS_IMPACT")

  for(i in 1:nrow(insee_data_cpeb))
  {
    art_data$GROSS_IMPACT[i] = round((as.numeric(art_data[i,2]) / sum(as.numeric(art_data[,2]))) * 111600, digits = 3)
    art_data$FOOTPRINT[i] = round(100*(111600 / sum(as.numeric(art_data[,2]))), digits = 3)
    art_data$UNIT_FOOTPRINT[i] = "P100"
  }

  return(art_data)
}

get_branches_imp_coef_art = function(year)
{
  print('imp_coef')
  branches_imp_coef = 0
  print(branches_imp_coef)
  return(branches_imp_coef)
}