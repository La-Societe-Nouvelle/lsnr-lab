#'Fetch societal data disponibility.
#'
#'Returns indicator-specific available years, in line with data sources used. An automation of the function actualization process is in working progress.
#'
#' @param Indicator Considered year.
#'
#' @return An `integer` object containing available year(s) for considered indicator.
#' @seealso \code{\link{BuildECOData}}, \code{\link{BuildGHGData}}, \code{\link{BuildNRGData}},
#'  \code{\link{BuildBranchesData}}, \code{\link{BuildDivisionsData}}, \code{\link{FetchDataDisponibility}}.
#' @examples
#' Year=FetchDataDisponibility("GHG")
#' BuildGHGData(Year)
#' @export


# DATASETS LIBRARIES

buildLSNREnv = function()
{
  lsnrEnv$years_ere = 1949:2020                     # Data Equilibre-Ressources-Emplois
  lsnrEnv$years_tess = c(2010:2013,2015:2018)       # Data Tableau Entrée-Sortie Symétrique
  lsnrEnv$years_tei = 1949:2019                     # Data Tableau des Entrées Intermédiaires
  lsnrEnv$years_cpeb = 1949:2020                    # Data Compte de Prodction & Ressource par branche

  return(lsnrEnv)
}

FetchDataDisponibility = function(indicator) 
{
  years_ere=1949:2020 # ERE ?
  years_tess=c(2010:2013,2015:2018)
  years_tei=1949:2019
  years_cpeb=1949:2020
  base=years_ere[years_ere %in% years_tess & years_ere %in% years_tei & years_ere %in% years_cpeb]

  indicator=tolower(indicator)
  
  if(indicator=="knw"){
    KNW=c(2010,2015)
    base=base[base %in% KNW]
  }
  if(indicator=="ghg"){
    GHG=c(2008:2020)
    WGHG=c(1970:2018)
    base=base[base %in% GHG & base %in% WGHG]
  }
  if(indicator=="wat"){
    WAT=c(2010:2018)
    base=base[base %in% WAT]
  }
  if(indicator=="eco"){
    base=base}
  if(indicator=="soc"){
    SOC=2015
    base=base[base %in% SOC]}
  if(indicator=="was"){
    WAS=c(2010,2012,2014,2016,2018)
    NAMA=c(1975:2021)
    base=base[base %in% WAS & base %in% NAMA]
  }
  if(indicator=="nrg"){
    NRG=c(2014:2019)
    NAMA=c(1975:2021)
    base=base[base %in% NRG & base %in% NAMA]
  }
  if(indicator=="geq"){
    GEQ=c(2006,2010,2014,2018)
    base=base[base %in% GEQ]
  }
  if(indicator=="dis"){
    DIS=c(1995:2020)
    base=base[base %in% DIS]
  }
  if(indicator=="haz"){
    HAZ=c(2004:2020)
    base=base[base %in% HAZ]
  }
  if(indicator=="mat"){
    MAT=c(1990:2021)
    NAMA=c(1975:2021)
    base=base[base %in% MAT & base %in% NAMA]
  }
  if(indicator=="art"){
    art=2015
    base=base[base %in% art]
    print(base)
  }
  return(base)
}
