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
FetchDataDisponibility=function(Indicator){
  ERE=1949:2020
  TESS=c(2010:2013,2015:2018)
  TEI=1949:2019
  CPEB=1949:2020
  base=ERE[ERE %in% TESS & ERE %in% TEI & ERE %in% CPEB]
  Indicator=tolower(Indicator)
  if(Indicator=="knw"){
    KNW=c(2010,2015)
    base=base[base %in% KNW]
  }
  if(Indicator=="ghg"){
    GHG=c(2008:2020)
    WGHG=c(1970:2018)
    base=base[base %in% GHG & base %in% WGHG]
  }
  if(Indicator=="wat"){
    WAT=c(2010:2018)
    base=base[base %in% WAT]
  }
  if(Indicator=="eco"){
    base=base}
  if(Indicator=="soc"){
    SOC=2015
    base=base[base %in% SOC]}
  if(Indicator=="was"){
    WAS=c(2010,2012,2014,2016,2018)
    NAMA=c(1975:2021)
    base=base[base %in% WAS & base %in% NAMA]
  }
  if(Indicator=="nrg"){
    NRG=c(2014:2019)
    NAMA=c(1975:2021)
    base=base[base %in% NRG & base %in% NAMA]
  }
  if(Indicator=="geq"){
    GEQ=c(2006,2010,2014,2018)
    base=base[base %in% GEQ]
  }
  if(Indicator=="dis"){
    DIS=c(1995:2020)
    base=base[base %in% DIS]
  }
  if(Indicator=="haz"){
    HAZ=c(2004:2020)
    base=base[base %in% HAZ]
  }
  if(Indicator=="mat"){
    MAT=c(1990:2021)
    NAMA=c(1975:2021)
    base=base[base %in% MAT & base %in% NAMA]
  }
  if(Indicator=="art"){
    art=2015
    base=base[base %in% art]}
  return(base)
}
