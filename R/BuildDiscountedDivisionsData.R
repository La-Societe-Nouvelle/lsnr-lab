#' Compute dicounted macroeconomic values of societal footprints by NACE divisions (88).
#'
#' @details This function calls the latest available footprints for divisions and discount it in specified-year euro terms.
#'
#' @importFrom insee get_insee_idbank
#' @importFrom stringr str_remove
#'
#' @param startYear year of original data.
#' @param endYear year of discounted data.
#' @param indicator requested non-financial dimension.
#' @param verbose TRUE by default, FALSE refers to the silent mode
#'
#' @return A table of macroeconomic footprint values by economic activities division.
#'
#' @seealso \code{\link{build_divisions_fpt}}, \code{\link{get_indicator_list}}.
#'
#' @examples
#'
#' build_divisions_fpt("GHG",2019,2022)
#'
#' @export

build_discounted_divisions_fpt=function(indicator,startYear,endYear = as.numeric(format(Sys.Date(), "%Y"))-1 ,verbose = T)
{
  original_data = build_divisions_fpt(indicator,startYear,verbose = verbose)

  if(str_detect(unique(original_data$UNIT),"EUR"))

  {

    price_data = suppressMessages(get_insee_idbank("010605954"))

    if(endYear %in% price_data$TIME_PERIOD == F )
    {
      stop(paste0("Indice des prix à la consommation indisponible pour l'année ", endYear))
    }

    if(startYear %in% price_data$TIME_PERIOD == F )
    {
      stop(paste0("Indice des prix à la consommation indisponible pour l'année ", startYear))
    }

    discount_coeff = price_data$OBS_VALUE[price_data$TIME_PERIOD == endYear] / price_data$OBS_VALUE[price_data$TIME_PERIOD == startYear]

    original_data$VALUE = discount_coeff * original_data$VALUE

    original_data$YEAR = endYear

    original_data$INFO = paste0(startYear," footprint expressed in ", endYear, " terms (discounted)")

  }

  return(original_data)

}
