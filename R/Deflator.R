#'Correct monetary value with respect to French inflation.
#'
#'Returns a deflated value.
#'
#' @param Value Initial value to deflate.
#' @param Start Initial y.
#' @param Target Initial value to deflate.
#'
#' @importFrom insee get_insee_idbank
#'
#' @return A `numeric` value.
#' @seealso \code{\link{BuildBranchesData}}, \code{\link{BuildDivisionsData}}, \code{\link{FetchDataDisponibility}}.
#' @examples
#' Deflator(1250,1990,2020)
#' @export
Deflator=function(Value, Start, Target){
  if(is.numeric(Value)==F|is.numeric(Start)==F|is.numeric(Target)==F){return("Value,Start and Target must be numeric")}
  else{
    index=get_insee_idbank("010605954")[c(2,3)]
    deflated=Value*index$OBS_VALUE[index$TIME_PERIOD==Target]/index$OBS_VALUE[index$TIME_PERIOD==Start]
    return(deflated)
  }
}
