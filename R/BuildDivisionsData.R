#' Compute macroeconomic values of societal footprints by NACE divisions (88)
#'
#' @details This function aims to compute French branch societal footprints by non-financial dimension by year.
#' It involves, on one hand, a macroeconomic input-output modelization of the French economy and its interactions with
#' the rest of the world, based on INSEE IOTs, and requires, on the other hand, direct impact data from institutional sources.
#' Specifically BuildDivisionsData compute division footprints by adjusting branch footprints thanks to
#' division economic aggregate values.
#'
#' @param year year of requested data.
#' @param indicator requested non-financial dimension.
#'
#' @importFrom dplyr %>%
#' @importFrom dplyr mutate
#' @importFrom tidyr pivot_wider
#' @importFrom stringr str_remove
#'
#' @return A table of macroeconomic footprint values by economic activities division.
#'
#' @seealso \code{\link{buildBranchesData}}, \code{\link{getIndicatorList}}, \code{\link{buildDiscountedData}}.
#'
#' @examples
#' buildDivisions("ECO",2019)
#' @export

build_divisions_fpt = function(indicator,year)
{

  indicator = tolower(as.character(indicator))

  divisions = lsnr:::Divisions

  # divisions_aggregates = get_divisions_aggregates(year)

  # build branches data
  fpt_branches = build_branches_fpt(indicator, year)

  # get nva data
  nva_fpt = get_divisions_nva_fpt(indicator,year)

  # divisions fpt
  fpt_divisions = get_empty_divisions_fpt(divisions)

  divisions_aggregates = get_divisions_aggregates(year)

  for(i in 1:nrow(fpt_divisions))
  {
    branch = divisions$BRANCH[i]
    fpt_divisions$NVA_FPT[i] = nva_fpt$FOOTPRINT[i]
    fpt_divisions$IC_FPT[i]  = as.numeric(fpt_branches %>% filter(BRANCH == branch & AGGREGATE == "IC") %>% select(VALUE))
    fpt_divisions$CFC_FPT[i]  = as.numeric(fpt_branches %>% filter(BRANCH == branch & AGGREGATE == "CFC") %>% select(VALUE))
    fpt_divisions$PRD_FPT[i] = (fpt_divisions$NVA_FPT[i]*divisions_aggregates$NVA[i] + fpt_divisions$IC_FPT[i]*divisions_aggregates$IC[i] + fpt_divisions$CFC_FPT[i]*divisions_aggregates$CFC[i]) / divisions_aggregates$PRD[i]
  }

  output_2 = fpt_divisions %>%
    pivot_longer(!DIV, names_to = "AGGREGATE", values_to = "VALUE") %>%
    mutate(AGGREGATE = str_remove(AGGREGATE,"_FPT"))

  indic_metadata = lsnr:::IndicatorsMetadata

  output_2$YEAR = year
  output_2$UNIT = indic_metadata$UNIT[indic_metadata$CODE==toupper(indicator)]
  output_2$INDIC = toupper(indicator)

  return(output_2)
}

get_empty_divisions_fpt = function(divisions)
{
  fpt_divisions = as.data.frame(divisions$DIVISION)
  names(fpt_divisions)="DIV"
  fpt_divisions[,c('NVA_FPT','IC_FPT','CFC_FPT','PRD_FPT')] = c(0,0,0,0)
  return(fpt_divisions)
}
