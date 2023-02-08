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
#' @param verbose TRUE by default, FALSE refers to the silent mode
#'
#' @importFrom dplyr %>%
#' @importFrom dplyr mutate
#' @importFrom tidyr pivot_wider
#' @importFrom stringr str_remove
#'
#' @return A table of macroeconomic footprint values by economic activities division.
#'
#' @seealso \code{\link{build_branches_fpt}}, \code{\link{get_indicator_list}}
#'
#' @examples
#' build_divisions_fpt("ART",2015)
#' build_divisions_fpt("ECO",2019)
#' build_divisions_fpt("GEQ",2018)
#' build_divisions_fpt("GHG",2019)
#' build_divisions_fpt("HAZ",2019)
#' build_divisions_fpt("IDR",2019)
#' build_divisions_fpt("KNW",2015)
#' build_divisions_fpt("MAT",2019)
#' build_divisions_fpt("NRG",2019)
#' build_divisions_fpt("SOC",2015)
#' build_divisions_fpt("WAS",2018)
#' build_divisions_fpt("WAT",2018)
#' @export

build_divisions_fpt = function(indicator,year,verbose=T)
{

  indicator = tolower(as.character(indicator))

  divisions = lsnr:::Divisions

  if(verbose == F)
  {
    rec_fl = tempfile(fileext = ".txt")
    sink(rec_fl,type = c("output", "message")) #Record console output in a temp file
  }

  # divisions_aggregates = get_divisions_aggregates(year)

  # build branches data
  fpt_branches = build_branches_fpt(indicator, year,verbose)

  # get nva data
  nva_fpt = suppressMessages(get_divisions_nva_fpt(indicator,year))

  # divisions fpt
  fpt_divisions = get_empty_divisions_fpt(divisions)

  divisions_aggregates = suppressMessages(get_divisions_aggregates(year))

  for(i in 1:nrow(fpt_divisions))
  {
    branch = divisions$BRANCH[i]
    fpt_divisions$NVA_FPT[i] = nva_fpt$FOOTPRINT[i]
    fpt_divisions$IC_FPT[i]  = as.numeric(fpt_branches %>% filter(BRANCH == branch & AGGREGATE == "IC") %>% select(VALUE))
    fpt_divisions$CFC_FPT[i]  = as.numeric(fpt_branches %>% filter(BRANCH == branch & AGGREGATE == "CFC") %>% select(VALUE))
    fpt_divisions$RESS_FPT[i]  = as.numeric(fpt_branches %>% filter(BRANCH == branch & AGGREGATE == "RESS") %>% select(VALUE))
    fpt_divisions$IMP_FPT[i]  = as.numeric(fpt_branches %>% filter(BRANCH == branch & AGGREGATE == "IMP") %>% select(VALUE))
    fpt_divisions$TRESS_FPT[i]  = as.numeric(fpt_branches %>% filter(BRANCH == branch & AGGREGATE == "TRESS") %>% select(VALUE))
    fpt_divisions$PRD_FPT[i] = (fpt_divisions$NVA_FPT[i]*divisions_aggregates$NVA[i] + fpt_divisions$IC_FPT[i]*divisions_aggregates$IC[i] + fpt_divisions$CFC_FPT[i]*divisions_aggregates$CFC[i]) / divisions_aggregates$PRD[i]
  }

  output_2 = fpt_divisions %>%
    pivot_longer(!DIV, names_to = "AGGREGATE", values_to = "VALUE") %>%
    mutate(AGGREGATE = str_remove(AGGREGATE,"_FPT"))

  indic_metadata = lsnr:::IndicatorsMetadata

  output_2$YEAR = year
  output_2$UNIT = indic_metadata$UNIT[indic_metadata$CODE==toupper(indicator)]
  output_2$INDIC = toupper(indicator)

  if(verbose == F)
  {
    sink() ; closeAllConnections() #Stop recording
    unlink(rec_fl) #Delete console ouput
  }

  return(output_2)
}

get_empty_divisions_fpt = function(divisions)
{
  fpt_divisions = as.data.frame(divisions$DIVISION)
  names(fpt_divisions)="DIV"
  fpt_divisions[,c('NVA_FPT','IC_FPT','CFC_FPT','PRD_FPT')] = c(0,0,0,0)
  return(fpt_divisions)
}
