#'Compute default values of societal footprints by NACE divisions (88).
#'
#'#'Unlike BuildDivisionsData function, this derived model take into
#'account fixed capital consumption.
#'
#'This function returns a table summarizing final default values
#'of the chosen indicator on the selected year.
#'
#' @param Year Considered year.
#' @param Indicator Considered indicator.
#'
#' @return A `data.frame` object containing final default values by economic activities divisions.
#' @seealso \code{\link{BuildBranchesData}}, \code{\link{FetchDataDisponibility}}.
#' @examples
#' BuildDivisionsDataV2("GHG",2018)
#' @export

source('R/BuildBranchesData.R')

build_divisions_fpt = function(indicator,year)
{
  wd = getwd()
  path = paste0(wd,"/lib/","Divisions.csv")

  divisions = read.csv(path, header=T, sep=";")

  # divisions_aggregates = get_divisions_aggregates(year)

  # build branches data
  fpt_branches = buildBranchesData(toupper(Indicator), year)

  # get nva data
  nva_fpt = get_divisions_nva_fpt(indicator,year)

  # divisions fpt
  fpt_divisions = get_empty_divisions_fpt(divisions)

  for(i in 1:nrow(fpt_divisions)) 
  {
    branch = divisions$BRANCH[i]
    fpt_divisions$NVA_FPT[i] = nva_fpt$FOOTPRINT[i]
    fpt_divisions$IC_FPT[i]  = fpt_branches$IC_FPT[fpt_branches$BRANCH==branch]
    fpt_divisions$CFC_FPT[i]  = fpt_branches$CFC_FPT[fpt_branches$BRANCH==branch]
    fpt_divisions$PRD_FPT[i] = (nva_fpt$FOOTPRINT[i]*divisions_aggregates$NVA[i] + fpt_branches$IC_FPT[i]*divisions_aggregates$IC[i] + fpt_divisions$CFC_FPT[i]*divisions_aggregates$CFC[i]) / divisions_aggregates$PRD[i]
  }

  output_2 = fpt_divisions %>% 
    pivot_longer(!DIV, names_to = "AGGREGATE", values_to = "VALUE") %>%
    mutate(AGGREGATE = str_remove(AGGREGATE,"_FPT"))

  indic_metadata = read.csv(paste0(wd,"/lib/","IndicatorsMetadata.csv"), header=T, sep=";")

  output_2$YEAR = year
  output_2$UNIT = indic_metadata$UNIT[indic_metadata$CODE==toupper(indicator)]
  output_2$INDIC = toupper(indicator)

  return(output_2)
}

get_empty_divisions_fpt = function(divisions)
{
  fpt_divisions = as.data.frame(divisions$CODE)
  names(fpt_divisions)="DIV"
  fpt_divisions[,c('NVA_FPT','IC_FPT','CFC_FPT','PRD_FPT')] = c(0,0,0,0)
  return(fpt_divisions)
}