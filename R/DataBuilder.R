
source("R/BuildARTData.R")
source("R/BuildDISData.R")
source("R/BuildECOData.R")
source("R/BuildGEQData.R")
source("R/BuildGHGData.R")
source("R/BuildHAZData.R")
source("R/BuildKNWData.R")
source("R/BuildMATData.R")
source("R/BuildNRGData.R")
source("R/BuildSOCData.R")
source("R/BuildWASData.R")
source("R/BuildWATData.R")

get_branches_nva_fpt = function(indic,year)
{
  # Init financial data

  data = switch(indic,
    "art" = build_branches_nva_fpt_art(year),
    "dis" = build_branches_nva_fpt_dis()(year),
    "eco" = build_branches_nva_fpt_eco()(year),
    "geq" = buildGEQData(year),
    "ghg" = buildGHGData(year),
    "haz" = buildHAZData(year),
    "knw" = buildKNWData(year),
    "mat" = buildMATData(year),
    "nrg" = buildNRGData(year),
    "soc" = buildSOCData(year),
    "was" = buildWASData(year),
    "wat" = buildWATData(year)
  )
}

get_branches_imp_coef = function(indic,year)
{
  # Init financial data

  data = switch(indic,
    "art" = get_branches_imp_coef_art(year),
    "dis" = get_branches_imp_coef_dis(year),
    "eco" = get_branches_imp_coef_eco(year)
  )
}