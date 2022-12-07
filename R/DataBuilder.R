
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
    "dis" = build_branches_nva_fpt_dis(year),
    "eco" = build_branches_nva_fpt_eco(year),
    "geq" = build_branches_nva_fpt_geq(year),
    "ghg" = build_branches_nva_fpt_ghg(year),
    "haz" = build_branches_nva_fpt_haz(year),
    "knw" = build_branches_nva_fpt_knw(year),
    "mat" = build_branches_nva_fpt_mat(year),
    "nrg" = build_branches_nva_fpt_nrg(year),
    "soc" = build_branches_nva_fpt_soc(year),
    "was" = build_branches_nva_fpt_was(year),
    "wat" = build_branches_nva_fpt_wat(year)
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