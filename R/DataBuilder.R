
get_branches_nva_fpt = function(indic,year)
{
  # Init financial data

  data = switch(indic,
    "art" = build_branches_nva_fpt_art(year),
    "eco" = build_branches_nva_fpt_eco(year),
    "geq" = build_branches_nva_fpt_geq(year),
    "ghg" = build_branches_nva_fpt_ghg(year),
    "haz" = build_branches_nva_fpt_haz(year),
    "idr" = build_branches_nva_fpt_idr(year),
    "knw" = build_branches_nva_fpt_knw(year),
    "mat" = build_branches_nva_fpt_mat(year),
    "nrg" = build_branches_nva_fpt_nrg(year),
    "soc" = build_branches_nva_fpt_soc(year),
    "was" = build_branches_nva_fpt_was(year),
    "wat" = build_branches_nva_fpt_wat(year)
  )
  return(data)
}

get_divisions_nva_fpt = function(indic,year)
{
  # Init financial data

  data = switch(indic,
                "art" = build_divisions_nva_fpt_art(year),
                "eco" = build_divisions_nva_fpt_eco(year),
                "geq" = build_divisions_nva_fpt_geq(year),
                "ghg" = build_divisions_nva_fpt_ghg(year),
                "haz" = build_divisions_nva_fpt_haz(year),
                "idr" = build_divisions_nva_fpt_idr(year),
                "knw" = build_divisions_nva_fpt_knw(year),
                "mat" = build_divisions_nva_fpt_mat(year),
                "nrg" = build_divisions_nva_fpt_nrg(year),
                "soc" = build_divisions_nva_fpt_soc(year),
                "was" = build_divisions_nva_fpt_was(year),
                "wat" = build_divisions_nva_fpt_wat(year)
  )
  return(data)
}


get_branches_imp_coef = function(indic,year)
{
  # Init financial data

  data = switch(indic,
    "art" = get_branches_imp_coef_art(year),
    "eco" = get_branches_imp_coef_eco(year),
    "geq" = get_branches_imp_coef_geq(year),
    "ghg" = get_branches_imp_coef_ghg(year),
    "haz" = get_branches_imp_coef_haz(year),
    "idr" = get_branches_imp_coef_idr(year),
    "knw" = get_branches_imp_coef_knw(year),
    "mat" = get_branches_imp_coef_mat(year),
    "nrg" = get_branches_imp_coef_nrg(year),
    "soc" = get_branches_imp_coef_soc(year),
    "was" = get_branches_imp_coef_was(year),
    "wat" = get_branches_imp_coef_wat(year)
  )
  return(data)
}
