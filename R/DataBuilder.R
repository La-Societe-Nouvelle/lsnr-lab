
get_branches_nva_fpt = function(indic,year)
{
  # Init financial data

  data = switch(indic,
    "art" = try(build_branches_nva_fpt_art(year),silent = T),
    "eco" = try(lsnr:::build_branches_nva_fpt_eco(year),silent = T),
    "geq" = try(build_branches_nva_fpt_geq(year),silent = T),
    "ghg" = try(build_branches_nva_fpt_ghg(year),silent = T),
    "haz" = try(build_branches_nva_fpt_haz(year),silent = T),
    "idr" = try(build_branches_nva_fpt_idr(year),silent = T),
    "knw" = try(build_branches_nva_fpt_knw(year),silent = T),
    "mat" = try(build_branches_nva_fpt_mat(year),silent = T),
    "nrg" = try(build_branches_nva_fpt_nrg(year),silent = T),
    "soc" = try(build_branches_nva_fpt_soc(year),silent = T),
    "was" = try(build_branches_nva_fpt_was(year),silent = T),
    "wat" = try(build_branches_nva_fpt_wat(year),silent = T)
  )

  if("try-error" %in% class(data) || nrow(data) != nrow(lsnr:::Branches))
  {
    stop(paste0("Données d'impacts des branches de l'indicateur ",toupper(indic)," indisponibles pour l'année ", year))
  }

  return(data)
}

get_divisions_nva_fpt = function(indic,year)
{
  # Init financial data

  data = switch(indic,
                "art" = try(build_divisions_nva_fpt_art(year),silent = T),
                "eco" = try(build_divisions_nva_fpt_eco(year),silent = T),
                "geq" = try(build_divisions_nva_fpt_geq(year),silent = T),
                "ghg" = try(build_divisions_nva_fpt_ghg(year),silent = T),
                "haz" = try(build_divisions_nva_fpt_haz(year),silent = T),
                "idr" = try(build_divisions_nva_fpt_idr(year),silent = T),
                "knw" = try(build_divisions_nva_fpt_knw(year),silent = T),
                "mat" = try(build_divisions_nva_fpt_mat(year),silent = T),
                "nrg" = try(build_divisions_nva_fpt_nrg(year),silent = T),
                "soc" = try(build_divisions_nva_fpt_soc(year),silent = T),
                "was" = try(build_divisions_nva_fpt_was(year),silent = T),
                "wat" = try(build_divisions_nva_fpt_wat(year),silent = T)
  )

  if("try-error" %in% class(data) || nrow(data) != nrow(lsnr:::Divisions)-1)
  {
    stop(paste0("Données d'impacts des divisions de l'indicateur ",toupper(indic)," indisponibles pour l'année ", year))
  }

  return(data)
}


get_branches_imp_coef = function(indic,year)
{
  # Init financial data

  data = switch(indic,
    "art" = try(get_branches_imp_coef_art(year),silent = T),
    "eco" = try(get_branches_imp_coef_eco(year),silent = T),
    "geq" = try(get_branches_imp_coef_geq(year),silent = T),
    "ghg" = try(get_branches_imp_coef_ghg(year),silent = T),
    "haz" = try(get_branches_imp_coef_haz(year),silent = T),
    "idr" = try(get_branches_imp_coef_idr(year),silent = T),
    "knw" = try(get_branches_imp_coef_knw(year),silent = T),
    "mat" = try(get_branches_imp_coef_mat(year),silent = T),
    "nrg" = try(get_branches_imp_coef_nrg(year),silent = T),
    "soc" = try(get_branches_imp_coef_soc(year),silent = T),
    "was" = try(get_branches_imp_coef_was(year),silent = T),
    "wat" = try(get_branches_imp_coef_wat(year),silent = T)
  )

  if("try-error" %in% class(data) || length(data) != 1)
  {
    stop(paste0("Coefficient d'impacts des importations de l'indicateur ",toupper(indic)," indisponible pour l'année ", year))
  }
  return(data)
}
