#' @importFrom dplyr rename_all
#' @importFrom lsnstat lsnstat_macrodata

get_branches_nva_fpt = function(indic,year)
{
  # Init financial data

  data = switch(indic,
    "art" = try(build_branches_nva_fpt_art(year),silent = T),
    "eco" = try(build_branches_nva_fpt_eco(year),silent = T),
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

  if(inherits(data,"try-error") || nrow(data) != nrow(lsnr:::Branches))
  {
    if(year <= 2030 & year > 2010)
    {
    data = get_forecasted_fpt(indic,year,"B")
    return(data)
    }
    stop(paste0("Donnees d'impacts des branches de l'indicateur ",toupper(indic)," indisponibles pour l'annee ", year))
  }

  data = data %>% mutate(flag = "")

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

  if(inherits(data,"try-error") || nrow(data) != nrow(lsnr:::Divisions)-1)
  {
    if(year <= 2030 & year > 2010)
    {
      data = get_forecasted_fpt(indic,year,"D")
      return(data)
    }

    stop(paste0("Donnees d'impacts des divisions de l'indicateur ",toupper(indic)," indisponibles pour l'annee ", year))
  }

  data = data %>% mutate(flag = "")

  return(data)
}


get_branches_imp_coef = function(indic,year)
{
  # Init financial data

  data = NA
  attempt = 0

  while((all(is.na(data)) || inherits(data,"try-error") || all(is.na(data))) & year >= 2000 & year <= 2030)
  {

    if(attempt > 0)
      {print(paste0("Coefficient d'impacts des importations de l'indicateur ",toupper(indic)," indisponible pour l'annee ", fyear,", tentative pour l'annee ",fyear - 1))}

    way = ifelse(year - 2015 <= 0, +1, -1)

    fyear = year + attempt * way

  data = switch(indic,
    "art" = try(get_branches_imp_coef_art(fyear),silent = T),
    "eco" = try(get_branches_imp_coef_eco(fyear),silent = T),
    "geq" = try(get_branches_imp_coef_geq(fyear),silent = T),
    "ghg" = try(lsnr:::get_branches_imp_coef_ghg(fyear),silent = T),
    "haz" = try(get_branches_imp_coef_haz(fyear),silent = T),
    "idr" = try(get_branches_imp_coef_idr(fyear),silent = T),
    "knw" = try(get_branches_imp_coef_knw(fyear),silent = T),
    "mat" = try(get_branches_imp_coef_mat(fyear),silent = T),
    "nrg" = try(get_branches_imp_coef_nrg(fyear),silent = T),
    "soc" = try(get_branches_imp_coef_soc(fyear),silent = T),
    "was" = try(get_branches_imp_coef_was(fyear),silent = T),
    "wat" = try(get_branches_imp_coef_wat(fyear),silent = T)
  )

  attempt = attempt + abs(way)

  if(attempt == 20)
    {
      stop(paste0("Coefficient d'impacts des importations de l'indicateur ",toupper(indic)," indisponible pour l'annee ", year))
  }


  }

  if(attempt > 0){
    print(paste0("Annee retenue : ",fyear))
  }

  return(data)
}

get_impact_availability = function(indicator,Force = F)
{
  indicator = tolower(indicator)
  if(Force == F){
    return(lsnr:::internal_impact_availability[[indicator]][lsnr:::internal_impact_availability[[indicator]] >= 2000])
  }


  endpoints = as.data.frame(t(matrix(c(
    "ghg", "https://ec.europa.eu/eurostat/api/dissemination/statistics/1.0/data/env_ac_ainah_r2?geo=FR&time_period=",
    "knw", "https://ec.europa.eu/eurostat/api/dissemination/statistics/1.0/data/trng_cvt_16n2?geo=FR&time_period=",
    "mat","https://ec.europa.eu/eurostat/api/dissemination/statistics/1.0/data/env_ac_mfa?geo=FR&time_period=",
    "nrg","https://ec.europa.eu/eurostat/api/dissemination/statistics/1.0/data/env_ac_pefasu?geo=FR&time_period=",
    "was","https://ec.europa.eu/eurostat/api/dissemination/statistics/1.0/data/env_wasgen?geo=FR&time_period=",
    "wat","https://ec.europa.eu/eurostat/api/dissemination/statistics/1.0/data/env_wat_abs?geo=FR&time_period="),
    nrow = 2))) %>% setNames(c("indicator","link"))

  yrs = list()
  for(Indicator in endpoints$indicator){
    res = GET(paste0(endpoints$link[endpoints$indicator == Indicator],"lastTimePeriod"))
    content = fromJSON(rawToChar(res$content))


    yrs[[Indicator]] = content$extension$annotation$title[content$extension$annotation$type %in% c("OBS_PERIOD_OVERALL_LATEST","OBS_PERIOD_OVERALL_OLDEST")]
  }

  df_years = setNames(as.data.frame(t(as.data.frame(yrs))),c("end","start"))

  years = list()

  for(i in row.names(df_years)){
    vec = vector()
    int = yrs[[i]][2] : yrs[[i]][1]
    for(x in int){
      res = GET(paste0(endpoints$link[endpoints$indicator == i],x))
      content = fromJSON(rawToChar(res$content))
      if(length(content$value) > 0)

      {

        vec[length(vec)+1] = x
        print(paste0(i,"  :  ",x))
      }
      years[[i]] = vec
    }
  }

  #Patch WAS

  years[["was"]] = years[["was"]][years[["was"]] >= 2010]

  #Patch WAT

  years[["wat"]] = years[["wat"]][years[["wat"]] >= 2010]

  #HAZ
  link = "https://api.lasocietenouvelle.org/serie/MACRO_HAZARDOUSPRODUCTS_PRODQNT_PRODCOM_FRA_T"
  res = GET(link)
  content = fromJSON(rawToChar(res$content))

  years[["haz"]] = as.numeric(content$data$year)[-which(as.numeric(content$data$year) == "2021")]

  #GEQ

  years[["geq"]] = 2016:2020

  #ECO

  years[["eco"]] = 2000:2021

  #IDR

  years[["idr"]] = 2016:2020

  #SOC
  years[["soc"]] = 2015

  #ART
  years[["art"]] = 2015

  return(years[[indicator]][years[[indicator]] >= 2000])
}


get_forecasted_fpt = function(indicator,year,nace_level = "B"){
  y = year
  tab = ifelse(nace_level == "B","macro_fpt_trd_a38","macro_fpt_trd_a88")
  type = ifelse(nace_level == "B","branch","division")
  NVA = lsnr:::get_forecasted_data(year,type) %>% select(NVA) %>% filter(.[,1] %in% c("TOTAL","00") == F)

  if(nrow(NVA) %in% c(36,86)){
    NVA = c(unlist(NVA),0)
  }

  f_data = lsnstat_macrodata(tab,filters = paste0("year=",y,"&indic=",toupper(indicator),"&aggregate=NVA")) %>% rename_with(tolower) %>% filter(.[,1] %in% c("TOTAL","00") == F) %>%
    mutate(FOOTPRINT = as.numeric(value), NVA = as.numeric(unlist(NVA))) %>%
    mutate(GROSS_IMPACT = FOOTPRINT * NVA,
           UNIT_FOOTPRINT = lsnr:::IndicatorsMetadata$UNIT[lsnr:::IndicatorsMetadata$CODE == toupper(indicator)],
           UNIT_GROSS_IMPACT = NA) %>%
    select(!c(year,value,flag,currency,area,lastupdate,aggregate)) %>%
    rename_all(toupper) %>%
    arrange(.[,1])

  return(f_data)
}
