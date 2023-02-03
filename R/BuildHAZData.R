#'Build and returns all data required to the HAZ indicator computations.
#'
#'Returns a `list` made up of value added impacts by French branches, imported products associated coefficient,
#'Data sources and values unit. This data will be used in both BuildBranchesData and BuildDivisionsData functions.
#'
#' @param Year Considered Year.
#'
#' @return An object `list` made up of 4 elements : value added impacts by French branches,
#' imported products associated coefficient, data sources and values unit.
#' @seealso \code{\link{BuildECOData}}, \code{\link{BuildGHGData}},
#'  \code{\link{BuildBranchesData}}, \code{\link{BuildDivisionsData}}, \code{\link{FetchDataAvailability}}.
#' @examples
#' BuildHAZData(max(FetchDataAvailability("HAZ"))
#' @noRd

build_branches_nva_fpt_haz = function(selectedYear)
{
  # get branches aggregates -------------------------- #

  branches_aggregates = get_branches_aggregates(selectedYear)

  # fetch data --------------------------------------- #

  # fetch data --------------------------------------- #

  tryCatch({
    # prodcom data
    res_prodqnt = GET("https://api.lasocietenouvelle.org/serie/MACRO_HAZARDOUSPRODUCTS_PRODQNT_PRODCOM_FRA_T")
    res_impqnt = GET("https://api.lasocietenouvelle.org/serie/MACRO_HAZARDOUSPRODUCTS_IMPQNT_PRODCOM_FRA_T")
    res_expqnt = GET("https://api.lasocietenouvelle.org/serie/MACRO_HAZARDOUSPRODUCTS_EXPQNT_PRODCOM_FRA_T")

    data_prodqnt = fromJSON(rawToChar(res_prodqnt$content))$data %>%
      mutate(aggregate = "PRODQNT")
    data_impqnt = fromJSON(rawToChar(res_impqnt$content))$data %>%
      mutate("aggregate" = "IMPQNT")
    data_expqnt = fromJSON(rawToChar(res_expqnt$content))$data %>%
      mutate("aggregate" = "EXPQNT")

    prodcom_data = data_prodqnt %>%
      rbind(data_impqnt) %>%
      rbind(data_expqnt) %>%
      filter(year == selectedYear) # control if empty

    # tei data (reuse insee data set -> coef tech not usable)
    reversed_ic_matrix = suppressMessages(get_reversed_ic_matrix(selectedYear)) %>%
      filter(PRODUCT == "CE") %>%
      pivot_longer(!PRODUCT, names_to = "BRANCH", values_to = "VALUE")

  }, error = function(e) {
    print(e)
    stop(paste0("Données indisponibles pour ",selectedYear))
  })

  haz_dmc_qnt = prodcom_data$value[prodcom_data$aggregate=="PRODQNT"] + prodcom_data$value[prodcom_data$aggregate=="IMPQNT"] - prodcom_data$value[prodcom_data$aggregate=="EXPQNT"]
  print(haz_dmc_qnt)

  # build nva fpt dataframe -------------------------- #

  nva_fpt_data = as.data.frame(cbind(branches_aggregates$BRANCH, branches_aggregates$NVA))
  colnames(nva_fpt_data) = c("BRANCH", "NVA")

  for(i in 1:nrow(nva_fpt_data))
  {
    # get sector
    branch = nva_fpt_data$BRANCH[i]

    # build values
    nva_fpt_data$GROSS_IMPACT[i] = haz_dmc_qnt * reversed_ic_matrix$VALUE[reversed_ic_matrix$BRANCH==branch]
    nva_fpt_data$UNIT_IMPACT[i] = "T"
    nva_fpt_data$FOOTPRINT[i] = (haz_dmc_qnt * reversed_ic_matrix$VALUE[reversed_ic_matrix$BRANCH==branch]) / branches_aggregates$NVA[branches_aggregates$BRANCH==branch]
    nva_fpt_data$UNIT_FOOTPRINT[i] = "G_CPEUR"
  }

  # temp correction
  nva_fpt_data$FOOTPRINT[37] = 0

  return(nva_fpt_data)
  # -------------------------------------------------- #
}

build_divisions_nva_fpt_haz = function(selectedYear)
{
  # get branches aggregates -------------------------- #

  divisions_aggregates = get_divisions_aggregates(selectedYear)

  # sector fpt --------------------------------------- #

  sector_fpt = build_branches_nva_fpt_haz(selectedYear)

  # build nva fpt dataframe -------------------------- #

  nva_fpt_data = data.frame(DIVISION = as.character(divisions_aggregates$CNA_ACTIVITE), NVA = as.numeric(divisions_aggregates$NVA))

  divisions = lsnr:::Divisions

  for(i in 1:nrow(nva_fpt_data))
  {
    # get division
    division = nva_fpt_data$DIVISION[i]
    branch = divisions$BRANCH[divisions$DIVISION==division]

    # build values
    nva_fpt_data$GROSS_IMPACT[i] = sector_fpt$FOOTPRINT[sector_fpt$BRANCH==branch] * divisions_aggregates$NVA[divisions_aggregates$DIVISION==division]
    nva_fpt_data$UNIT_IMPACT[i] = "T"
    nva_fpt_data$FOOTPRINT[i] = sector_fpt$FOOTPRINT[sector_fpt$BRANCH==branch]
    nva_fpt_data$UNIT_FOOTPRINT[i] = "G_CPEUR"
  }

  return(nva_fpt_data)
  # -------------------------------------------------- #
}

get_branches_imp_coef_haz = function(selectedYear)
{
  # fetch data

  # prodcom data - FRA
  res_prodqnt = GET("https://api.lasocietenouvelle.org/serie/MACRO_HAZARDOUSPRODUCTS_PRODQNT_PRODCOM_FRA_T")
  res_impqnt = GET("https://api.lasocietenouvelle.org/serie/MACRO_HAZARDOUSPRODUCTS_IMPQNT_PRODCOM_FRA_T")
  res_expqnt = GET("https://api.lasocietenouvelle.org/serie/MACRO_HAZARDOUSPRODUCTS_EXPQNT_PRODCOM_FRA_T")

  data_prodqnt_fra = fromJSON(rawToChar(res_prodqnt$content))$data %>%
    mutate(aggregate = "PRODQNT") %>%
    mutate(area = "FRA")

  data_impqnt_fra = fromJSON(rawToChar(res_impqnt$content))$data %>%
    mutate("aggregate" = "IMPQNT") %>%
    mutate(area = "FRA")

  data_expqnt_fra = fromJSON(rawToChar(res_expqnt$content))$data %>%
    mutate("aggregate" = "EXPQNT") %>%
    mutate(area = "FRA") %>%
    filter(year == selectedYear)

  haz_dmc_qnt_fra = data_prodqnt_fra$value + data_impqnt_fra$value - data_expqnt_fra$value

  # prodcom data - EUU
  res_prodqnt = GET("https://api.lasocietenouvelle.org/serie/MACRO_HAZARDOUSPRODUCTS_PRODQNT_PRODCOM_EUU_T")
  res_impqnt = GET("https://api.lasocietenouvelle.org/serie/MACRO_HAZARDOUSPRODUCTS_IMPQNT_PRODCOM_EUU_T")
  res_expqnt = GET("https://api.lasocietenouvelle.org/serie/MACRO_HAZARDOUSPRODUCTS_EXPQNT_PRODCOM_EUU_T")

  data_prodqnt_euu = fromJSON(rawToChar(res_prodqnt$content))$data %>%
    mutate(aggregate = "PRODQNT") %>%
    mutate(area = "EUU")

  data_impqnt_euu = fromJSON(rawToChar(res_impqnt$content))$data %>%
    mutate("aggregate" = "IMPQNT")%>%
    mutate(area = "EUU")

  data_expqnt_euu = fromJSON(rawToChar(res_expqnt$content))$data %>%
    mutate("aggregate" = "EXPQNT")%>%
    mutate(area = "EUU")

  prodcom_data = data_prodqnt_fra %>%
    rbind(data_impqnt_fra) %>%
    rbind(data_expqnt_fra) %>%
    rbind(data_prodqnt_euu) %>%
    rbind(data_impqnt_euu) %>%
    rbind(data_expqnt_euu) %>%
    filter(year == selectedYear)  # control if empty

  haz_dmc_qnt_euu = data_prodqnt_euu$value + data_impqnt_euu$value - data_expqnt_euu$value

  # domestic production
  eurostat_nama_data = get_eurostat_data(paste0("https://ec.europa.eu/eurostat/api/dissemination/statistics/1.0/data/nama_10_a64?geo=FR&geo=EU27_2020&unit=CP_MEUR&time=",selectedYear,"&nace_r2=TOTAL&na_item=B1G"))

  fpt_fra =  haz_dmc_qnt_fra / eurostat_nama_data$values[eurostat_nama_data$geo=="FR"]
  fpt_wld =  haz_dmc_qnt_euu / eurostat_nama_data$values[eurostat_nama_data$geo=="EU27_2020"]

  branches_imp_coef = fpt_wld / fpt_fra

  return(branches_imp_coef)
}
