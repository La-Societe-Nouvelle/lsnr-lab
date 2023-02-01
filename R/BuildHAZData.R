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

  print(prodcom_data);
  print(reversed_ic_matrix);

  # sector fpt --------------------------------------- #

  # sector_fpt_list = list()

  # sector_fpt_list[["TOTAL"]] = env_chmhaz_data$values*1000000 / nama_data$values

  # sector_fpt = cbind.data.frame(sector_fpt_list) %>% pivot_longer(cols = names(sector_fpt_list))
  # colnames(sector_fpt) = c("SECTOR", "FOOTPRINT")
  # print(sector_fpt)

  # build nva fpt dataframe -------------------------- #

  nva_fpt_data = as.data.frame(cbind(branches_aggregates$BRANCH, branches_aggregates$NVA))
  colnames(nva_fpt_data) = c("BRANCH", "NVA")

  branch_sector_fpt_matrix = lsnr:::MatrixHAZ

  haz_dmc_qnt = prodcom_data$value[prodcom_data$aggregate=="PRODQNT"] + prodcom_data$value[prodcom_data$aggregate=="IMPQNT"] - prodcom_data$value[prodcom_data$aggregate=="EXPQNT"]
  print(haz_dmc_qnt)

  for(i in 1:nrow(nva_fpt_data))
  {
    # get sector
    branch = nva_fpt_data$BRANCH[i]
    # sector = branch_sector_fpt_matrix$SECTOR[branch_sector_fpt_matrix$BRANCH==branch]

    # build values
    nva_fpt_data$GROSS_IMPACT[i] = haz_dmc_qnt * reversed_ic_matrix$VALUE[reversed_ic_matrix$BRANCH==branch]
    nva_fpt_data$UNIT_IMPACT[i] = "T"
    nva_fpt_data$FOOTPRINT[i] = (haz_dmc_qnt * reversed_ic_matrix$VALUE[reversed_ic_matrix$BRANCH==branch]) / branches_aggregates$NVA[branches_aggregates$BRANCH==branch]
    nva_fpt_data$UNIT_FOOTPRINT[i] = "G_CPEUR"

    # build values
    # nva_fpt_data$GROSS_IMPACT[i] = sector_fpt$FOOTPRINT[sector_fpt$SECTOR==sector] * branches_aggregates$NVA[i]
    # nva_fpt_data$FOOTPRINT[i] = sector_fpt$FOOTPRINT[sector_fpt$SECTOR==sector]
    # nva_fpt_data$UNIT_FOOTPRINT[i] = "G_CPEUR"
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

  nva_fpt_data = as.data.frame(cbind(divisions_aggregates$DIVISION, divisions_aggregates$NVA))
  colnames(nva_fpt_data) = c("DIVISION", "NVA")

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
    mutate(area = "FRA")

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
    filter(year == selectedYear) # control if empty

  # domestic production
  eurostat_nama_data = get_eurostat(
    "nama_10_a64",
    filters = list(geo=c("FR","EU27_2020"), na_item="B1G", time=year, unit="CP_MEUR", nace_r2="TOTAL")
  )

  print(prodcom_data)
  print(eurostat_nama_data)

  fpt_fra =  wdi_data$EN.ATM.CO2E.KD.GD[wdi_data$iso2c=="FR"]
  fpt_wld =  wdi_data$EN.ATM.CO2E.KD.GD[wdi_data$iso2c=="1W"]

  branches_imp_coef = fpt_wld / fpt_fra

  return(branches_imp_coef)
}
