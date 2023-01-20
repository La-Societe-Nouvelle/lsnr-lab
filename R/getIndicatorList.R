#' Get non-financial data list and main informations.
#'
#' @details Get non-financial data list and main informations : full label, code, unit, data sources and used rounding.
#'
#' @return A table of all 12 supplied non-financial dimensions.
#'
#' @seealso \code{\link{buildDivisionsData}}, \code{\link{buildBranchesData}}, \code{\link{buildDiscountedData}}.
#'
#'
#' @examples
#' getIndicatorList()
#'
#' @export

getIndicatorList = function(){

  indicator_list = t(matrix(data = c(
    "ART", "Contribution to crafts and skills", "Contribution aux Métiers d'Art et aux Savoir-Faire", "percentage", "%", "P100","Insee and DGE",1,
    "DIS", "Remuneration distribution index", "Indice de répartition des rémunérations", "percentage", "%", "P100","Insee and Eurostat",1,
    "ECO", "Contribution to the national economy", "Contribution à l'économie nationale", "percentage", "%", "P100","Insee",1,
    "GEQ", "Gender income inequality index", "Indice d'écart de rémunérations F/H", "percentage", "%", "P100","Insee",1,
    "GHG", "Greenhouse gases emissions intensity", "Intensité d'Émissions de Gaz à effet de serre", "gram of carbon dioxide equivalent by current euro", "gCO2 / €", "GCO2_CPEUR","Insee, Eurostat and the World Bank",1,
    "HAZ", "Hazardous products use intensity", "Intensité d'Utilisation de produits dangereux", "gram per euro", "g / €", "G_CPEUR","Insee and Eurostat",1,
    "IDR", "Interdecile ratio", "Rapport interdécile d9/d1", "Index", NA, "IND", "Insee", 2,
    "KNW", "Contribution to the skills and knowledge development", "Contribution à l'Evolution des compétences et des connaissances", "percentage", "%", "P100","Insee and Eurostat",1,
    "MAT", "Raw material extraction intensity", "Intensité d'Extraction de Matières premières", "gram per euro", "g / €", "G_CPEUR","Insee and Eurostat",0,
    "NRG", "Energy consumption intensity", "Intensité de Consommation d'Energie", "kilojoule by current euro", "kJ / €", "KJ_CPEUR","Insee and Eurostat",0,
    "SOC", "Contribution to actors of social interest", "Contribution aux Acteurs d'Intérêt social", "percentage", "%", "P100","Insee and 2020 commented Atlas of Social Solidarity Economy",1,
    "WAS", "Waste generation intensity", "Intensité de Production de Déchets", "gram per euro", "g / €", "G_CPEUR","Insee and Eurostat",0,
    "WAT", "Water consumption intensity", "Intensité de Consommation d'Eau", "liter per euro", "L / €", "L_CPEUR", "Insee - Eurostat - SDES (French Ecological Transition Ministry)", 1),
    ncol = 13
  ))

  colnames(indicator_list) = c("Indicator code", "English label", "French label", "Unit label", "Unit symbol", "Unit code", "Data source(s)","Used rounding")

  return(indicator_list)
}