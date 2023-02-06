#' @noRd

# Données exploitées
# -> Les données étant relatives à la valeur ajoutée nette des branches/divisions françaises, le taux de contribution est par définition de 100 %
# -> De même, les immportations sont par définition nullement contributrices à l'économie francçaise (i.e. coef = 0)

build_branches_nva_fpt_eco = function(selectedYear)
{
  # get branches aggregates -------------------------- #

  branches_aggregates = get_branches_aggregates(selectedYear)

  # build nva fpt dataframe -------------------------- #

  nva_fpt_data = as.data.frame(cbind(branches_aggregates$BRANCH, branches_aggregates$NVA))
  colnames(nva_fpt_data) = c("BRANCH", "NVA")

  for(i in 1:nrow(nva_fpt_data))
  {
    # build values
    nva_fpt_data$GROSS_IMPACT[i] = branches_aggregates$NVA[i]
    nva_fpt_data$UNIT_GROSS_IMPACT[i] = "CPMEUR"
    nva_fpt_data$FOOTPRINT[i] = 100.0
    nva_fpt_data$UNIT_FOOTPRINT[i] = "P100"
  }

  return(nva_fpt_data)
  # -------------------------------------------------- #
}

build_divisions_nva_fpt_eco = function(selectedYear)
{
  # get divisions aggregates -------------------------- #

  divisions_aggregates = get_divisions_aggregates(selectedYear)

  # build nva fpt dataframe -------------------------- #

  nva_fpt_data = data.frame(DIVISION = divisions_aggregates$CNA_ACTIVITE, NVA = divisions_aggregates$NVA)

  for(i in 1:nrow(nva_fpt_data))
  {
    # build values
    nva_fpt_data$GROSS_IMPACT[i] = divisions_aggregates$NVA[i]
    nva_fpt_data$UNIT_GROSS_IMPACT[i] = "CPMEUR"
    nva_fpt_data$FOOTPRINT[i] = 100.0
    nva_fpt_data$UNIT_FOOTPRINT[i] = "P100"
  }

  return(nva_fpt_data)
  # -------------------------------------------------- #
}

get_branches_imp_coef_eco = function(selectedYear)
{
  branches_imp_coef = 0
  return(branches_imp_coef)
}
