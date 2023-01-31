#' @keywords internal
"_PACKAGE"

## usethis namespace: start
#' @importFrom dplyr %>%
#' @importFrom dplyr select
#' @importFrom eurostat get_eurostat
#' @importFrom insee get_insee_dataset
#' @importFrom stringr str_remove
#' @importFrom tidyr pivot_wider
#' @importFrom utils read.csv
## usethis namespace: end
<<<<<<< HEAD
NULL
=======

setwd("C:/Users/sylva/Documents/La Société Nouvelle/lsnr")
wd = getwd()
print(wd)

#list.files()

source("R/DataBuilder.R")
source("R/BuildBranchesData.R")

print("start")
#test = buildBranchesData("art", 2015)
tryCatch({
  test = suppressWarnings(buildBranchesData("eco","2018"))
  print(test)
  print(filter(test, AGGREGATE=="TRESS"), n=38)
}, warning = function(war) {
  print(war$message)
}, error = function(err) {
  print(err$message)
})


# abs(difftime(file.info(path)$ctime,Sys.time(),units="days"))>7
>>>>>>> 861152fe4597143143ebda2ccc7843a80d4f6b35
