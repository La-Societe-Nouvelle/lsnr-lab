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

setwd("C:/Users/sylva/Documents/La Société Nouvelle/lsnr")
wd = getwd()
print(wd)

#list.files()

#source("R/BuildARTData.R")
source("R/BuildBranchesData.R")


print("start")
test = buildBranchesData("art", 2015)
print(test)


# abs(difftime(file.info(path)$ctime,Sys.time(),units="days"))>7