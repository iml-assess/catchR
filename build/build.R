# functions to help build the package when developing

## 1) remove prior package installation (safety check)
# remove.packages('catchR') 

## 2) add new raw data (used in functions but not exported, generally for reading in stuff)
# file.remove('R/sysdata.rda')  # remove prior data
# source('data-raw/DATASET.R')  # generate new data

## 3) exclude directories from package (not necessary for package to work)
# usethis::use_build_ignore(c('example', 'build','data-raw'))

## 4) build
# roxygen2::roxygenise()  # update help (man directory)
# devtools::build()       # build package
# devtools::install()     # install package


#roxygenise(".", clean = TRUE)
devtools::load_all(".")
document(".")
build(pkg = ".", manual = TRUE)
check(".", manual = TRUE )
install("../temperature", reload = TRUE)

##### Other helpful stuff #####
# Faster load (no full installation)
# devtools::load_all() 

# preview help
# rstudioapi::previewRd('man/fit.armatrix.Rd')
# rstudioapi::previewRd('man/find.species.Rd')

# for use of cpps (TMB) see https://rtbecard.gitlab.io/2018/02/11/Distributing-TMB-in-R-packages.html
