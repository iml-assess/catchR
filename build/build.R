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


##### Other helpful stuff #####
# Faster load (no full installation)
# devtools::load_all() 

# preview help
# rstudioapi::previewRd('man/read.ziff.Rd')
# rstudioapi::previewRd('man/find.species.Rd')
