## All read functions require additional data, which is stored in raw csv files; 
# - key: positions of fixed width columns
# - other: transform codes into their values (e.g. species code to species name, engin code to engin name)

# load all raw data
raw  <- list.files('data-raw' , pattern = 'csv', full.names = T)
rawn <- sub('\\.csv$', '',list.files('data-raw' , pattern = 'csv'))

rawcsv <- lapply(raw, read.csv)
names(rawcsv) <- rawn
list2env(rawcsv,envir=.GlobalEnv)

# basic transformations
ziff_meta_csv[] <- lapply(ziff_meta_csv, as.character)
ziff_species <- ziff_species[,c(1:4)]
bio_gear <- bio_gear[,c(1:3)]

# save for use in package
usethis::use_data(ziff_meta_csv, 
                  ziff_meta_dat, 
                  ziff_species, 
                  ziff_gear, 
                  ziff_region,
                  ziff_tonnage,
                  lf_key_freq,
                  lf_key_set,
                  lf_subsample,
                  lf_lengthtype,
                  lf_bins,
                  lf_fishshape,
                  lf_prov,
                  bio_gear,
                  bio_key,
                  bio_matur,
                  bio_zones,
                  internal = TRUE, overwrite = TRUE)


