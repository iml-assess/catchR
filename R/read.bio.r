##' Reads biological data 
##' @param file .dat file to read. This file is the output of this SAS program: S:/SAS/Peche/autoexec_peche.sas.
##' @param year Vector of years to read (ex: 2015:2020). NULL by default, meaning the function will keep data from all available years.
##' @param species Either "cod" or "mackerel".
##' @param language Language to use for column names, either "en" (default) or "fr".
##' @param ... Arguments used with read_fwf() (e.g., progress, skip_empty_rows, n_max).
##' @details 
#' Read biological data from dockside monitoring. 
#' Currently only mackerel and Atlantic cod are supported (To add a species: fill in data_raw/bio_key.csv)
##' @import lubridate readr
##' @export
read.bio <- function(file, year = NULL, species, language = "en", ...) {

    species <- match.arg(species, choices = c('cod','mackerel'))
    language <- match.arg(language, choices = c("en", "fr"))

    # read in data
    key <- bio_key[bio_key$species == species, ] # different keys depending on the species 
    id  <- setNames(split(key[, c('start', 'end')], seq(nrow(key))), key[, language]) # transform key into named list
    bio <- read_fwf(file, col_positions = do.call(fwf_cols, id), col_types = cols(.default = "c"), ...)  
    bio <- as.data.frame(bio)
    bio <- type.convert(bio)
    attr(x = bio, which = "spec") <- NULL
    
    if(!is.null(year)){
        if (language == "en"){
            bio <- bio[bio$year %in% year, ]
        } else {
            bio <- bio[bio$annee %in% year, ]
        }
    }

    # add date stuff
    temp <- with(key, c(which(en == 'year'), which(en == 'month'), which(en == 'day')))
    bio$date <- ymd(apply(bio[,temp], 1, paste, collapse = "-"))  
    bio$trim <- quarter(bio$date) 
    bio$doy  <- as.integer(format(bio$date, "%j"))

    # add extra colums
    if('engin' %in% key$fr) bio <- merge(bio, bio_gear[, c(2:3)], all.x = TRUE, by.x = key[key$fr == 'engin', language], by.y = 'gear.cat') # gears
    if('zone' %in% key$fr) bio <- merge(bio, bio_zones, all.x = TRUE) # zones for mackerel
    if('no_prov' %in% key$fr){ # provinces for cod
        bio <- merge(bio, lf_prov[, c("no_prov", language)], all.x = TRUE, by.x = key[key$fr == 'no_prov', language], by.y = 'no_prov') 
        bio[, paste0("prov_", language)] <- bio[, ncol(bio)]
        bio[, ncol(bio) - 1] <- NULL
    }

    # maturity codes
    if(species == 'mackerel'){
        bio <- merge(bio, bio_matur, all.x = TRUE, by.x = key[key$fr == 'stad_mat', language], by.y = 'stad_mat')
        bio$matur_class <- factor(bio$matur_class, levels = unique(bio_matur$matur_class))
        bio$gsi <- bio[, which(names(bio) %in% c('pdgon', 'wgonad'))]/bio[, which(names(bio) %in% c('pdind', 'wgonad'))] * 100
    }
    
    return(bio)
}
