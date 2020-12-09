##' Read bio data 
##' @param ... arguments to read_fwf (e.g., file, progress, skip_empty_rows,n_max)
##' @param species see details
##' @param language en or fr (english by default)
##' @details read bio data from Peche SAS program. 
##' currently only mackerel and cod are supported
##' @import lubridate readr
##' @export
read.bio <- function(...,species=c('cod','mackerel'),language=c('en','fr')) {

    species <- match.arg(species)
    language <- match.arg(language)

    # read in data
    key <- bio_key[bio_key$species==species,]
    id  <- setNames(split(key[,c('start','end')], seq(nrow(key))), key[,language])              # transform key into named list
    bio <- read_fwf(..., do.call(fwf_cols,id))   
    bio <- as.data.frame(bio)

    # add date stuff
    temp <- with(key,c(which(en=='year'),which(en=='month'),which(en=='day')))
    bio$date <- ymd(apply(bio[,temp],1,paste,collapse = "-" ))  
    bio$trim <- quarter(bio$date) 
    bio$doy  <- as.integer(format(bio$date, "%j"))

    # add extra colums
    if('engin' %in% key$fr) bio <- merge(bio,bio_gear[,c(2:3)],all.x=TRUE, by.x=key[key$fr=='engin',language], by.y='gear.cat')     # gears
    if('zone' %in% key$fr)  bio <- merge(bio,bio_zones,all.x=TRUE)                                                                  # zones for mackerel
    if('no_prov' %in% key$fr) bio <- merge(bio,lf_prov,all.x=TRUE, by.x=key[key$fr=='no_prov',language], by.y='no_prov')             # provinces for cod

    # maturity codes
    if(species=='mackerel'){
        bio <- merge(bio,bio_matur,all.x=TRUE, by.x=key[key$fr=='stad_mat',language], by.y='stad_mat')
        bio$matur_class <- factor(bio$matur_class,levels = unique(bio_matur$matur_class))
        bio$gsi <- bio[,which(names(bio) %in% c('pdgon','wgonad'))]/bio[,which(names(bio) %in% c('pdind','wgonad'))]*100
    }
    
    return(bio)
}
