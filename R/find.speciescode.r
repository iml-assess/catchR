##' find species codes
##' @param species species name (character string)
##' @param language en or fr (english by default)
##' @details find species codes
##' @rdname find.speciescode
##' @export
find.speciescode <-function(species,language=c('en','fr')){
    language <- match.arg(language)
    if(missing(species))stop('provide species name')
    species <- tolower(species)
    
    avail <- tolower(ziff_species[,paste0('esp_nom_',language)])
    id <- grep(species,avail)
    return(ziff_species[id,])
}
