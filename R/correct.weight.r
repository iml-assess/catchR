##' species specific correction of weight in function of fish state (round vs headless,...)
##' @param x value of weight
##' @param id state.id (numeric or string; french or english, e.g. Round / Rond)
##' @param species species
##' @details conversion of weights
##' @rdname correct.weight
##' @export
correct.weight <- function(x,id,species=c('cod','mackerel')){
    if(missing(x)) stop('weights?')
    if(missing(id)) stop('states?')
    species <- tolower(species)
    species <- match.arg(species)
    
    if(is.numeric(id)){
        s <- setdiff(na.omit(unique(id)),lf_fishshape[,1])
        if(length(s)>0) stop('invalid id(s)')
        id <- lf_fishshape[match(id,lf_fishshape[,1]),'en']
    }
    en <- any(unique(id) %in% lf_fishshape$en)
    if(!en) lf_fishshape[match(id,lf_fishshape[,3]),'en']

    conv <- lf_fishshape[match(id,lf_fishshape[,2]),grep(species,names(lf_fishshape))]
    x*conv
}

