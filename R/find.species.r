##' find species (the code or name used in a certain database)
##' @param species species name (character string)
##' @param base database name
##' @details 
#' Given a certain species name (english), find corresponding codes or names in the given database.Makes use of grep.
#' 
#' If species is set to '' than all available species in the database are given.
#' 
#' Databases included:
#'  \enumerate{
#'    \item ziff
#'    \item{nafoA}
#'    \item{nafoB}
#' }
##' @rdname find.species
##' @export
find.species <-function(species,base=c('ziff','nafoA','nafoB')){

    if(missing(species))stop('provide species name')
    base <- match.arg(base)
    
    sel <- base_species[base_species$base==base & grepl(species,base_species$species,ignore.case = TRUE),] # select database  + find species
    sel <- type.convert(sel,as.is=T)                                                                       # to numeric if code needs to be returned
    
    ret <- sel$use
    if(is.numeric(ret)) names(ret) <-sel$species
    
    return(ret)
}
