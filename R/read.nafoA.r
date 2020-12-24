##' read nafoA csv file
##' @param file file name
##' @param year vector of years to read (all by default)
##' @param species species name(s)
##' @details 
#' Read csv file downloaded from from https://www.nafo.int/Data/STATLANT. Data is cleaned so that it is conform with other DFO data (ziff, carbio, length-frequencies)
#' 
#' See find.species to get the species names used in the database
##' @rdname read.nafoA
##' @export
read.nafoA <- function(file,year=NULL,species=NULL){
    if(missing(file))stop('file?')
    
    nafo <- read.csv(file,sep=',')
    names(nafo) <- tolower(names(nafo))
    names(nafo)[c(1,3,5)] <-c('year','nafo','catch')
    nafo[,2:4] <- apply(nafo[,2:4],2,as.character)
    
    if(!is.null(year)) nafo <- nafo[nafo$year %in% year,]
    if(!is.null(species)) nafo <- nafo[tolower(nafo$species.name) %in% tolower(species),]
    
    return(nafo)
}