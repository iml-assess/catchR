##' Read NAFO 21A database data stored in a .csv file
##' @param file .csv file name to read.
##' @param year Vector of years to read (ex: 2015:2020, c(2001, 2003:2018)). By default, the function will keep all years found in the file.
##' @param species species name(s). Ex: "ATLANTIC COD - COD" for Atlantic cod.
##' @details 
#' Read .csv file downloaded from from https://www.nafo.int/Data/STATLANT. Data is cleaned so that it is conform with other DFO data (ziff, carbio, length-frequencies).
#' 
#' See find.species to get the species names used in the database.
##' @rdname read.nafoA
##' @export
read.nafoA <- function(file, year = NULL, species = NULL){
    if(missing(file)) stop('The function needs a .csv file to read.')
    
    nafo <- read.csv(file, sep = ',')
    names(nafo) <- tolower(names(nafo))
    names(nafo)[c(1,3,5)] <- c('year', 'nafo', 'catch')
    nafo[, 2:4] <- apply(nafo[, 2:4], 2, as.character)
    
    if(!is.null(year)) nafo <- nafo[nafo$year %in% year, ]
    if(!is.null(species)) nafo <- nafo[tolower(nafo$species.name) %in% tolower(species), ]
    
    return(nafo)
}