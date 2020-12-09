##' read nafoA csv file
##' @param path path to which to download raw files
##' @param year vector of years to read (all by default)
##' @param overwrite download again? see Details
##' @importFrom rvest html_nodes html_attr
##' @importFrom readr read_lines
##' @importFrom tidyr gather
##' @importFrom xml2 read_html
##' @importFrom data.table rbindlist fread
##' @details 
##' Load and clean downloaded csv data from https://www.nafo.int/Data/STATLANT
##' @rdname read.nafoA
##' @export
read.nafoA <- function(file){
    if(missing(file))stop('file?')
    nafo <- read.csv(file,sep=',')
    names(nafo) <- tolower(names(nafo))
    names(nafo)[c(1,3,5)] <-c('year','nafo','catch')
    nafo[,2:4] <- apply(nafo[,2:4],2,as.character)
    return(nafo)
}