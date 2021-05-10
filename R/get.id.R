##' get sample id
##' @param x vector of strings
##' @details Converts a character string to a unique number for use as the sample id. This approach ensure that every id is unique and independent of future data.frame updates (changes, reordering, etc.)
##' @rdname get.id
##' @export
get.id <- function(x){
    sapply(1:length(x),function(i){
        y <- strsplit(x[i],'')[[1]]
        n <- sapply(y,function(x){match(tolower(x),c(letters,0:9,'-','/','.',' ','_'))})
        n <- as.numeric(paste(n,collapse = ''))
    })
}
