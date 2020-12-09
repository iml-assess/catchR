##' Simple cohort strength calculation based on catch-at-age
##' @param x catch-at-age (age x year)
##' @param min min number of observations necessary for average
##' @details calculates catch-at-age by cohort
##' @rdname cohort
##' @examples 
##' caa <-matrix(rnorm(500,0.5,0.2),nrow=10,ncol=50)
##' dimnames(caa) <- list(age=1:10,year=1951:2000)
##' cohort(caa)
##' @export
cohort <- function(x,min=2){
    a <- dimnames(x)
    if(is.null(a[[1]])) rownames(x) <- 1:nrow(x)
    if(is.null(a[[2]])) colnames(x) <- 1:ncol(x)
    
    d <- col(x) - row(x)
    s <- split(x, d)
    id <- sapply(s,function(x) length(x)>=min)
    
    strength <- sapply(s[id],function(x) mean(x))
    ret <- as.data.frame(strength)
    ret$year <-  as.numeric(rownames(ret))+min(as.numeric(colnames(x)))
    return(ret)
}
