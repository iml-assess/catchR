##' Standardized proportions at year
##' @param x matrix to be standardized (years in columns, ages in rows)
##' @rdname spay
##' @details Noel's approach
##' @export
spay <- function(x){
    sx <- apply(x,2,sum,na.rm=T)
    ny <- length(sx)
    na <- nrow(x)
    msx <- matrix(sx,nrow=na,ncol=ny,byrow=T)
    px <- x/msx
    px.dev <- px - matrix(apply(px,1,mean,na.rm=T),nrow=na,ncol=ny,byrow=F)
    px.std <- sqrt(apply(px.dev^2,1,mean,na.rm=T))
    zx <- px.dev/matrix(px.std,nrow=na,ncol=ny,byrow=F)
    return(zx)
}