##' Standardized proportions at age
##' @param x matrix to be standardized (years in columns, ages in rows)
##' @rdname spya
##' @details Noel's approach
##' @export
spya <- function(x){
    sx <- apply(x,1,sum,na.rm=T)
    na <- length(sx)
    ny <- ncol(x)
    msx <- matrix(sx,nrow=na,ncol=ny,byrow=F)
    px <- x/msx
    px.dev <- px - matrix(apply(px,1,mean,na.rm=T),nrow=na,ncol=ny,byrow=T)
    px.std <- sqrt(apply(px.dev^2,1,mean,na.rm=T))
    zx <- px.dev/matrix(px.std,nrow=na,ncol=ny,byrow=F)
    return(zx)
}