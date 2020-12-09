##' Roudn to the nearest x cam
##' @param x vector
##' @param cm to which to round
##' @rdname roundFish
##' @export
roundFish <- function(x,cm){
    if(missing(cm))stop('cm?')
    cm*round(x/cm) 
}
