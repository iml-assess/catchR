##' Identify outliers based on the interquantile range
##' @param x vector
##' @param coef 
##' @details Identifies outliers based on common rule of coef * the interquantile range. The default coefficient is set to 1.5. A coefficient of three is recommended for extremes.  Returns a vector of true or false.
is.outlier <- function(x, coef=1.5, na.rm = TRUE, ...) {
    qnt <- quantile(x, probs=c(.25, .75), na.rm = na.rm, ...)
    H <- coef * IQR(x, na.rm = na.rm)
    x < (qnt[1] - H) |x > (qnt[2] + H)
}
