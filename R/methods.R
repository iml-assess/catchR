##' Print armatrix object
##' @param  x output from fit.armatrix
##' @details ...
##' @method print armatrix
##' @export
print.armatrix<-function(x){
    cat("armatrix model: log likelihood is", logLik.armatrix(x),"Convergence", ifelse(0==x$opt$convergence, "OK\n", "failed\n"))
}

##' Log likelihood of armatrix object
##' @param  x output from fit.armatrix
##' @details ...
##' @method logLik armatrix
##' @export
logLik.armatrix<-function(x){
    ret<- -x$opt$objective
    attr(ret,"df")<-length(x$opt$par)
    class(ret)<-"logLik"
    ret
}
