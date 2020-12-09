##' catch-at-age raw: return length frequencies and raw age-length keys
##' @param x output from caa.all
##' @param plus plus group age
##' @details calculates caa from the samples found with caa.raw
##' @importFrom reshape2 melt
##' @importFrom plyr ddply
##' @rdname caa.clean
##' @export
caa.clean <- function(x,plus=NULL){
    
    id.age <- grep('X',names(x))
    
    # plus group
    if(!is.null(plus)){
        ages <- as.numeric(gsub('X','',names(x)[id.age]))
        too.old <- ages>plus
        x[,max(id.age[!too.old])] <- rowSums(x[,id.age[too.old]])
        x[,id.age[too.old]] <- NULL
        id.age <- id.age[!too.old]
    }
    
    # calculations
    caa <- melt(x,id=names(x)[-id.age],variable.name='age',value.name='age.prop')
    caa$age <- as.numeric(gsub('X','',caa$age))
    caa$caan <- with(caa,catch*age.prop*n.lf/(weight.sample.tot/10))
    caa$caaw <- with(caa,catch*age.prop*weight.sample/weight.sample.tot)
    caa <- ddply(caa,c('year','age'),function(y){
        wt=with(y,caaw/sum(caaw))
        waa=weighted.mean(y$caaw/y$caan,wt)
        waa.var=sum(wt * (y$weight.unit/10 - waa)^2)
        waa.sd=sqrt(waa.var)
        caan=sum(y$caan)
        caaw=sum(y$caaw)
        c(caan=caan,caaw=caaw,waa=waa,waa.var=waa.var,waa.sd=waa.sd)})
    return(caa)
}









