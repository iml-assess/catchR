##' Get catch-at-age results
##' @param x Output from function get.samples().
##' @param plus Whether an age group + is desired (numeric). NULL by default.
##' @details 
#'  This function calculates catch-at-age number (caan), catch-at-age weight (caaw), mean weight-at-age (waa) and mean length-at-age (laa).
#'  So-called scores were added as an exploratory way to gauge the quality of the estimates each years.  
##' @importFrom reshape2 melt
##' @rdname get.caa
##' @export
get.caa <- function(x, plus = NULL){
    
    id.age <- grep('age\\.', colnames(x))
    
    # plus group
    if(!is.null(plus)){
        ages <- as.numeric(gsub('age.', '', names(x)[id.age]))
        too.old <- ages > plus
        x[, max(id.age[!too.old])] <- rowSums(x[, id.age[too.old]])
        x[, id.age[too.old]] <- NULL
        id.age <- id.age[!too.old]
    }

     # calculations
    caa <- melt(x, id = names(x)[-id.age], variable.name = 'age', value.name = 'age.prop')
    caa$age <- as.numeric(gsub('age.', '', caa$age))

    caa <- do.call("rbind", as.list(
        by(caa, list(caa[, 'year'], caa[, 'age']), function(y){
            wt_w = with(y, catch / weight.unit.mean)  # number of fish landed
            waa = weighted.mean(y$weight.unit, wt_w)
            waa.var = sum(wt_w * (y$weight.unit - waa)^2)
            laa = weighted.mean(y$length*y$prop, wt_w)
            laa.var = sum(wt_w * (y$length*y$prop - laa)^2)
            data.frame(year = y$year[1],
                       age = y$age[1],
                       caan = sum(with(y, catch * age.prop * prop / waa)),
                       caaw = sum(with(y, catch * age.prop * prop)),
                       waa = waa,
                       waa.var = waa.var,
                       waa.sd = sqrt(waa.var),
                       laa = laa,
                       laa.var = laa.var,
                       laa.sd = sqrt(laa.var),
                       score.lf.option = round(weighted.mean(y$option.lengthfreq, y$catch), 2),
                       score.al.option = round(weighted.mean(y$option.agelength, y$catch), 2),
                       score.lf.nsample = round(weighted.mean(y$nsample.lengthfreq, y$catch), 2),
                       score.al.nsample = round(weighted.mean(y$nsample.agelength, y$catch), 2))
        })))
    
    return(caa)}









