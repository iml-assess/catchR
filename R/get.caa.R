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
    ret <- melt(x, id = names(x)[-id.age], variable.name = 'age', value.name = 'age.prop')
    ret$age <- as.numeric(gsub('age.', '', ret$age))
    caa$wt <- with(caa,catch/weight.unit.mean*age.prop*prop) # caan according to Gavaris
    
    ret <- caa %>%
        group_by(year,age) %>% 
        summarise(caan = sum(catch * age.prop * prop / weight.unit),
                  caaw = sum(catch * age.prop * prop),
                  laa = weighted.mean(length*prop,  wt)) %>%  
        as.data.frame()
    
    ret[ret$age>plus,'age'] <- plus
    ret <- ret %>%
        group_by(year,age) %>% 
        summarise(caan = sum(caan),
                  caaw = sum(caaw),
                  waa = caaw/caan,
                  laa = weighted.mean(laa,  caan)) %>%  
        as.data.frame()
    
    return(ret)
    }









