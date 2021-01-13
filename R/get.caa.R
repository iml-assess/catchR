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
    
     # calculations
    ret <- melt(x, id = names(x)[-id.age], variable.name = 'age', value.name = 'age.prop')
    ret$age <- as.numeric(gsub('age.', '', ret$age))
    ret$wt <- with(ret,catch/weight.unit*age.prop*prop) # caan according to Gavaris
    
    ret <- ret %>%
        group_by(year,age) %>% 
        summarise(caanp = sum(catch * age.prop * prop / weight.unit),
                  caawp = sum(catch * age.prop * prop),
                  laap = weighted.mean(length*prop,  wt)) %>%  
        as.data.frame()
    
    ret[ret$age>plus,'age'] <- plus
    ret <- ret %>%
        group_by(year,age) %>% 
        summarise(caan = sum(caanp),
                  caaw = sum(caawp),
                  waa = caaw/caan,
                  laa = weighted.mean(laap,  caanp)) %>%  
        as.data.frame()
    
    return(ret)
}









