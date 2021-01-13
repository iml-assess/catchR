##' Get catch-at-age results
##' @param x Output from function get.samples().
##' @param plus Whether an age group + is desired (numeric). NULL by default.
##' @details 
#'  This function calculates catch-at-age number (caan), catch-at-age weight (caaw), mean weight-at-age (waa) and mean length-at-age (laa).
#'  So-called scores were added as an exploratory way to gauge the quality of the estimates each years.  
##' @rdname get.caa
##' @export
get.caa <- function(x, plus = NULL){
    
    id.age <- grep('age\\.', colnames(x))
    
    # calculations
    caa <- x %>%
           pivot_longer(cols = all_of(id.age), names_to = "age", names_prefix = "age.", values_to = "age.prop") %>% # transform to long format
           mutate(age = as.numeric(age), # make sure age is numeric
                  wt = catch / weight.unit.mean * age.prop * lf.prop) %>%  # number of fish of that length-age
           group_by(year,age) %>% 
           summarise(caan = sum(catch / weight.unit * age.prop * lf.prop), # caan according to Gavaris
                     caaw = sum(catch * age.prop * lf.prop),
                     laa = weighted.mean(length, wt),
                     waa = caaw / caan) %>%  
           as.data.frame()
    
    if (!is.null(plus)){ # if a plus group is provided
        caa[caa$age > plus, 'age'] <- plus 
        
        # re-calculations with the plus group
        caa <- caa %>%
               group_by(year, age) %>% 
               summarise(laa = weighted.mean(laa, caan),
                         caan = sum(caan),
                         caaw = sum(caaw),
                         waa = caaw / caan) %>%  
               as.data.frame()
    }
    return(caa)
}
