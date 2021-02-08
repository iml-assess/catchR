##' Get catch-at-age results
##' @param x Output from function get.samples().
##' @param plus Value of plus group (numeric). 
##' @details 
#'  This function calculates catch-at-age number (caan), catch-at-age weight (caaw), mean weight-at-age (waa) and mean length-at-age (laa).
#'  So-called scores were added as an exploratory way to gauge the quality of the estimates each years. 
##' @import dplyr tidyr 
##' @rdname get.caa
##' @export
get.caa <- function(x, plus = Inf){
    
    id.age <- grep('age\\.', colnames(x))
    bound <- function(x,y){x<- as.numeric(x);x[x>y]<-y;return(x)}

    # calculations
    caa <- x %>%
           pivot_longer(cols = all_of(id.age), names_to = "age", names_prefix = "age.", values_to = "age.prop") %>% # transform to long format
           mutate(age = bound(age, plus), # make sure age is numeric and smaller than the plus group
                  wt = catch * age.prop * lf.prop) %>%  # weight of fish of that length-age (caaw)
           group_by(year,age) %>% 
           summarise(caan = sum(wt/weight.unit),                 # caan according to Gavaris (BUT with weight by length)
                     caaw = sum(wt),                              # caaw according to Gavaris (BUT with weight by length)
                     laa = weighted.mean(length, wt/weight.unit), # weighted by their number at length and age in the catch
                     waa = caaw / caan) %>%  
           as.data.frame()
    return(caa)
}
