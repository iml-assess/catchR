##' Get catch-at-age results
##' @param x Output from function get.samples().
##' @param plus Value of plus group (numeric). 
##' @details 
#'  This function calculates catch-at-age number (caan), catch-at-age weight (caaw), mean weight-at-age (waa) and mean length-at-age (laa).
##' @import dplyr tidyr 
##' @rdname get.caa
##' @export
get.caa <- function(x, plus = Inf){
    
    id.age <- grep('age\\.', colnames(x))
    bound <- function(x,y){x<- as.numeric(x);x[x>y]<-y;return(x)}
    weighted.sd <- function(mu,x,w){if(sum(w)>1)sqrt(sum(w*(x-mu)^2)/(sum(w)-1))else NA} # idem to Hmish package::wtd.var(weight.unit,wtn), see https://en.wikipedia.org/wiki/Weighted_arithmetic_mean 6.1 frequency weights (bessel's correction).  
    
    # calculations
    caa <- x %>%
        pivot_longer(cols = all_of(id.age), names_to = "age", names_prefix = "age.", values_to = "age.prop") %>% # transform to long format
        mutate(age = bound(age, plus),            # make sure age is numeric and smaller than the plus group
               wt = catch * age.prop * lf.prop,   # Ckij = Ck * pijk * pjk (in weight)
               wtn = catch * age.prop * lf.prop / weight.unit) %>%  # Ckij = Ck * pijk * pjk / wjk (in numbers)
        group_by(year,age) %>% 
        summarise(caan = sum(wtn),                 
                  caaw = sum(wt),                           
                  waa = caaw / caan,      # idem to weighted.mean(weight.unit, wtn)
                  waa.sd = weighted.sd(waa,weight.unit,wtn), 
                  laa = weighted.mean(length, wtn),
                  laa.sd = weighted.sd(laa,length,wtn))%>%   # idem to Hmish package::wtd.var(length,wtn), see https://en.wikipedia.org/wiki/Weighted_arithmetic_mean 6.1 frequency weights (bessel's correction). 
        as.data.frame()
    return(caa)
}

