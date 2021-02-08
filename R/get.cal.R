##' Get catch-at-length results
##' @param x Output from function get.samples().
##' @details 
#'  This function calculates catch-at-length in number (caan) as well as the percentage of each length in the annual catch.
##' @import dplyr tidyr 
##' @rdname get.cal
##' @export
get.cal <- function(x){
    
    # calculations
    cal <- x %>%
        mutate(wt = catch * lf.prop / weight.unit) %>% 
        group_by(year,length) %>% 
        summarise(caan = sum(wt)) %>% 
        group_by(year) %>% 
        mutate(percentage = caan/sum(caan)*100) %>% 
        as.data.frame()
    return(cal)
}
