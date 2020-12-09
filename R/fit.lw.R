##' fit log length-weight relationship by year or period
##' @param length vector of lengths (log scale)
##' @param weight vector of weights (log scale)
##' @param year vector of years
##' @param period vector of periods
##' @param tresh minimum number of observations
##' @param interval period is month or quarter?
##' @param regression type of regression (ols or rubust)
##' @details 
##' find lenght weight-relationships for all possible combinations of years and periods. Works with a minimal threshold for individuals needed, and if not attained 1) looks for data in nieghbouring months and 2) uses data for the entire year
##' returns coefficients, bias correction factor for use log scale , number of observations and the option used to get enough observations
##' RObuust regression by default (lts). lms and ordinary ls also possible. See MASS package for details
##' @import lubridate
##' @importFrom robustbase lmrob
##' @rdname fit.lw
##' @export
fit.lw <- function(length,weight,year=1,period=1,tresh=150,period.unit=c('month','quarter'),regression=c('robust','ols')){
    
    period.unit <- match.arg(period.unit)
    regression <- match.arg(regression)
    
    y <- unique(year)
    p <- unique(period)
    opt <- c('lw of year and period','lw including neighbouring periods','lw including neigbouring years')
    
    comb <- expand.grid(y=y,p=p)                              # get models for all possible combinations
    
    dates <- ymd(paste(year,period,'01'))                     # all available dates
    
    s <- apply(comb,1,function(x){
        # 1) find samples
        n <- 0
        o <- 1
        while(n<=tresh & o<=3){
            switch(as.character(o), 
                   '1'  = {  # perfect match
                           id <- which(year==x['y'] & period==x['p'])
                       },              
                   '2'  = {  # lw with neighbouring months
                           d <- ymd(paste(x['y'],x['p'],'01'))                     # lw date (for quarters this will be month 1 to 4, but that doesn't matter)
                           dn <- d %m+% months(c(1,0,-1))                            # neigbouring date
                           if(period.unit=='quarter'){
                               month(dn[which(month(dn)==12) ]) <- 4                 # # if quater: replace 12 by 4
                           }   
                           id <- which(dates %in% dn)
                       },              
                   '3'  = { # lw with neigbouring years
                           yn <- x['y'] + c(1,0,-1) 
                           id <- which(year %in% yn)
                       }                            
            )
            o <- o+1
            n <- length(id)  
        }
        # 2) fit model
        switch(regression, 
               'ols'   ={m <- lm(weight[id]~length[id])},              
               'robust'={m <- lmrob(weight[id]~length[id])}
               )
       
        cf <- exp((sigma(m)^2)/2)
        co <- coef(m)
        names(co) <- c('intercept','slope')
        ret <- c(co,cf=cf,nlw=n,option=o-1)
        return(ret)
        })
    
    s <- data.frame(comb,t(s))
    names(s)[1:2] <- c('year','period')
    
    s$option <- opt[s$option]
    s$regression <- regression
    
    return(s)
}
   