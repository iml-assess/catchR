##' catch-at-age raw: return length frequencies and raw age-length keys
##' @param catch data.frame with columns  year,period,region,gear,catch
##' @param lf data.frame with columns year,period,region,gear,length,weight.sample,n, sample.id
##' @param al data.frame with columns year,period,region,gear,length,age,sample.id
##' @param tresh.al threshold of minimum number of samples required for age length
##' @param tresh.al threshold of minimum number of samples required for length frequencies
##' @param period.unit whether catch and lf are grouped by month or quarter
##' @details diffuses catch into length classes
##' @importFrom  data.table rbindlist
##' @importFrom nnet multinom
##' @importFrom plyr ddply
##' @rdname caa.raw
##' @export
caa.raw <- function(catch,lf,al,tresh.al=2,tresh.lf=2,period.unit=c('month','quarter')){

    cacol <- c('year','period','region','gear','catch')
    lfcol <- c('year','period','region','gear','sample.id','length','weight.sample','n')
    alcol <- c('year','period','region','gear','length','age','sample.id')
    
    if(!setequal(names(catch),cacol)) stop(paste0('colnames of catch need to be: ', paste(cacol,collapse = ', ')))
    if(!setequal(names(lf),lfcol))    stop(paste0('colnames of lf need to be: ',    paste(lfcol,collapse = ', ')))
    if(!setequal(names(al),alcol))    stop(paste0('colnames of al need to be: ',    paste(lfcol,collapse = ', ')))
    if(any(duplicated(catch[,cacol[1:4]]))) stop('only one catch value per level allowed')
    
    period.unit <- match.arg(period.unit)
    
    lf <- ddply(lf,lfcol[1:5],transform,prop=n/sum(n))   # proportion of each length in sample
    lf$weight.unit <- with(lf, weight.sample/n)       # average fish weight
    
    lf$date <- with(lf,ymd(paste(year,period,'01')))
    al$date <- with(al,ymd(paste(year,period,'01')))
    
    my.levels <- sapply(c('period','region','gear'),function(x) unique(c(lf[,x],al[,x]))) # unique levels of each group
    
    ### for every catch row, get caa information
    pb <- txtProgressBar(min = 0, max = nrow(catch), style = 3)
    ret <- lapply(1:nrow(catch),function(x){
        # get id and total catch
        C <- catch[x,'catch']
        y <- catch[x,'year']
        p <- catch[x,'period']
        r <- catch[x,'region']
        g <- catch[x,'gear']
        
        # !! if p / r / g  is na -> the function should not automatically skip options (e.g. if only missing gear in catch data, option 1 should still be used and not 5)
        if(is.na(p)) p <- my.levels$period
        if(is.na(r)) r <- my.levels$region
        if(is.na(g)) g <- my.levels$gear
        
        # neighbouring dates/months/years 
        d  <- ymd(paste(y,p,'01'))                        # date (for semesters this will be month 1 to 4, but that doesn't matter)
        dn <- d                                           # !! neigbouring date; IF no period data; all periods allowed  (so option 1 and 2 are the same :/)
        if(!is.na(catch[x,'period'])){                    # IF period data; normal way
            dn <- d %m+% months(c(1,0,-1))                # neigbouring date
            if(period.unit=='quarter'){                   # cheat and replace december with last quarter
                month(dn[which(month(dn)==12)]) <- 4
            }       
        }
        pn <- month(dn)                               # neigbouring period
        yn <- y + c(1,0,-1)                           # neigbouring year
        
        #####  1) ALK samples ####################################################################################
        # A get samples
        o.al <- 1                           # first option
        n.al <- 0                           # number of samples found
        while(n.al<=tresh.al & o.al<=12){                                                   # keep looking as long as n not big enough and next step possible
            id <- find.samples(al,o.al,d,dn,g,r,y,yn,p,pn)                                  # find samples according to step o
            this.al <- al[id,]                                                              # select them
            samples <- unique(apply(this.al[,alcol[1:4]], 1, paste, collapse=" "))          # count unique samples
            n.al <- length(samples)                     
            o.al <-  o.al+1                                                                 # go to next step
        }
        # B get key
        if(n.al<tresh.al){
            warning('impossible to reach threshold for age-length key')
            age.key <- data.frame(length=-1,n.agekey=0)                                     # use -1 to indicate problems 
        }else{
            age.key <- with(this.al,table(length,age))                                      # age key for this group
            n.fish  <- rowSums(age.key)                                                     # store info on number of fish
            age.key <- prop.table(age.key,1)                                                # get proportions
            age.key <- data.frame(length=as.numeric(rownames(age.key)),                     # get a df with all the info
                                  n.agekey=n.fish,
                                  as.data.frame.matrix(age.key))
        }
        
        #####  2) LF samples ####################################################################################
        # A get samples
        o.lf <- 1                           # first option
        n.lf <- 0                           # number of samples found
        while(n.lf<=tresh.lf & o.lf<=12){
            id <- find.samples(lf,o.lf,d,dn,g,r,y,yn,p,pn)                                  # find samples according to step o
            this.lf <- lf[id,]                                                              # select them
            samples <- unique(apply(this.lf[,lfcol[1:5]], 1, paste, collapse=" "))          # count unique samples
            n.lf <- length(samples)                     
            o.lf <-  o.lf+1                                                                 # go to next step
        }
        # B get distribution
        if(n.lf<tresh.lf){
            warning('impossible to reach threshold')
            lf.key <- data.frame(length=-1,n.lf=0,prop=0,weight.sample=0,weight.unit=0)
        }else{
            lf.key <- ddply(this.lf,c('length'),summarise,                                  # for selected samples, get MEAN weight and propotion (so each sample has equal weight)
                            n.lf=sum(n),
                            prop=mean(prop),
                            weight.sample=sum(weight.sample),
                            weight.unit=mean(weight.unit))
        }
        
        #####  3) bind and fill up the gaps ####################################################################################
        # A bind
        ret <- merge(lf.key,age.key,all = T)
        cola <- grep('X',names(ret))                                           # columns with ages
        
        # B fill  (here I deviate from JOP by using just round to avoid unlikely probabilities)
        if(any(is.na(ret[,cola]))){
            mod <- as.matrix(na.omit(ret[,c(1,cola)]))                                   # matrix with the age-length key
            alen <- mod[,-1]
            len  <- mod[,1]
            m    <- multinom(alen~len,trace=F,maxit=1000)                                # fit model
            if(m$convergence!=0)warning(paste0('non-convergence when filling gaps for id ',x))
            new <- predict(m, newdata = data.frame(len = ret$length), type = "probs")    # predict for all lengths in the freq data
            new <- round(new,3)                                                          # acceptable precision level (to avoid 0.0000001% chances of absurd age-length combos)
            
            ret[,cola][is.na(ret[,cola])] <- new[is.na(ret[,cola])]                      # replace Na's by predictions
            ret[is.na(ret$n.agekey),'n.agekey'] <- 0                                     # there were 0 age key samples
        }

        ret[is.na(ret$n),names(lf.key)[-1]] <- 0                                         # when there are lenghts in the alk that are not in the length frequencies -> keep them but give 0 weight
        
        #####  4) return it all ####################################################################################
        ret <- cbind(id = x, 
                     catch[x, cacol[1:5]][rep(1, nrow(ret)),], 
                     ret,
                     n.lftot = sum(lf.key$n),
                     weight.sample.tot = sum(lf.key$weight.sample),
                     nsample.lengthfreq = n.lf,
                     nsample.agelength = n.al,
                     option.lengthfreq = o.lf-1,
                     option.agelength = o.al-1)
        
        setTxtProgressBar(pb, x)
        return(ret)
    })
    ret <- rbindlist(ret,fill=TRUE)          # automatically fills missing ages for certain grouping      
    ret <- as.data.frame(ret)
    ret[is.na(ret)] <- 0                     # some ages can be missing for a certain catch -> set to 0
    return(ret)
}

##' find samples
##' @param df data.frame in which to find sample
##' @param o option for searching
##' @param d date
##' @param nd neighbouring date
##' @param g gear
##' @param r region
##' @param y year
##' @param yn neighbouring year
##' @param p period
##' @param pn neighbouring period
##' @details 12 steps to find samples
##' @rdname find.samples
find.samples <- function(df,o,d,dn,g,r,y,yn,p,pn){
    switch(as.character(o), 
           '1'  = {id <- with(df, date %in% d  & gear %in% g & region %in% r)},               # year, period, region, gear
           '2'  = {id <- with(df, date %in% dn & gear %in% g & region %in% r)},               # year, neighbouring period, region, gear
           '3'  = {id <- with(df, date %in% d  & gear %in% g)},                               # year, period, gear                 
           '4'  = {id <- with(df, date %in% dn & gear %in% g)},                               # year, neighbouring period, gear
           '5'  = {id <- with(df, date %in% dn)},                                             # year, neighbouring period
           '6'  = {id <- with(df, year %in% y)},                                              # year
           '7'  = {id <- with(df, year %in% yn & period %in% p  & gear %in% g & region %in% r)},  # neighbouring year, period, gear, region
           '8'  = {id <- with(df, year %in% yn & period %in% pn & gear %in% g & region %in% r)},  # neighbouring year, neigbouring period, gear, region
           '9'  = {id <- with(df, year %in% yn & period %in% p  & gear %in% g)},              # neighbouring year, period, gear
           '10' = {id <- with(df, year %in% yn & period %in% pn & gear %in% g)},              # neighbouring year, neigbouring period, gear
           '11' = {id <- with(df, year %in% yn & gear %in% g)},                               # neighbouring year, gear
           '12' = {id <- with(df, year %in% yn)}                                              # neighbouring year
    )
    return(id)
}

