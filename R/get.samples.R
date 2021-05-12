##' Return length frequencies and age-length keys corresponding to the catch from a period, gear and region to use in catch-at-age calculations
##' @param catch Data.frame with these columns: year, period, region, gear, catch.
##' @param lf Data.frame with these columns: year, period, region, gear, length, weight.unit, n, sample.id.
##' @param al Data.frame with these columns: year, period, region, gear, length, age, sample.id.
##' @param tresh.al Threshold of minimum number of samples required for age-length keys. By default = 2.
##' @param tresh.lf Threshold of minimum number of samples required for length frequencies. By default = 2.
##' @param tresh.al.fish Treshold of minimum number of fish required for age-length keys. By default = 5
##' @param period.unit Whether catch and lf are grouped by "month" (default) or "quarter".
##' @param prob.al Maximum likelihood (0-1, default =0.95) allowed for any length of a stratum-specific LF distribution to be of an age not included in the ALK. If it is prob.al likely that an age is missing, the algorithm will continue adding samples.
##' @details 
#' Used to calculate catch-at-age, weight-at-age and length-frequency distributions.
#' 
#' Attributes length-frequencies and age-length keys to a catch level based on a 12 step approach (see steps below). 
#' 
#' For the length-frequencies, the following criteria can be used to determine a representative amount of samples has been found:
#' I. minimum number of samples (tresh.lf).
#' 
#' For the age-length-keys, the following criteria can be used to determine a representative amount of samples has been found:
#' I. minimum number of samples (tresh.al).
#' II. match with the lf.key (prob.al).
#' III. minimum number of fish (tresh.al.fish), so that small or incomplete age key samples can still be used if present.
#' 
#' Regions of the catch (e.g., 3P) can be broader then the sample regions (e.g., 3Pn). Sample regions containing the catch region (such as the 3Pn example) will all be used.
#' When catch in a certain year is not attributed to a certain region, gear or period, that factor is ignored (e.g., catch with no gear will have samples that could be from any gear)
#' Age-length keys can have missing values. Predictions are made based on a multivariate normal distribution, and are presumed to have a precision of 0,001. 
#' 
#' Steps:
#' \enumerate{
#'  \item year, period, region, gear
#'  \item year, neighbouring period, region, gear
#'  \item year, period, gear                 
#'  \item year, neighbouring period, gear
#'  \item year, neighbouring period
#'  \item year
#'  \item neighbouring year, period, gear, region
#'  \item neighbouring year, neigbouring period, gear, region
#'  \item neighbouring year, period, gear
#'  \item neighbouring year, neigbouring period, gear
#'  \item neighbouring year, gear
#'  \item neighbouring year
#' }
##' @import dplyr lubridate tidyr
##' @importFrom data.table rbindlist
##' @rdname get.samples
##' @export
get.samples <- function(catch, lf, al, tresh.al = 2, tresh.lf = 2, tresh.al.fish = 5, period.unit = c('month', 'quarter'), prob.al=0.95){
    
    # 1) Validation of the arguments
    cacol <- c('year', 'period', 'region', 'gear', 'catch') # required columns in catch
    lfcol <- c('year', 'period', 'region', 'gear', 'sample.id', 'length', 'weight.unit', 'n') # required columns in lf
    alcol <- c('year', 'period', 'region', 'gear', 'sample.id', 'length', 'age') # required columns in al
    
    if(!setequal(names(catch), cacol)) stop(paste0('Colnames of catch need to be: ', paste(cacol, collapse = ', ')))
    if(!setequal(names(lf), lfcol))    stop(paste0('Colnames of lf need to be: ',    paste(lfcol, collapse = ', ')))
    if(!setequal(names(al), alcol))    stop(paste0('Colnames of al need to be: ',    paste(lfcol, collapse = ', ')))
    if(any(duplicated(catch[, cacol[1:4]]))) stop('Only one catch value per level allowed.') # one line per level allowed
    
    period.unit <- match.arg(period.unit)
    
    if(prob.al<0 | prob.al>1) warning("prob.al should be restricted to 0-1")
    
    # 2) Minor calculations and variable creation
    id <- as.factor(apply(lf[,lfcol[1:5]],1,paste,collapse=""))
    lf$prop <- ave(lf$n, id, FUN = function(z) z/sum(z))
    lf$date <- with(lf, ymd(paste(year, period, '01')))
    al$date <- with(al, ymd(paste(year, period, '01')))
    
    my.levels <- sapply(c('period', 'region', 'gear'), function(x) sort(unique(c(lf[, x], al[, x])))) # unique levels of each group
    
    # 3) Global alk (alg): from all years and all ages available.
    alg <- table(al$length,al$age)
    alg <- prop.table(alg,1)
    alg <- cbind(length=as.numeric(rownames(alg)),data.frame(rbind(alg)))
    
    le <- expand.grid(length=seq(min(alg$length),max(alg$length),min(diff(alg$length))))
    alg <- merge(le,alg) # matplot(alg[,-1])
    
    alg <- fill.multinom(df = alg, acol = 2:ncol(alg), lcol = 1, id='global', zero = TRUE) # matplot(alg[,-1])

    # 4) For each line of catch, i.e. for each combination of year-period-region-gear, get lf and al data to use in caa calculations
    pb <- txtProgressBar(min = 0, max = nrow(catch), style = 3) # set the progress bar
    ret <- lapply(1:nrow(catch), function(x){
        # get id and total catch
        C <- catch[x, 'catch']
        y <- catch[x, 'year']
        p <- catch[x, 'period']
        r <- catch[x, 'region']
        g <- catch[x, 'gear']
        
        # If p/r/g is NA, the function should not automatically skip options 
        # (e.g. if only missing gear in catch data, option 1 should still be used and not 5)
        if(is.na(p)) p <- my.levels$period
        if(is.na(g)) g <- my.levels$gear
        if(is.na(r)) r <- my.levels$region else r <- my.levels$region[grepl(r, as.character(my.levels$region), fixed = T)]  # samples from region and subregions
        
        # 4.1) Neighbouring dates/months/years 
        d  <- ymd(paste(y, p, '01')) # date (for semesters this will be month 1 to 4, but that doesn't matter)
        dn <- d # neigbouring dates. If no period data, all periods allowed  (so option 1 and 2 are the same)
        if(!is.na(catch[x, 'period'])){      # If period data; normal way
            dn <- d %m+% months(c(-1, 0, 1)) # neigbouring dates
            if(period.unit == 'quarter'){    # cheat and replace december with last quarter
                month(dn[which(month(dn) == 12)]) <- 4
            }       
        }
        pn <- month(dn) # neigbouring periods
        yn <- y + c(-1, 0, 1) # neigbouring years

        # 4.2) LF samples ####
        # 4.2.1) Get samples to be used
        o.lf <- 1 # first option
        n.lf <- 0 # number of samples found
        while(n.lf < tresh.lf && o.lf <= 12){
            id <- find.samples(lf,o.lf,d,dn,g,r,y,yn,p,pn) # find samples according to step o
            this.lf <- lf[id, ] # select them
            n.lf <- nrow(unique(this.lf[,lfcol[1:5]])) # count unique samples 
            o.lf <- o.lf + 1 # go to next step
        }

        # 4.2.2) Get length frequency distribution
        if(n.lf==0){
            warning(paste0('** for catch level ', x, ' no length-frequency exists**'))
            lf.key <- data.frame(length = -1, n.lf = 0, lf.prop = 0, weight.sample = 0, weight.unit = 0)
        }else{
            if(n.lf < tresh.lf){
                warning(paste0('** for catch level ', x, ' the length-frequency threshold was not reached**'))
            }
            lf.key <- this.lf %>%
                      group_by(length) %>% 
                      summarise(n.lf = sum(n), # (gavaris:njk)
                                lf.prop = sum(prop) / sum(this.lf$prop), # (gavaris:pjk ) ALL SAMPLES HAVE EQUAL WEIGHT (n.lf/sum(n/lf) would give more weight to samples with more fish)
                                weight.sample = sum(weight.unit*n), # (gavaris:wjk )
                                weight.unit = mean(weight.unit)) %>%  # (gavaris:wbarjk: he doesn't calculate this becauase he always goes for 'aggregated average fish weight ')
                      filter(n.lf > 0) %>%  # lines where n.lf = 0 are impossible. If present, remove
                      as.data.frame()
        }
        # 4.3) ALK samples ####
        # 4.3.1) Get samples to be used
        o.al <- 1 # first option
        n.al <- 0 # number of samples found
        f.al <- 0 # number of fish found
        a.al <- FALSE # does the ALK match the LF distribution
        while(o.al<=12 && !(n.al>=tresh.al && a.al && f.al>=tresh.al.fish)){ # only continue if steps available AND both quality criteria (number of samples, coverage with LF dist) are met
            id <- find.samples(al,o.al,d,dn,g,r,y,yn,p,pn) # find samples according to step o
            this.al <- al[id,] # select them
            n.al <- nrow(unique(this.al[,alcol[1:5]])) # count unique samples
            f.al <- nrow(this.al) # count fish
            if(n.al>0){
                le.miss <- lf.key[!lf.key$length %in% this.al$length,1]  # lengths not covered by ALK (missing)
                le.prob <- alg[alg$length %in% le.miss,!names(alg) %in% paste0('X',unique(this.al$age))] # for any missing length, probabilities that they are of an age currently not yet included in the ALK
                le.prob <- rowSums(le.prob[,-1,drop=FALSE]) # total probability
                a.al <- any(le.prob>prob.al) # if for any length it is at least prob.al likely that an age is missing, criteria is not met
            } 
            o.al <- o.al + 1                                        # go to next step
        }
        # 4.3.2) Get age-length key
        if(n.al==0){
            warning(paste0('** for catch level ', x, ' no age-length key exists**'))
            age.key <- data.frame(length = -1, n.agekey = 0)                # use -1 to indicate problems 
        }else{
            if(n.al < tresh.al){
                warning(paste0('** for catch level ', x, ' the age-length key threshold was not reached**'))
            }
            age.key <- this.al %>% # age key for this group
                group_by(length, age) %>% 
                count() %>%                   # (gavaris:n'ijk )
                group_by(length) %>% 
                mutate(prop = n / sum(n),     # (gavaris:p'ijk )
                       n.agekey = sum(n)) %>% # (gavaris:n'jk )
                ungroup() %>% 
                pivot_wider(id_cols = c("length", "n.agekey"), names_from = "age", names_prefix = "age.", # put it in a wide format
                            values_from = "prop", values_fill = 0) %>% 
                as.data.frame()
        }

        # 3.4) Bind and fill up the gaps ####
        # 3.4.1) Bind length frequency and age-length keys
        ret <- merge(lf.key, age.key, all=TRUE) 
        cola <- grep('age\\.', names(ret)) # columns with ages
        
        # 3.4.2) Filling the gaps
        ret <- fill.multinom(df = ret, acol = cola, lcol = 1, id=x)
        ret <- ret[!is.na(ret$n.lf),]             # in alk but not lf
        ret[is.na(ret$n.agekey), 'n.agekey'] <- 0 # 0 age key samples

        # 3.5) Return it all ####
        ret <- cbind(id = x, 
                     catch[x, cacol[1:5]][rep(1, nrow(ret)), ],
                     ret,
                     n.lftot = sum(lf.key$n.lf), # (gavaris:nk) total number of fish measured for length in that combination of year-period-region-gear
                     weight.sample.tot = sum(lf.key$weight.sample), # (gavaris:wk) total weight of fish measured for length in that combination year-period-...
                     #weight.unit.mean = sum(lf.key$weight.sample) / sum(lf.key$n.lf),  # (gavaris:wbark)
                     nsample.lengthfreq = n.lf, # Count of samples used to create the LF key
                     nsample.agelength = n.al, # Count of samples used to create the AL key
                     option.lengthfreq = o.lf - 1, # Level of data aggregation used to create the LF key (1-12)
                     option.agelength = o.al - 1) # Level of data aggregation used to create the AL key (1-12)
        
        setTxtProgressBar(pb, x)
        return(ret)
    })
    ret <- rbindlist(ret, fill = TRUE) # automatically fills missing ages for certain grouping      
    ret <- as.data.frame(ret)
    cola <- grep('age\\.', names(ret))
    ret[cola][is.na(ret[cola])] <- 0 # some ages can be missing for a certain catch. They are set to 0
    ages <- sort(as.numeric(gsub("age\\.", "", names(ret)[cola]))) # sorted age columns
    
    # reordering of ret
    ret <- ret[, c("id", cacol[1:5], "length", "n.lf", "lf.prop", "weight.sample", "weight.sample.tot", "weight.unit",
                   "n.agekey", "n.lftot", paste0("age.", ages), "nsample.lengthfreq", "nsample.agelength", "option.lengthfreq", "option.agelength")]
    ret <- ret %>% as_tibble
    
    return(ret)
}

##' Find samples
##' @param df Data.frame in which to find sample.
##' @param o Option for searching.
##' @param d Date.
##' @param nd Neighbouring dates.
##' @param g Gear.
##' @param r Region.
##' @param y Year.
##' @param yn Neighbouring years.
##' @param p Period.
##' @param pn Neighbouring periods.
##' @details 12 steps to find samples.
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
           '12' = {id <- with(df, year %in% yn)})                                             # neighbouring year
    return(id)
}

##' Fill gaps in age-length keys
##' @param df Data.frame with columns length and ages, where NA values need to be replaced
##' @param acol Vector of ids of age columns 
##' @param lcol Id of length column (integer)
##' @param id Identifier for warning messages
##' @param zero Logical. Allow replacement of zero values.
##' @details Functions that uses the nnet::multinom function to impute age probabilities for lengths that were found in LF samples, but absent of AL samples.
##' @importFrom nnet multinom
##' @rdname multinom_fill
fill.multinom <- function(df, acol, lcol, id=NULL, zero=FALSE){
    alen <- as.matrix(df[,acol,drop=FALSE])
    len  <- df[,lcol]
    if(ncol(alen) == 1){ # if there is only one age class than the likelihood is always 1??
        new <- rep(1, nrow(alen))
        warning(paste0('** for catch level ', id, ' only one age class was found **'))
    }else{ # model if there are multiple age classes
        m <- multinom(alen~len, trace = F, maxit = 1500)
        if (m$convergence != 0) warning(paste('** Non-convergence when filling gaps for id', id))
        new <- predict(m, newdata = data.frame(len = df$length), type = "probs") # predictions for all lengths found in df
        new <- round(new, 3) # acceptable precision level (to avoid 0.0000001 % chances of absurd age-length combos)
    }
    
    # replace NA's by predictions
    df[, acol][is.na(df[, acol])] <- new[is.na(df[, acol])] 
    
    # replace zeros by predictions
    if(zero){
        q <- 0.001                 # acceptable precision level
        id <- new>q & df[,acol]==0 # will replace the zeros in ALK that are most likely not zeros
        df[,acol][id] <- new[id] 
    }
    return(df)
}

