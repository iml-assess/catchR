##' Model to smooth and fill in year*age matrix
##' @param year vector of years
##' @param age  vector of ages
##' @param x vector of values (e.g., weight-at-age)
##' @param cv coefficient of variation
##' @param shrink.cv numeric (0 = original values,1 = identical to historic average for all years)
##' @param ... extra arguments to MakeADFun
##' @return list with model output
##' @details 
#'  \subsection{General info}{
#'    \describe{
#' Models a matrix (e.g., weight-at-age) using an AR1 process over time, age and cohort (separable extension of two densities, 
#' see see section 6.4 of https://kaskr.github.io/adcomp/_book/Densities.html). 
#' This will smooth predictions over the three dimensions and fill in gaps (NA values can be used). 
#' 
#' Developed by Noel Cadigan. 
#'    }
#' }
#'  \subsection{specifics}{
#'    \describe{
#'    Log(x) = age + year + cohort + age*year + residual
#'      \item{age}{fixed effect}
#'      \item{year}{random AR1 effect}
#'      \item{cohort}{random AR1 effect}
#'      \item{age*year}{random separable AR1 effect in age and year}
#'      \item{residual}{based on shrinkage CVs. see shrink.cv}
#'    }
#' }
##' @importFrom TMB MakeADFun sdreport
##' @importFrom stats nlminb
##' @importFrom utils relist packageDescription
##' @useDynLib armatrix
##' @rdname armatrix.fit
##' @export
##' @examples
##' d <- expand.grid(year=2001:2010,age=1:10)
##' d$x <- exp(log(d$age)+rnorm(nrow(d),1,0.1))
##' d$cv <- d$x/10
##' fit <- do.call(armatrix.fit,as.list(d))
armatrix.fit <- function(year,age,x,cv,shrink.cv=0.5,...){
    
    if(missing(year)) stop("argument 'year' is missing, with no default")
    if(missing(age))  stop("argument 'age' is missing, with no default")
    if(missing(x))    stop("argument 'x' is missing, with no default")
    if(missing(cv))   stop("argument 'cv' is missing, with no default")
    
    input <- data.frame(year=year,age=age,x=x,cv=cv)
    
    if(!all(apply(input,2,length)==length(input[[1]]))){
        stop('Not all arguments are of the same length')
    }
    
    if(shrink.cv<0 | shrink.cv>1) warning("shrink.cv restricted to 0-1")
    
    id <- cv==0 | is.na(cv)
    if(any(id)){
        if(all(id)) stop('not all cv should be zero or NA')
        warning('cvs equal to 0/NA replaced by value of historic 95% quantile')
        upper.cv <- tapply(cv[!id],age[!id],quantile,prob=0.95)
        cv[id] <- upper.cv[age[which(id)]-min(age)+1]
    }
    
    mean.cv <- tapply(cv[!id0],age[!id0],mean) 
    input$se <- as.vector((1-shrink.cv)*cv + shrink.cv*mean.cv[age-min(age)+1])
    input$x <- log(x)
    
    date <- expand.grid(year=min(year):max(year), age=min(age):max(age))
    datd <- merge(date, input, all.x = TRUE)
    datd[is.na(datd)] <- NA  # model does not accept NaN
    dat <- as.list(datd[,-4])
    
    para <- list( 
        age_eff=tapply(dat$x,dat$age,mean,na.rm=T),
        year_eff = rep(0,length(unique(dat$year))),       
        cohort_eff = rep(0,length(unique(dat$year-dat$age))),
        log_std=rep(log(0.1),3),                 
        logit_rho=rep(0,4),      
        dev=matrix(0,nrow=length(unique(dat$year)),ncol=length(unique(dat$age)),byrow=T)
    )  
 
    para.L <- list(
        age_eff=rep(-Inf,length(para$age_eff)),
        log_std=rep(-Inf,3),                 
        logit_ar=rep(-10,4)  
    )
    
    para.U <- list(
        age_eff=rep(Inf,length(para$age_eff)),
        log_std=rep(2,3),                 
        logit_ar=rep(10,4)  
    )
    
    lower = unlist(para.L)
    upper = unlist(para.U)
    
    obj <- MakeADFun(dat,para,
                     random=c("year_eff","cohort_eff","dev"), 
                     DLL = "armatrix",                 
                     inner.control=list(maxit=500,trace=F),
                     ...)

    obj$gr(obj$par)
    opt <- nlminb(obj$par,obj$fn,obj$gr,upper=upper,lower=lower,
                  control = list(trace=10,eval.max=2000,iter.max=1000))

    rep <- obj$report()
    sdrep<-sdreport(obj)
    
    datd$pred <- rep$pred
    datd$res <- rep$std_resid  
    datd[is.na(datd$x),'res'] <- NA
    datd$sign <- sign(datd$res)
    datd$pred_exp <- exp(rep$pred)
    datd$pred_exp_low = exp(sdrep$value - qnorm(0.975)*sdrep$sd) 
    datd$pred_exp_upp = exp(sdrep$value + qnorm(0.975)*sdrep$sd)
    
    ret <- list(output=datd, opt=opt, obj=obj, rep=rep)
    attr(ret, "RemoteSha") <- substr(packageDescription("catchR")$RemoteSha, 1, 12)
    attr(ret, "Version") <- packageDescription("catchR")$Version
    class(ret)<-"armatrix"
    return(ret)   
}
