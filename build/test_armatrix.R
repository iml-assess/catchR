setwd("./src")
compile('armatrix.cpp', "-O0 -g")
dyn.load(dynlib("armatrix"))

year <- caa$year
age <- caa$age
x <- caa$waa
cv <- caa$waa.sd/caa$waa
shrink.cv <- 0.5

#year <- sub.catch$year
#age <- sub.catch$age
#x <- sub.catch$Cw
#cv <- sub.catch$CV1

input <- data.frame(year=year,age=age,x=x,cv=cv)

id <- cv==0 | is.na(cv)
if(any(id)){
    upper.cv <- tapply(cv[!id],age[!id],quantile,prob=0.95)
    cv[id] <- upper.cv[age[which(id)]-min(age)+1,drop=TRUE]
}

mean.cv <- tapply(cv[!id0],age[!id0],mean)
input$se <- as.vector((1-shrink.cv)*cv + shrink.cv*mean.cv[age-min(age)+1])
input$x <- log(x)

date <- expand.grid(year=min(year):max(year), age=min(age):max(age))
datd <- merge(date, input, all.x = TRUE)
datd[is.na(datd)] <- NA
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
    log_std=rep(Inf,3),                 
    logit_ar=rep(10,4)  
)

lower = unlist(para.L)
upper = unlist(para.U)

obj <- MakeADFun(dat,para,
                 random=c("year_eff","cohort_eff","dev"), 
                 DLL = "armatrix",                 
                 inner.control=list(maxit=500,trace=F))

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

fit <- list(output=datd, opt=opt, obj=obj, rep=rep)
attr(fit, "RemoteSha") <- substr(packageDescription("catchR")$RemoteSha, 1, 12)
attr(fit, "Version") <- packageDescription("catchR")$Version
class(fit)<-"armatrix"

armatrix.res(fit)
armatrix.res2(fit)
armatrix.predobs(fit,scale='free')
armatrix.effects(fit)
armatrix.cvs(fit,scale='free')
armatrix.pred(fit,scale='free')
