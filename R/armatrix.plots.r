##' Plot residuals armatrix model
##' @param x object of class armatrix
##' @param xlab xlab
##' @param ylab ylab
##' @param title title
##' @param size bubble size
##' @param col names color vector (positive, zero, negative)
##' @details ...
##' @import ggplot2
##' @rdname armatrix.res
##' @export
armatrix.res <-function(x,xlab='Year',ylab='Age',title='Standardized residuals',size=1.5,col=c('1'='blue','-1'='red')){
    d <- x$output
    ggplot(d, aes(x=year, y=age, color=factor(sign))) + 
        geom_point(aes(size = size*abs(res)),alpha=0.6,show.legend = FALSE) +
        scale_size_continuous(range = c(0,10)) + 
        ggtitle(title) +
        scale_color_manual(values = col)+
        labs(x=xlab,y=ylab)
}

##' Plot predicted and observed armatrix model
##' @param x object of class armatrix
##' @param xlab xlab
##' @param ylab ylab
##' @param title title
##' @param  ... extra arguments to facet_wrap
##' @details ...
##' @import ggplot2
##' @rdname armatrix.predobs
##' @export
armatrix.predobs <- function(x,xlab='Year',ylab='Age',title='Predicted vs Observed',...){
    d <- x$output
    ggplot(d,aes(x=year))+
        geom_line(aes(y=pred_exp),col='red')+
        geom_ribbon(aes(ymax=pred_exp_upp, ymin=pred_exp_low), fill='red',alpha=0.4)+
        geom_point(aes(y=exp(x)),size=1)+
        facet_wrap(~age,...)+
        ggtitle(title) +
        labs(x=xlab,y=ylab)
}

##' Plot effect sizes
##' @param x object of class armatrix
##' @param xlab xlab
##' @param ylab ylab
##' @param title title
##' @details ...
##' @import ggplot2
##' @importFrom grid unit.pmax
##' @importFrom gridExtra grid.arrange
##' @rdname armatrix.effects
##' @export
armatrix.effects <- function(x){
    
    d1 <- data.frame(age=sort(unique(x$output$age)),age_eff=x$rep$age_eff)
    d2 <- data.frame(year=sort(unique(x$output$year)),year_eff=x$rep$year_eff)
    d3 <- data.frame(cohort=sort(unique(x$output$year-x$output$age)),cohort_eff=x$rep$cohort_eff)
    
    gglayers <- list(geom_line(),geom_hline(yintercept=0,lty='dashed'))
    p1 <- ggplot(d1,aes(x=age,y=age_eff))+labs(x='Age',y='')+gglayers
    p2 <- ggplot(d2,aes(x=year,y=year_eff))+labs(x='Year',y='')+gglayers
    p3 <- ggplot(d3,aes(x=cohort,y=cohort_eff))+labs(x='Cohort',y='')+gglayers
    
    g1 <- ggplotGrob(p1)
    g2 <- ggplotGrob(p2)
    g3 <- ggplotGrob(p3)
    maxWidth <- grid::unit.pmax(g1$widths[2:5], g2$widths[2:5],g3$widths[2:5])
    g1$widths[2:5] <- as.list(maxWidth)
    g2$widths[2:5] <- as.list(maxWidth)
    g3$widths[2:5] <- as.list(maxWidth)
    grid.arrange(g1, g2, g3, ncol=1,left='Model Effects')
}

##' Plot cvs used in armatrix model
##' @param x object of class armatrix
##' @param xlab xlab
##' @param ylab ylab
##' @param col color vector of length two
##' @param  ... extra arguments to facet_wrap
##' @details ...
##' @import ggplot2 
##' @rdname armatrix.cvs
##' @export
armatrix.cvs <- function(x,xlab='Year',ylab='CV',col=c('black','red'),...){
    d <- x$output[,c(1:2,4:5)]
    names(d)[3:4] <- c('original CV (observed)','model CV (input)')
    d <- suppressWarnings(melt(d,id.vars = c('year','age'),variable.name = 'CV'))
    
    ggplot(d,aes(x=year,y=value,col=CV,group=CV))+
        geom_line()+
        facet_wrap(.~age,...)+
        labs(x=xlab,y=ylab)+
        scale_color_manual(values=col)
}

##' Plot predicted armatrix model
##' @param x object of class armatrix
##' @param xlab xlab
##' @param ylab ylab
##' @param title title
##' @param  ... extra arguments to facet_wrap
##' @details ...
##' @import ggplot2
##' @rdname armatrix.pred
##' @export
armatrix.pred <- function(x,xlab='Year',ylab='Age',title='Predicted values',...){
    d <- x$output
    ggplot(d,aes(x=year))+
        geom_line(aes(y=pred_exp))+
        facet_wrap(~age,...)+
        ggtitle(title) +
        labs(x=xlab,y=ylab)
}

##' Plot predicted armatrix model
##' @param x object of class armatrix
##' @param xlab xlab
##' @param ylab ylab
##' @param title title
##' @details ...
##' @import ggplot2
##' @rdname armatrix.pred
##' @export
armatrix.waa <- function(x,xlab='Year',ylab='Weight (kg)',title='',col='Age'){
    d <- x$output
    ggplot(d,aes(x=year,y=pred_exp,col=as.factor(age)))+
        geom_line()+
        ggtitle(title) +
        labs(x=xlab,y=ylab,col=col)+
        scale_x_continuous(expand=c(0,0))
}

  
##' Plot residuals armatrix model (dotplots)
##' @param x object of class armatrix
##' @param ylab ylab
##' @details ...
##' @import ggplot2
##' @importFrom gridExtra grid.arrange
##' @rdname armatrix.res2
##' @export  
armatrix.res2 <- function(x,ylab='Standardized residuals'){
    d <- x$output
    d$cohort <- with(d,year-age)
    
    p1 <- ggplot(d,aes(x=year,y=res))+geom_text(aes(label=age),size=2)+geom_hline(yintercept=0,linetype='dashed')+labs(y='',x='Year')+geom_smooth()
    p2 <- ggplot(d,aes(x=cohort,y=res))+geom_point(size=1)+geom_hline(yintercept=0,linetype='dashed')+labs(y='',x='Cohort')+geom_smooth()
    p3 <- ggplot(d,aes(x=age,y=res))+geom_point(size=1)+geom_hline(yintercept=0,linetype='dashed')+labs(y='',x='Age')
    p4 <- ggplot(d,aes(x=pred_exp,y=res))+geom_point(size=1)+geom_hline(yintercept=0,linetype='dashed')+labs(y='',x='Predicted')
    
    grid.arrange(p1, p2, p3, p4,ncol=1,left=ylab)
}