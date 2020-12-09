##' Bubble plot
##' @param x matrix
##' @param scale scaler for the bubble size (scale_range)
##' @param col color for the bubbles. if 2 colors are given, they indicate positive and negative values
##' @param alpha transparency of the bubbles
##' @param xlab x label
##' @param ylab y label
##' @param arrow for cohort strength. see details
##' @param detatils
##' Arrows are draw in according to cohort strength (here average value of cohorts). 0 = no arrows, 1 = all arrows and values in between indicate proportions of total
##' @rdname bubble
##' @import ggplot2
##' @importFrom 
##' tidyr gather
##' @examples 
##' x <-matrix(rnorm(500,0.5,0.2),nrow=10,ncol=50)
##' dimnames(x) <- list(age=1:10,year=1951:2000)
##' bubble(x)
##' @export
bubble <- function(x,xlab='Year',ylab='Age',scale=15,col='black',alpha=0.8,arrow=0){
    
    d <- dimnames(x)
    if(is.null(d[[1]])) rownames(x) <- 1:nrow(x)
    if(is.null(d[[2]])) colnames(x) <- 1:ncol(x)
    
    long <- as.data.frame(as.table(x))
    names(long) <- c('y','x','value')
    long <- type.convert(long)
    long$posneg <- ifelse(long$value<=0,'<0','>0')

    p <- ggplot(long,aes(x=x,y=y,size=value))+
        scale_size(range = c(1,scale)) +
        ylab(ylab)+xlab(xlab)+
        labs(size="")
    
    # color options
    if(length(col)==2){
        p <- p + geom_point(aes(col=posneg))+scale_color_manual(values=col,alpha=alpha)
    }else{
        p <- p + geom_point(col=col,alpha=alpha)
    }
    
    # arrow
    if(arrow!=0){
        xs <- t(apply(x,1,scale))
        a <- cohort(xs)
        a <- a[a$strength>=quantile(a$strength, 1-arrow, na.rm=TRUE),]
        names(a)[2] <- 'x'
        a$y <- min(long$y)
        a$yend <- max(long$y)
        a$xend <- a$x+max(long$y)-min(long$y)
        p <- p+geom_segment(data=a,aes(y=y,x=x,yend=yend,xend=xend,size=strength),alpha=0.5,col='darkgrey')+
            scale_x_continuous(limits=range(long$x))
            
    }
    return(p)
}
