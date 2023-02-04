##' barplot for catches
##' @param data data.frame 
##' @param x name of columns with x values
##' @param y name of columns with y values
##' @param fill name of columns with fill values
##' @import ggplot2
##' @rdname plotCatch
##' @export
plotCatch <- function(data,x,y,fill=NULL){
    data[,fill] <- factor(data[,fill])
    ggplot(data,aes_string(x=x,y=y,fill=fill))+
        geom_bar(stat='identity',aes(x=year,y=catch))+
        scale_x_continuous(expand=c(0,0))+
        scale_y_continuous(expand=c(0,0))+
        labs(y=y)+
        theme(legend.position = 'top', legend.box="vertical")
}

##' plotAttribution
##' @param x output of get.samples 
##' @import ggplot2 reshape2 dplyr
##' @rdname plotAttribution
##' @export
plotAttribution <- function(x){
    y <- ddply(x,c('id','year','catch'),summarise,
                LF.option=unique(option.lengthfreq),
                LF.nsample=unique(nsample.lengthfreq),
                LF.nfish=sum(n.lf),
                ALK.option=unique(option.agelength),
                ALK.nsample=unique(nsample.agelength),
                ALK.nfish=sum(n.al))
    y <- reshape2::melt(y,id=c('id','year','catch'))
    ggplot(y,aes(x=value))+
        geom_histogram(bins=30,)+
        facet_wrap(~variable,scale='free')+
        labs(y='Count',x='Value')+
        scale_y_continuous(expand=c(0,0))
    
}

##' plotAttribution
##' @param x output of get.samples 
##' @param annual logical
##' @import ggplot2 reshape2 dplyr
##' @rdname plotQuality
##' @export
plotQuality <- function(x,variable=c('option.lengthfreq','option.agelength'),annual=FALSE){
    variable <- match.arg(variable)
    y <- ddply(x,c('year'),transform,catch.prop=catch/sum(catch))
    y$catch.propt <- y$catch.prop/length(unique(y$year))
    names(y)[which(names(y)==variable)] <- 'y'
    
    l <- ifelse(variable=='option.lengthfreq','LF', 'ALK')
    
    if (!annual) {
        ggplot(y,aes(x=as.factor(y),y=catch.propt))+
            geom_bar(stat='identity')+
            labs(x='Option',y='% Landings',title = l)+
            scale_y_continuous(expand=c(0,0),limits=c(0,1))
    }else{
        y2 <- ddply(y,c('year','y'),summarise,catch.prop=sum(catch.prop))
        y2 <- ddply(y2,c('year'),transform,score=round(weighted.mean(y,catch.prop),2))
        ggplot(y2,aes(x=as.factor(y),y=catch.prop))+
            geom_bar(stat='identity')+
            geom_text(aes(x=Inf,y=Inf,label=score,col=score),hjust=2,vjust=2)+
            facet_wrap(~year)+
            labs(x='Option',y='% Landings',title = l)+
            scale_y_continuous(expand=c(0,0),limits=c(0,1))+
            theme(legend.position = 'none')+
            scale_color_gradient(low='green',high='red')
    }
}

##' plotTotal
##' @param x output of get.samples
##' @param y output of get.caa
##' @import ggplot2 dplyr viridis
##' @rdname plotTotal
##' @export
plotTotal <- function(x,y){
    tot <- ddply(unique(x[,c('id','year','catch')]),c('year'),summarise,catch=sum(catch,na.rm=TRUE))
    ggplot(y,aes(x=year))+
        geom_bar(stat='identity',aes(y=caaw,fill=age))+
        geom_line(data=tot,aes(y=catch))+
        scale_fill_viridis()+
        scale_x_continuous(expand=c(0,0))+
        scale_y_continuous(expand=c(0,0))+
        labs(y='Landings (t)',x='Year',fill='Age')
}

##' plotCaan
##' @param x output of get.caa
##' @import ggplot2
##' @rdname plotCaan
##' @export
plotCaan <- function(x){
ggplot(x,aes(x=year,y=age))+
    geom_point(alpha=0.8,aes(size=caan))+ 
    scale_size(range = c(1,12))+
    scale_y_continuous(breaks=min(x$age):max(x$age))+
    labs(x='Year',y='Age',size='Numbers')
}

##' plotCaaw
##' @param x output of get.caa
##' @import ggplot2
##' @rdname plotCaaw
##' @export
plotCaaw <- function(x){
    ggplot(x,aes(x=year,y=age))+
        geom_point(alpha=0.8,aes(size=caaw))+
        scale_size(range = c(1,12))+
        scale_y_continuous(breaks=min(x$age):max(x$age))+
        labs(x='Year',y='Age',size='Weight (t)')
}

##' plotWaa
##' @param x output of get.caa
##' @param ci logical (plot with confidence intervals?)
##' @import ggplot2
##' @rdname plotWaa
##' @export
plotWaa <- function(x,ci=FALSE){
    p <- ggplot(x,aes(x=year,y=waa,group=age))+
        labs(x='Year',y='Weight')+
        scale_x_continuous(expand=c(0,0))
    if(!ci){
        p + geom_line(aes(color=age),size=1)+
            scale_color_viridis()+
            labs(col='Age')
    }else{
        p + facet_wrap(~age,scale='free')+
            geom_ribbon(aes(ymin=waa-1.96*waa.sd,ymax=waa+1.96*waa.sd),alpha=0.6)+
            geom_line(size=1)
    }
}

##' plotLaa
##' @param x output of get.caa
##' @param ci logical (plot with confidence intervals?)
##' @import ggplot2
##' @rdname plotLaa
##' @export
plotLaa <- function(x,ci=FALSE){
    p <- ggplot(x,aes(x=year,y=laa,group=age))+
        labs(x='Year',y='Length')+
        scale_x_continuous(expand=c(0,0))
    if(!ci){
        p + geom_line(aes(color=age),size=1)+
            scale_color_viridis()+
            labs(col='Age')
    }else{
        p + facet_wrap(~age,scale='free')+
            geom_ribbon(aes(ymin=laa-1.96*laa.sd,ymax=laa+1.96*laa.sd),alpha=0.6)+
            geom_line(size=1)
    }
}





