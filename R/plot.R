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
