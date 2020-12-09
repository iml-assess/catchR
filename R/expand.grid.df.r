##' expand grid for data frames
##' @param ... data.frames 
##' @details https://stackoverflow.com/questions/11693599/alternative-to-expand-grid-for-data-frames
##' @rdname expand.grid.df
##' @export
expand.grid.df <- function(...) type.convert(Reduce(function(...) merge(..., by=NULL), list(...)))

