##' ggplot theme for cod
##' @param base_size ggplot base_size
##' @param base_line_size ggplot baseline_line_size
##' @param base_rect_siz ggplot base_rect_size
##' @details JOP theme
##' @import ggplot2
##' @rdname theme_cod
##' @export
theme_cod <- function (base_size = 10, base_line_size = 0.25, base_rect_size = 0.25) {
    theme_bw(base_size = base_size, base_line_size = base_line_size, base_rect_size = base_rect_size) +
        theme(axis.title.y = element_text(angle = 90),
              axis.text.x = element_text(size = 8, colour = "black"),
              axis.text.y = element_text(size = 8, colour = "black"),
              legend.key = element_blank(),
              legend.title = element_text(colour = "black"), 
              legend.text = element_text(size = 8),
              legend.position = "right",
              legend.background = element_blank(),
              panel.grid.major = element_blank(),
              panel.grid.minor = element_blank(),
              axis.line = element_line(color = "black"),
              panel.border = element_rect(colour = "black", fill = NA))
}
##' ggplot theme for mackerel
##' @details EVB theme
##' @import ggplot2
##' @rdname theme_mackerel
##' @export
theme_mackerel <- function () {
    theme_classic()+theme(strip.background = element_blank(),
                              axis.line.x = element_line(color="black"),
                              axis.line.y = element_line(color="black"),
                              legend.background = element_rect(fill=alpha('blue', 0)))
}
