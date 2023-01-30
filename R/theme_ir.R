#' An Office of Institutional Research Theme
#'
#' @description A clean theme for presenting various visuals, plots, and graphics
#' @param base_size base font size
#' @param ticks logical Show axis ticks?
#' @param gridlines logical Show major grid lines?
#' @note The default font family is set to 'Sherman Sans.' theme_ir() attaches extrafont automatically. If Sherman fonts are not avaialble, they will be substituted with generic system fonts.
#'
#' @importFrom extrafont fonts
#' @import ggplot2
#'
#' @export theme_ir

theme_ir <- function (base_size = 12, ticks = TRUE, gridlines = FALSE)
{
  if(.Platform$OS.type == 'unix') {
    base_family <- ifelse('ShermanSerif-Book' %in% fonts(),'ShermanSerif-Book', 'serif')
    }

  if(.Platform$OS.type == 'windows') {
    base_family <- ifelse('Sherman Serif' %in% fonts(),'Sherman Serif', 'serif')
  }
   else{
      base_family <- 'serif'
    }

  ret <- theme_bw(base_family = base_family, base_size = base_size) +
    theme(text = element_text(colour = '#333333'),
          legend.background = element_blank(),
          legend.key = element_blank(),
          panel.background = element_blank(),
          panel.border = element_blank(),
          strip.background = element_blank(),
          strip.text = element_text(color = '#ADB3B8', size = 16),
          plot.background = element_blank(),
          axis.line = element_line(color = '#333333', size = .5),
          axis.title = element_text(size = 14),
          axis.ticks = element_line(color = '#333333'),
          axis.text = element_text(size = 12, color = '#333333'),
          panel.grid = element_blank(),
          plot.title = element_text(color = '#3E3D3C', size = 20),
          plot.subtitle = element_text(color = '#6F777D', size = 14),
          plot.caption = element_text(color = '#333333', size = 12))

  if (!ticks) {
    ret <- ret + theme(axis.ticks = element_blank())
  }

  if(gridlines == T){
    ret <- ret +
      theme(panel.grid.major.x = element_line(color = '#ADB3B8', size = .5, linetype = 3),
            panel.grid.major.y = element_line(color = '#ADB3B8', size = .5, linetype = 3),
            panel.grid.minor.x = element_line(color = '#ADB3B8', size = .5, linetype = 3),
            panel.grid.minor.y = element_line(color = '#ADB3B8', size = .5, linetype = 3),
            axis.line = element_blank(),
            axis.ticks = element_blank())
  }

  ret
}
