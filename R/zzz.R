#' Make Sherman Font available upon package launch

#' @description theme_ir_fonts() is a wrapper function for showtext_auto() that makes Syracuse University Shermans Fonts available upon loading the iRtools package.
#'
#' @importFrom showtext showtext_auto
#' @importFrom sysfonts font_add

.onLoad <- function(libname, pkgname) {
  font_add(
    family = "Sherman Sans", 
    regular = "C:/Windows/Fonts/Sherman Sans/ShermanSans-Book.ttf",
    bold = "C:/Windows/Fonts/Sherman Sans/ShermanSans-Bold.ttf",
    italic = "C:/Windows/Fonts/Sherman Sans/ShermanSans-BookItalic.ttf",
    bolditalic = "C:/Windows/Fonts/Sherman Sans/ShermanSans-BoldItalic.ttf"
)
  
  font_add(
    family = "Sherman Serif",
    regular = "C:/Windows/Fonts/Sherman Serif/ShermanSerif-Book.ttf",
    bold = "C:/Windows/Fonts/Sherman Sans/ShermanSerif-Bold.ttf",
    italic = "C:/Windows/Fonts/Sherman Sans/ShermanSerif-BookItalic.ttf",
    bolditalic = "C:/Windows/Fonts/Sherman Sans/ShermanSerif-BoldItalic.ttf"
)
  
  showtext_auto()
  
}



