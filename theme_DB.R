library(ggpubr)
theme_DB <- function(border = T, 
                     legend = "bottom", 
                     panel.grid.col = "gray60", 
                     linetype = "dotted",
                     grid.x = F,
                     rotate.x = F,
                     bold.axis.title = F) {
  ret <- theme_pubr(border = border, legend = legend) +
    theme(panel.grid.major.y = element_line(colour = panel.grid.col, 
                                            linetype = linetype))
  if (grid.x) {
    ret <- ret + theme(panel.grid.major.x = element_line(colour = panel.grid.col, 
                                                         linetype = linetype))
  }
  if (rotate.x) {
    ret <- ret + theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5))
  }
  if (bold.axis.title) {
    ret <- ret + theme(axis.title = element_text(face = "bold"))
  }
  return(ret)
}
