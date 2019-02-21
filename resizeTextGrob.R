#Inspired by: https://ryouready.wordpress.com/2012/08/01/creating-a-text-grob-that-automatically-adjusts-to-viewport-size/


library(grid)
library(scales)
 
resizingTextGrob <- function(...)
{
  grob(tg=textGrob(...), cl="resizingTextGrob")
}

drawDetails.resizingTextGrob <- function(x, recording=TRUE)
{
  grid.draw(x$tg)
}

preDrawDetails.resizingTextGrob <- function(x)
{
  h <- convertHeight(unit(1, "snpc"), "mm", valueOnly=TRUE)
  #fs <- rescale(h, to=c(18, 7), from=c(120, 20))
  fs <- rescale(h, to=c(7.5, 3), from=c(120, 10))
  pushViewport(viewport(gp = gpar(fontsize = fs)))
}

postDrawDetails.resizingTextGrob <- function(x)
popViewport()
