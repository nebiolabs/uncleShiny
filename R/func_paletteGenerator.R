
##--------------------------------------------------------------------------
##  Custom categorical palette generator                                  --
##--------------------------------------------------------------------------

mycolors <- function(palette, n) {
  paln <- palnList[[palette]]
  if(palette == "Default") {
    return(NULL)
  } else {
    grDevices::colorRampPalette(RColorBrewer::brewer.pal(paln, palette))(n)
  }
}