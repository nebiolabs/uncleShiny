
##--------------------------------------------------------------------------
##  Custom categorical palette generator                                  --
##--------------------------------------------------------------------------

mycolors <- function(palette, n) {
  paln <- palnList[[palette]]
  if(palette == "Default") {
    return(NULL)
  } else {
    colorRampPalette(brewer.pal(paln, palette))(n)
  }
}