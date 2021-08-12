
##-------------------------------------------------------------------------
##  Banner generation functions                                          --
##-------------------------------------------------------------------------

banner1 <- function(x, char = "-") {
  bannerCommenter::banner(
    x,
    emph = FALSE,
    upper = FALSE,
    centre = FALSE,
    bandChar = char,
    minHashes = 75
  )
}

banner2 <- function(x, char = "-") {
  bannerCommenter::banner(
    x,
    emph = FALSE,
    upper = FALSE,
    centre = FALSE,
    bandChar = char,
    minHashes = 57
  )
}

banner3 <- function(x, char = "-") {
  bannerCommenter::banner(
    x,
    emph = FALSE,
    upper = FALSE,
    centre = FALSE,
    bandChar = char,
    minHashes = 42
  )
}
