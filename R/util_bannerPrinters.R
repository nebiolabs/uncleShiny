
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
    upper = TRUE,
    centre = FALSE,
    bandChar = char,
    minHashes = 57
  )
}

bannerAssign <- function(x) {
  bannerCommenter::banner(
    x,
    emph = FALSE,
    upper = FALSE,
    centre = FALSE,
    bandChar = ">",
    minHashes = 42
  )
}

bannerReturn <- function(x) {
  bannerCommenter::banner(
    x,
    emph = FALSE,
    upper = FALSE,
    centre = FALSE,
    bandChar = "<",
    minHashes = 42
  )
}

bannerFlag <- function(x) {
  bannerCommenter::banner(
    x,
    emph = FALSE,
    upper = FALSE,
    centre = FALSE,
    bandChar = "/",
    minHashes = 42
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

banner4 <- function(x, char = "-") {
  bannerCommenter::banner(
    x,
    emph = FALSE,
    upper = FALSE,
    centre = FALSE,
    bandChar = char,
    minHashes = 24
  )
}