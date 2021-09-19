
##--------------------------------------------------------------------------
##  Float8 string parser                                                  --
##--------------------------------------------------------------------------

parse_float8 <- function(s) {
  readr::parse_double(
    purrr::simplify(
      stringr::str_split(
        stringr::str_replace_all(s, "(^\\{|\\}$)", ""),
        pattern = ","
      )
    )
  )
}
