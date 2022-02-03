
##--------------------------------------------------------------------------
##  Convert 64-bit integer to character string                            --
##--------------------------------------------------------------------------

df_char_int64 <- function(df) {
  path <- "R/util_vars.R"
  if (file.exists(path)) {
    source(path)
  } else {
    character_conversions <- c(
      "exp_set_id",
      "exp_id",
      "uncle_summary_id",
      "instrument",
      "well_id"
    )
  }
  
  dplyr::mutate(
    .data = df,
    dplyr::across(
      .cols = tidyselect::any_of(character_conversions),
      .fns = ~ ifelse(
        purrr::map_lgl(.x, bit64::is.integer64),
        purrr::map_chr(.x, bit64::as.character.integer64),
        purrr::map_chr(.x, as.character)
      )
    )
  )
}
