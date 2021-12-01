
##--------------------------------------------------------------------------
##  Generate a base plate for layout                                      --
##--------------------------------------------------------------------------

make_base_plate <- function(format = c(96, 384, 81)) {
  if (missing(format)) {
    format <- 96
  }
  
  if (format == 81) {
    n_rows <- 9
    n_cols <- 9
  } else if (format == 384) {
    n_rows <- 16
    n_cols <- 24
  } else {
    n_rows <- 8
    n_cols <- 12
  }
  
  tibble::tibble(
    well = purrr::map2_chr(
      rep(c(LETTERS[1:n_rows]), n_cols),
      purrr::flatten_chr(
        purrr::map(c(1:n_cols), rep, n_rows)
      ),
      paste0
    )
  ) |> 
    tidyr::separate(
      col = well,
      into = c("well_letter", "well_number"),
      sep = "(?<=^[A-P]{1})(?=\\d+$)",
      remove = FALSE
    ) |> 
    dplyr::mutate(across(
      c(well_letter, well_number),
      .fns = readr::parse_character
    ))
}
