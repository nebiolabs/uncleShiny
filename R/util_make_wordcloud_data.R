
##-------------------------------------------------------------------------
##  Prepare data for creating wordcloud                                  --
##-------------------------------------------------------------------------

make_wordcloud_data <- function(df, var_sel) {
  if (is.null(df)) {
    tibble::tibble(
      word = c("nothing", "is", "selected"),
      freq = c(8, 6, 10)
    )
  } else {
    df |>
      dplyr::mutate(
        pH = stringr::str_c("pH", pH, sep = "_")
      ) |>
      dplyr::select(tidyselect::any_of(var_sel)) |>
      tidyr::pivot_longer(
        cols = tidyselect::everything(),
        names_to = "trash",
        values_to = "word",
        values_drop_na = TRUE
      ) |>
      dplyr::select(word) |>
      dplyr::group_by(word) |>
      dplyr::add_count(name = "freq") |>
      dplyr::distinct(word, freq)
  }
}
