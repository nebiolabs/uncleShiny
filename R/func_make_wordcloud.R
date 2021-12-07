
##-------------------------------------------------------------------------
##  Create wordcloud from set of conditions                              --
##-------------------------------------------------------------------------

make_wordcloud <- function(df, min_freq) {
  df |> 
    dplyr::mutate(
      pH_condition_name = stringr::str_c(
        pH_condition_name,
        pH_unit_value,
        sep = "_"
      )
    ) |> 
    dplyr::select(tidyselect::contains("condition_name")) |> 
    tidyr::pivot_longer(
      cols = tidyselect::everything(),
      names_to = "trash", 
      values_to = "word",
      values_drop_na = TRUE
    ) |> 
    dplyr::select(word) |> 
    dplyr::group_by(word) |> 
    dplyr::add_count(name = "freq") |> 
    dplyr::distinct(word, freq) |>
    dplyr::filter(freq > min_freq) |> 
    wordcloud2::wordcloud2(
      shape = "square",
      fontFamily = google_heading_font,
      minRotation = -0.2,
      maxRotation = 0.2
    )
}
