
## -------------------------------------------------------------------------
##  Create wordcloud from set of conditions                              --
## -------------------------------------------------------------------------

make_wordcloud <- function(df, min_freq) {
  ggwrdcld <- function(df) {
    df |> 
      ggplot2::ggplot(ggplot2::aes(
        label = word,
        size = freq,
        color = word
      )) +
      ggwordcloud::geom_text_wordcloud_area() +
      ggplot2::scale_size_area(max_size = 20) +
      ggplot2::scale_color_brewer(palette = "Set2") +
      ggplot2::theme_minimal()
  }
  
  if (is.null(df)) {
    tibble::tibble(
      word = c("nothing", "is", "selected"),
      freq = c(8, 6, 10)
    ) |> 
      ggwrdcld()
  } else {
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
      ggwrdcld()
  }
}
