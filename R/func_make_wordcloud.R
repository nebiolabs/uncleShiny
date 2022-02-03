
## -------------------------------------------------------------------------
##  Create wordcloud from set of conditions                              --
## -------------------------------------------------------------------------

make_wordcloud <- function(df, max_area = 20, pal = "Set2") {
  df |> 
    ggplot2::ggplot(ggplot2::aes(
      label = word,
      size = freq,
      color = word
    )) +
    ggwordcloud::geom_text_wordcloud_area() +
    ggplot2::scale_size_area(max_size = 12) +
    ggplot2::scale_color_brewer(palette = pal) +
    ggplot2::theme_minimal()
}
