
##--------------------------------------------------------------------------
##  Hardcode color hex values into dataframe                              --
##--------------------------------------------------------------------------

cbindColors <- function(df, color_var, palette_name) {
  the_length <- length(unique(df[[color_var]]))
  cat(paste0("The length of the palette is ", the_length,".\n"))
  df |> 
    dplyr::mutate(
      !!color_var := forcats::fct_infreq(.data[[color_var]])
    ) |> 
    dplyr::group_by(.data[[color_var]]) |> 
    dplyr::mutate(
      color_hex = mycolors(
        palette_name,
        # length(unique(df[[color_var]]))
        the_length
      )[dplyr::cur_group_id()]
    ) |> 
    dplyr::ungroup()
}
