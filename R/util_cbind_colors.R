
##--------------------------------------------------------------------------
##  Hardcode color hex values into dataframe                              --
##--------------------------------------------------------------------------

cbind_colors <- function(df, color_var, palette_name) {
  the_length <- length(unique(df[[color_var]]))
  cat(paste0("The length of the palette is ", the_length,".\n"))
  df |> 
    dplyr::mutate(
      # convert 64-bit integers to character strings before factoring
      dplyr::across(
        .cols = tidyselect::any_of(c("exp_id", "exp_set_id", "instrument")),
        .fns = bit64::as.character.integer64
      )
    ) |> 
    dplyr::mutate(
      !!color_var := forcats::fct_infreq(.data[[color_var]])
    ) |> 
    dplyr::group_by(.data[[color_var]]) |> 
    dplyr::mutate(
      color_hex = make_palette(
        palette_name,
        # length(unique(df[[color_var]]))
        the_length
      )[dplyr::cur_group_id()]
    ) |> 
    dplyr::ungroup()
}
