
##--------------------------------------------------------------------------
##  Format variables for human readability                                --
##--------------------------------------------------------------------------

format_vars <- function(df) {
  df |> 
    dplyr::mutate(
      pH = stringr::str_extract(pH, "^[0-9.]+")
    )
}