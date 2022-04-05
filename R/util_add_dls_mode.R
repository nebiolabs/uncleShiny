
##-------------------------------------------------------------------------
##  Add mode variable for DLS                                            --
##-------------------------------------------------------------------------

add_dls_mode <- function(df) {
  purrr::pmap_dfr(
    df,
    function(...) {
      temp <- tibble::tibble(...)
      dplyr::mutate(
        temp,
        dls_mode = colSums(
          is.na(
            dplyr::select(temp, tidyselect::contains("mode_diameter"))
          )
        ) |> 
          {\(x) 4 - sum(x)}()
      )
    }
  )
}
