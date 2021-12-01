
##--------------------------------------------------------------------------
##  Spectra self-normalization function (0 to 1)                          --
##--------------------------------------------------------------------------

normalize_spectra <- function(df) {
    df |> dplyr::mutate(dplyr::across(
      .cols = tidyselect::contains("spec"),
      .fns = function(cols) {
        purrr::map(
          cols,
          purrr::possibly(function(col) {
            yvar <- do.call(
              switch,
              as.list(c(dplyr::cur_column(), spec_y_switch))
            )
            col[[yvar]] <- (col[[yvar]] - min(col[[yvar]])) /
              (max(col[[yvar]]) - min(col[[yvar]]))
            return(col)
          }, function(col) return(col), quiet = FALSE)
        )
      }
    ))
  }
  