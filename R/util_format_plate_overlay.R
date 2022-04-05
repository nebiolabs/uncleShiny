
##--------------------------------------------------------------------------
##  Format data for interactive plate layout                              --
##--------------------------------------------------------------------------

format_plate_overlay <- function(df) {
  df |>
    tidyr::separate(
      col = well,
      into = c("well_letter", "well_number"),
      sep = "(?<=^[A-H]{1})(?=\\d+$)",
      remove = FALSE
    ) |>
    dplyr::select(!tidyselect::any_of(c("residuals"))) |>
    dplyr::group_split(exp_set_id) |>
    {
      \(l) rlang::set_names(
        l,
        nm = purrr::map_chr(
          l,
          function(df) {
            paste0(
              "Experiment Set ",
              unique(df[["exp_set_id"]]),
              " (",
              unique(df[["plate"]]),
              ")"
            )
          }
        )
      )
    }()
}
