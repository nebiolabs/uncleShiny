
##--------------------------------------------------------------------------
##  Format data for interactive plate layout                              --
##--------------------------------------------------------------------------

formatPlateOverlay <- function(df) {
  df |>
    tidyr::separate(
      col = well,
      into = c("well_letter", "well_number"),
      sep = "(?<=^[A-H]{1})(?=\\d+$)",
      remove = FALSE
    ) |>
    dplyr::select(!c(tidyselect::contains("spec"), "residuals")) |>
    dplyr::group_split(exp_set_id) |>
    {
      \(l) rlang::set_names(
        l,
        nm = purrr::map_chr(
          l,
          function(df) {
            paste0(
              "exp_set_",
              bit64::as.character.integer64(unique(df[["exp_set_id"]]))
            )
          }
        )
      )
    }()
}
