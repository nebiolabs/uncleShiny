
##--------------------------------------------------------------------------
##  Nest spectra into list columns                                        --
##--------------------------------------------------------------------------

nest_spectra <- function(summary, spec_tbls) {
  join_list <- c(list(summary = summary), spec_tbls)
  join_vars <- c("uncle_summary_id")
  purrr::reduce(
    join_list,
    dplyr::left_join,
    by = join_vars
  )
}
