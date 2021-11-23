
##--------------------------------------------------------------------------
##  Add missing tooltip variables to dataframe                            --
##--------------------------------------------------------------------------

add_missing_tooltip_vars <- function(df, grps) {
  suffixes <- c("_condition_name", "_unit_name", "_unit_value")
  
  full_set <- purrr::flatten_chr(
    purrr::map(
      grps,
      function(var_name) {
        purrr::map_chr(
          suffixes,
          function(suffix) paste0(var_name, suffix)
        )
      }
    )
  )
  
  missing_set <- setdiff(full_set, colnames(df))

  missing_values <- rep(NA_character_, length(missing_set))

  injection <- rlang::set_names(missing_values, nm = missing_set)

  return(rlang::inject(tibble::add_column(df, !!!injection)))
}
