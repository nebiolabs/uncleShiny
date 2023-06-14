
##--------------------------------------------------------------------------
##  Add missing tooltip variables to dataframe                            --
##--------------------------------------------------------------------------

add_missing_tooltip_vars <- function(df, grps) {
  missing_set <- setdiff(grps, colnames(df))

  missing_values <- rep(NA_character_, length(missing_set))

  injection <- rlang::set_names(missing_values, nm = missing_set)

  return(rlang::inject(tibble::add_column(df, !!!injection)))
}
