
##--------------------------------------------------------------------------
##  Pivot conditions and units                                            --
##--------------------------------------------------------------------------

pivot_conditions <- function(df) {
  tidyr::pivot_wider(
    data = df[df$included == 1, ],
    id_cols = well_id,
    names_from = group_name,
    values_from = c(condition_name, unit_value, unit_name),
    names_glue = "{group_name}_{.value}"
  )
}
