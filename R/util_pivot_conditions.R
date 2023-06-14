
##--------------------------------------------------------------------------
##  Pivot conditions and units                                            --
##--------------------------------------------------------------------------

pivot_conditions <- function(df, ...) {
  df |> 
    dplyr::filter(included == TRUE) |> 
    dplyr::select(!c(included, condition_id, grouping_id)) |> 
    dplyr::mutate(
      condition = glue::glue(
        "{unit_value}{unit_name} {condition_name}"
      ),
      .keep = "unused"
    ) |> 
    dplyr::summarize(
      condition = stringr::str_c(condition, collapse = ", "),
      .by = c(well_id, group_id, group_name)
    ) |> 
    tidyr::pivot_wider(
      id_cols = well_id,
      names_from = group_name,
      values_from = condition,
      ...
    )
}
