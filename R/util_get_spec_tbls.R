
##--------------------------------------------------------------------------
##  Spectra table collection                                              --
##--------------------------------------------------------------------------

get_spec_tbls <- function(source, tbl_list, filter) {
  library(rlang)
  purrr::imap(
    tbl_list,
    function(t, n) {
      var_name <- rlang::ensym(n)
      DBI::dbGetQuery(
        source,
        glue::glue_sql(
          "SELECT *
          FROM {`t`}
          WHERE {`t`}.uncle_summary_id IN ({filter*})",
          .con = source,
          filter = filter
        )
      ) |>
        dplyr::select(-tidyselect::any_of(c(
          "id", "created_at", "updated_at"
        ))) |>
        tidyr::nest(!!var_name := -c(uncle_summary_id))
    }
  ) |>
    rlang::set_names(nm = names(tbl_list))
}