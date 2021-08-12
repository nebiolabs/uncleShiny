
##-------------------------------------------------------------------------
##  Postgres data manipulations                                          --
##-------------------------------------------------------------------------


##-------------------------------------------------------
##  Retrieving spectra data                            --
##-------------------------------------------------------

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
        .con = source
        )
      ) |>
        dplyr::select(-c(id)) |>
        tidyr::nest(!!var_name := -c(uncle_summary_id))
    }
  ) |>
    rlang::set_names(nm = names(tbl_list))
}


##--------------------------------------------------------
##  Parsing float8 strings                              --
##--------------------------------------------------------

parse_float8 <- function(s) {
  readr::parse_double(
    purrr::simplify(
      stringr::str_split(
        stringr::str_replace_all(s, "(^\\{|\\}$)", ""),
        pattern = ","
      )
    )
  )
}


##-------------------------------------------------------
##  Creating nested data frames                        --
##-------------------------------------------------------

nest_spectra <- function(summary, spec_tbls) {
  join_list <- c(list(summary = summary), spec_tbls)
  # join_list <- purrr::map(
  #   join_list,
  #   \(df) dplyr::mutate(df, dplyr::across(c(uncle_summary_id), .funs = bit64::as.character.integer64))
  # )
  join_vars <- c("uncle_summary_id")
  purrr::reduce(
    join_list,
    dplyr::left_join,
    by = join_vars
  )
}

##-------------------------------------------------------
##  Trimming data                                      --
##-------------------------------------------------------

drop_unused <- function(df) {
  df[colSums(!is.na(df)) > 0 & !(names(df) %in% c("created_at", "updated_at", "export_type", "sample"))]
}



##--------------------------------------------------------
##  Deprecated                                          --
##--------------------------------------------------------

# parse_summary <- function(df) {
#   if ("sls_266" %in% names(df)) {
#     df |> 
#       dplyr::filter(export_type == "sum") |> 
#       dplyr::rename(sls_id = id) |> 
#       drop_unused()
#   } else {
#     df |> 
#       dplyr::filter(export_type == "sum") |> 
#       dplyr::rename(dls_id = id, specDLS_C_residuals = residuals) |> 
#       dplyr::mutate(
#         specDLS_C_residuals = purrr::map(specDLS_C_residuals, parse_float8)
#       ) |> 
#       drop_unused()
#   }
# }

# parse_bundle <- function(df) {
#   dls_vars_to_nest <- rlang::expr(c("time", "hydrodynamic_diameter", "amplitude", "data_id"))
# 
#   df_dropped <- df |>
#   dplyr::filter(export_type == "bundle") |>
#   dplyr::rename(data_id = id) |>
#   drop_unused()
# 
#   if ("sls_266" %in% names(df_dropped)) {
#     df_nested <- df_dropped |>
#     tidyr::nest(
#       specTm = c("temperature", "bcm", "data_id"),
#       specSLS266 = c("temperature", "sls_266", "data_id"),
#       specSLS473 = c("temperature", "sls_473", "data_id")
#     )
#   } else {
#     df_nested <- df_dropped |>
#     dplyr::select(-any_of(c("residuals"))) |>
#     tidyr::nest(
#       spec_data = tidyselect::any_of(!!dls_vars_to_nest)
#     ) |>
#     dplyr::mutate(
#       spec_data = purrr::map(
#         spec_data,
#         \(df) df[colSums(!is.na(df)) > 0 & names(df) != "id"]
#       )
#     ) |>
#     tidyr::pivot_wider(
#       id_cols = c(uncle_experiment_id, well),
#       names_from = dls_data_type,
#       values_from = spec_data
#     ) |>
#     dplyr::rename(
#       specDLS_I = intensity,
#       specDLS_M = mass,
#       specDLS_C = correlation
#     )
#   }
#   return(df_nested)
# }