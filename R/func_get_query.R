
##--------------------------------------------------------------------------
##  Database querying function                                            --
##--------------------------------------------------------------------------

get_query <- function(connection, query_string, ...) {
require(rlang)
  args <- list(query_string, ..., .con = connection)
  DBI::dbGetQuery(
    connection,
    do.call(glue::glue_sql, args)
  )
}
