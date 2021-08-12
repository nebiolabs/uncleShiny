
##--------------------------------------------------------------------------
##  Database querying function                                            --
##--------------------------------------------------------------------------

getQuery <- function(connection, query_string, user_input = NULL) {
  DBI::dbGetQuery(
    connection,
    glue::glue_sql(
      query_string,
      input = user_input,
      .con = connection
    )
  )
}