
##-------------------------------------------------------------------------
##  Drop NULL columns from db query                                      --
##-------------------------------------------------------------------------

drop_unused_cols <- function(df) {
  df[colSums(
    !is.na(df)) > 0 & 
      !(names(df) %in% c("created_at", "updated_at", "export_type", "sample")
  )]
}
