get_sparkline_vars <- function(df) {
  source("R/util_vars.R", local = TRUE)
  spec_list <- spec_vars
  summary_list <- summary_vars
  
  filtered_spec_list <- spec_list[spec_list %in% colnames(df)]
  
  separated_list <- NULL
  
  separated_list$spec_vars <- as.vector(filtered_spec_list)
  separated_list$spec_names <- names(filtered_spec_list)
  separated_list$x_vars <- as.vector(unlist(lapply(
    filtered_spec_list,
    function(x) do.call(switch, as.list(c(x, spec_x_switch)))
  )))
  separated_list$y_vars <- as.vector(unlist(lapply(
    filtered_spec_list,
    function(y) do.call(switch, as.list(c(y, spec_y_switch)))
  )))
  
  separated_list$summary_vars <- as.vector(summary_list[order(match(
    summary_list[names(summary_list) %in% filtered_spec_list],
    filtered_spec_list
  ))])
  
  separated_list$n <- seq_along(filtered_spec_list)
  
  return(separated_list)
}