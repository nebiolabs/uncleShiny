source("R/vars.R")


##===============================================================
##                    Custom Color Palettes                    ==
##===============================================================

mycolors <- function(palette, n) {
  paln <- palnList[[palette]]
  if(palette == "Default") {
    return(NULL)
  } else {
    colorRampPalette(brewer.pal(paln, palette))(n)
  }
}


##================================================================
##                      Spectra Sparklines                      ==
##================================================================

ggspark <- function(dfHover, dfClick, hover, title, n, derivedList, alpha = 0.6) {
  # this grabs the summary value associated with the plotted spectra (see vars.R)
  targetHover <- derivedList[hover]

  
  if (targetHover == "none") {
    ## the "none" is for the correlation plot, which has no derived summary value
    valHover <- NA
    xval <- 0
    yval <- 0
  } else {
    valHover <- dfHover[[targetHover]][[1]]# in case two points are somehow selected by hover, take the first
    if (is.na(valHover)) {
      xval <- 0
      yval <- 0
    } else {
      xval <- valHover
      ## this is to compensate for the called value not being in the spectra plot vectors, it will match the nearest value
      # xval <- dfHover[[hover]][[1]][which(abs(dfHover[[hover]][[1]][[1]]-valHover) == min(abs(dfHover[[hover]][[1]][[1]]-valHover))), ][[1]]
      yval <- dfHover[[hover]][[1]][which(abs(dfHover[[hover]][[1]][[1]]-valHover) == min(abs(dfHover[[hover]][[1]][[1]]-valHover))), ][[2]]
    }
  }
  
  base <- ggplot(
    data = dfHover[[hover]][[1]],
    aes(
      x = dfHover[[hover]][[1]][[1]],
      y = dfHover[[hover]][[1]][[2]]
    )
  )
  
  if (!is.null(dfClick) & !is.null(dfClick[[hover]][[1]])) {
    base <- base +
      geom_area(
        data = dfClick[[hover]][[1]],
        aes(
          x = dfClick[[hover]][[1]][[1]],
          y = dfClick[[hover]][[1]][[2]]
        ),
        fill = "black", alpha = 0.3
      ) +
      geom_line(
        data = dfClick[[hover]][[1]],
        aes(
          x = dfClick[[hover]][[1]][[1]],
          y = dfClick[[hover]][[1]][[2]]
        ),
        color = "black", alpha = 0.4
      )
  }
  
  p <- base +
    geom_area(fill = colors[n], alpha = alpha) +
    geom_line(color = "black", alpha = alpha) +
    geom_vline(xintercept = xval, color = "red", linetype = "dashed") +
    geom_point(aes(x = xval, y = yval), color = "red") +
    annotate("text", x = xval, y = Inf, hjust = 1, vjust = 1, label = targetHover) +
    ggtitle(title) +
    sparklineTheme()
  
  if (grepl("DLS_I|DLS_M", hover, perl = TRUE) & !is.null(dfHover[[hover]][[1]]) & (is.null(dfClick) | !is.null(dfClick[[hover]][[1]]))) {
    p <- p +
      annotate("rect", xmin = 1, xmax = 5, ymin = 0, ymax = Inf, fill = "orange", alpha = 0.15) +
      annotate("rect", xmin = 20, xmax = Inf, ymin = 0, ymax = Inf, fill = "red", alpha = 0.15) +
      geom_vline(xintercept = 5, linetype = "dotted", alpha = 0.5) +
      geom_vline(xintercept = 20, linetype = "dotted", alpha = 0.5) +
      scale_x_log10(limits = c(1, 100000), breaks = c(1, 5, 10, 100, 1000), labels = scales::label_comma(accuracy = 1)) +
      annotation_logticks(sides = "b", alpha = 0.5)
  }
  
  if (grepl("DLS_C", hover, perl = TRUE) & !is.null(dfHover[[hover]][[1]]) & (is.null(dfClick) | !is.null(dfClick[[hover]][[1]]))) {
    p <- p +
      scale_x_log10() +
      annotation_logticks(sides = "b", alpha = 0.5)
  }
  
  
  
  return(p)
}


##================================================================
##                      Postgres Functions                      ==
##================================================================

##:::::::::::::::::::::::::
##  Parsing float8 type  ::
##:::::::::::::::::::::::::

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


##:::::::::::::::::::
##  Trimming data  ::
##:::::::::::::::::::

drop_unused <- function(df) {
  df[colSums(!is.na(df)) > 0 & !(names(df) %in% c("created_at", "updated_at", "export_type", "sample"))]
}


##:::::::::::::
##  Parsers  ::
##:::::::::::::

parse_summary <- function(df) {
  if ("sls_266" %in% names(df)) {
    df |> 
      dplyr::filter(export_type == "sum") |> 
      dplyr::rename(sls_id = id) |> 
      drop_unused()
  } else {
    df |> 
      dplyr::filter(export_type == "sum") |> 
      dplyr::rename(dls_id = id, specDLS_C_residuals = residuals) |> 
      dplyr::mutate(
        specDLS_C_residuals = purrr::map(specDLS_C_residuals, parse_float8)
      ) |> 
      drop_unused()
  }
}

parse_bundle <- function(df) {
  dls_vars_to_nest <- rlang::expr(c("time", "hydrodynamic_diameter", "amplitude", "data_id"))

  df_dropped <- df |>
  dplyr::filter(export_type == "bundle") |>
  dplyr::rename(data_id = id) |>
  drop_unused()

  if ("sls_266" %in% names(df_dropped)) {
    df_nested <- df_dropped |>
    tidyr::nest(
      specTm = c("temperature", "bcm", "data_id"),
      specSLS266 = c("temperature", "sls_266", "data_id"),
      specSLS473 = c("temperature", "sls_473", "data_id")
    )
  } else {
    df_nested <- df_dropped |>
    dplyr::select(-any_of(c("residuals"))) |>
    tidyr::nest(
      spec_data = tidyselect::any_of(!!dls_vars_to_nest)
    ) |>
    dplyr::mutate(
      spec_data = purrr::map(
        spec_data,
        \(df) df[colSums(!is.na(df)) > 0 & names(df) != "id"]
      )
    ) |>
    tidyr::pivot_wider(
      id_cols = c(uncle_experiment_id, well),
      names_from = dls_data_type,
      values_from = spec_data
    ) |>
    dplyr::rename(
      specDLS_I = intensity,
      specDLS_M = mass,
      specDLS_C = correlation
    )
  }
  return(df_nested)
}

join_sls_dls <- function(sls, dls) {
  join_vars <- c("uncle_experiment_id", "well")
  purrr::reduce(
    list(parse_summary(sls), parse_bundle(sls), parse_summary(dls), parse_bundle(dls)),
    dplyr::full_join,
    by = join_vars
  )
}




##================================================================
##                       Banner Functions                       ==
##================================================================

# mainBanner <- function(x, y) {bannerCommenter::banner(x, y, emph = TRUE, upper = FALSE, leftSideHashes = 4, rightSideHashes = 4, numLines = 4)}
# secBanner <- function(x) {bannerCommenter::banner(x, emph = FALSE, upper = FALSE, bandChar = "=")}
# subSecBanner <- function(x) {bannerCommenter::banner(x, emph = FALSE, upper = FALSE, bandChar = ":", numLines = 1, snug = TRUE)}


