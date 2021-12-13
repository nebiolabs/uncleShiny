
##-------------------------------------------------------------------------
##  Plot spectra ridgelines                                              --
##-------------------------------------------------------------------------

ggridgeline <- function(data, spec_type, dls_type = "intensity", 
                        sort_var, color_var, palette_name = "Set2",
                        show_legend = TRUE, alpha = 0.6) {
  if (!(spec_type %in% c("dls", "corr", "sls", "dsf"))) {
    stop("Warning: invalid spec_type; use 'dls', 'corr', 'sls' or 'dsf'.")
  }
  if (!(dls_type %in% c("intensity", "mass"))) {
    stop("Warning: invalid dls_type; use 'intensity' or 'mass'.")
  }
  if (is.null(data)) {
    stop("Nothing is selected.")
  }
  
  ##----------------------------------------
  ##  Variable switching                  --
  ##----------------------------------------
  spec_switch <- list(
    "dsf" = "specTm",
    "sls" = list("specSLS266", "specSLS473"),
    "dls" = list("specDLS_I", "specDLS_M"),
    "corr" = "specDLS_C"
  )
  x_switch <- list(
    "dsf" = "temperature",
    "sls" = "temperature",
    "dls" = "hydrodynamic_diameter",
    "corr" = "time"
  )
  y_switch <- list(
    "dsf" = "bcm",
    "sls" = list("sls_266", "sls_473"),
    "dls" = "amplitude",
    "corr" = "amplitude"
  )
  summary_switch <- list(
    "dsf" = "Tm1",
    "sls" = list("Tagg266", "Tagg473"),
    "dls" = "Z_D",
    "corr" = NA_character_
  )
  spec_var <- do.call(switch, c(spec_type, spec_switch))
  x_var <- do.call(switch, c(spec_type, x_switch))
  y_var <- do.call(switch, c(spec_type, y_switch))
  summary_var <- do.call(switch, c(spec_type, summary_switch))
  
  ##----------------------------------------
  ##  Data factor manipulation            --
  ##----------------------------------------
  plot_data <- data |> 
    # dplyr::mutate(well_id = bit64::as.character.integer64(well_id)) |>
    dplyr::mutate(well_id = forcats::fct_reorder(well_id, .data[[sort_var]]))
  
  
  ##----------------------------------------
  ##  Ridgeline formatting                --
  ##----------------------------------------
  ridgelineTheme <- function() {
    list(
      ggplot2::theme_minimal(base_family = google_base_font),
      ggplot2::theme(
        panel.grid = ggplot2::element_blank(),
        strip.text.y.left = element_text(size = 14, angle = 0, vjust = 0.2),
        strip.text.y.right = element_text(size = 14, angle = 0, vjust = 0.2),
        axis.title = ggplot2::element_text(hjust = 0.95, size = 15),
        axis.text.x = ggplot2::element_text(size = 11),
        axis.text.y = ggplot2::element_blank(),
        axis.line.x.bottom = ggplot2::element_line(),
        plot.margin = ggplot2::margin(0.1,1,0.1,0.1, "cm"),
        legend.position = "left",
        legend.justification = "top"
      )
    )
  }
  
  ##----------------------------------------
  ##  Facet labeller                      --
  ##----------------------------------------
  custom_labeller <- function(facet_var) {
    switch_list <- as.list(rlang::set_names(
      glue::glue_data(
        plot_data,
        "{well}_{plate}({exp_set_id})",
      ),
      nm = plot_data[["well_id"]]
    ))
    rlang::inject(dplyr::recode(facet_var, !!!switch_list))
  }
  
  ##----------------------------------------
  ##  Ridgeline plot                      --
  ##----------------------------------------
  p <- ggplot2::ggplot(data = plot_data) + ridgelineTheme()
  
  ##-----------------------
  ##  DLS                --
  ##-----------------------
  if (spec_type == "dls" & dls_type == "intensity") {
    dls_var = spec_var[[1]]
  } else if (spec_type == "dls" & dls_type == "mass") {
    dls_var = spec_var[[2]]
  } else {
    dls_var = spec_switch[["dls"]][[1]]
  }
  if (spec_type == "dls") {
    dls_data <- unnest(plot_data, tidyselect::all_of(dls_var))
    p <- p +
      ggplot2::geom_area(
        data = dls_data,
        ggplot2::aes(
          x = .data[[x_var]],
          y = .data[[y_var]],
          fill = .data[[color_var]]
        ),
        show.legend = show_legend
      ) +
      ggplot2::geom_line(
        data = dls_data,
        ggplot2::aes(
          x = .data[[x_var]],
          y = .data[[y_var]]
        ),
        color = "black",
        size = 1,
        show.legend = FALSE
      ) +
      ggplot2::geom_vline(
        data = plot_data,
        ggplot2::aes(xintercept = .data[[summary_var]]),
        color = "red"
      ) +
      ggplot2::facet_grid(
        rows = vars(well_id),
        labeller = ggplot2::as_labeller(custom_labeller),
        switch = "y"
      ) +
      ggplot2::scale_x_log10(limits = c(1, 1000), expand = c(0,0)) +
      ggplot2::scale_y_continuous(expand = c(0, 0.1)) +
      ggplot2::annotation_logticks(sides = "b") +
      ggplot2::scale_fill_manual(
        values = make_palette(palette_name, length(unique(plot_data[[color_var]])))
      ) +
      ggplot2::labs(
        title = glue::glue("DLS ({dls_type} distribution)"),
        subtitle = glue::glue("with {summary_var} overlay in red"),
        y = "signal intenisty",
        x = "average hydrodynamic diameter (nm)"
      )
  }
  
  ##-----------------------
  ##  Correlation        --
  ##-----------------------
  if (spec_type == "corr") {
    corr_data <- tidyr::unnest(plot_data, tidyselect::all_of(spec_var))
    p <- p +
      ggplot2::geom_line(
        data = corr_data,
        ggplot2::aes(
          x = .data[[x_var]],
          y = .data[[y_var]],
          color = .data[[color_var]]
        ),
        size = 1,
        alpha = alpha,
        show.legend = show_legend
      ) +
      ggplot2::geom_line(
        data = corr_data,
        ggplot2::aes(
          x = .data[[x_var]],
          y = .data[[y_var]],
          color = .data[[color_var]]
        ),
        stat = "smooth",
        linetype = "dashed",
        size = 1,
        # alpha = alpha,
        show.legend = show_legend
      ) +
      ggplot2::facet_grid(
        rows = vars(well_id),
        labeller = ggplot2::as_labeller(custom_labeller)
      ) +
      ggplot2::scale_x_log10(limits = c(0.000001, 0.1), expand = c(0,0)) +
      ggplot2::scale_y_continuous(expand = c(0, 0.1)) +
      ggplot2::annotation_logticks(sides = "b") +
      ggplot2::scale_color_manual(
        values = make_palette(palette_name, length(unique(plot_data[[color_var]])))
      ) +
      ggplot2::theme(
        axis.title.y = ggplot2::element_blank()
      ) +
      ggplot2::labs(
        title = glue::glue("DLS (correlation function)"),
        subtitle = glue::glue("no overlay available"),
        x = "time (s)",
        
      )
  }
  
  ##-----------------------
  ##  SLS                --
  ##-----------------------
  if (spec_type == "sls") {
    p <- p +
      ggplot2::geom_line(
        data = tidyr::unnest(plot_data, tidyselect::all_of(spec_var[[1]])),
        ggplot2::aes(
          x = .data[[x_var]],
          y = .data[[y_var[[1]]]],
          color = .data[[color_var]]
        ),
        size = 1,
        show.legend = show_legend
      ) +
      ggplot2::geom_line(
        data = tidyr::unnest(plot_data, tidyselect::all_of(spec_var[[2]])),
        ggplot2::aes(
          x = .data[[x_var]],
          y = .data[[y_var[[2]]]],
          color = .data[[color_var]]
        ),
        linetype = "dashed",
        size = 1,
        alpha = alpha,
        show.legend = show_legend
      ) +
      ggplot2::geom_vline(
        data = plot_data,
        ggplot2::aes(xintercept = .data[[summary_var[[1]]]]),
        color = "red"
      ) +
      ggplot2::geom_vline(
        data = plot_data,
        ggplot2::aes(xintercept = .data[[summary_var[[2]]]]),
        color = "red",
        linetype = "dashed",
        size = 1,
        alpha = alpha
      ) +
      ggplot2::facet_grid(
        rows = vars(well_id),
        labeller = ggplot2::as_labeller(custom_labeller),
        switch = "y"
      ) +
      ggplot2::scale_y_continuous(expand = c(0, 0.1)) +
      ggplot2::scale_color_manual(
        values = make_palette(palette_name, length(unique(plot_data[[color_var]])))
      ) +
      ggplot2::labs(
        title = glue::glue("SLS ({spec_var[[1]]} solid, {spec_var[[2]]} dashed)"),
        subtitle = glue::glue("with {summary_var[[1]]} overlays in red"),
        y = "light scattering intensity @ \u03bb",
        x = "temperature (°C)"
      )
  }
  
  ##-----------------------
  ##  NanoDSF            --
  ##-----------------------
  if (spec_type == "dsf") {
    p <- p +
      ggplot2::geom_line(
        data = tidyr::unnest(plot_data, tidyselect::all_of(spec_var)),
        ggplot2::aes(
          x = .data[[x_var]],
          y = .data[[y_var]],
          color = .data[[color_var]]
        ),
        size = 1,
        show.legend = show_legend
      ) +
      ggplot2::geom_vline(
        data = plot_data,
        ggplot2::aes(xintercept = .data[[summary_var]]),
        color = "red"
      ) +
      ggplot2::facet_grid(
        rows = vars(well_id),
        labeller = ggplot2::as_labeller(custom_labeller)
      ) +
      ggplot2::scale_y_continuous(expand = c(0, 0.1)) +
      ggplot2::scale_color_manual(
        values = make_palette(palette_name, length(unique(plot_data[[color_var]])))
      ) +
      ggplot2::labs(
        title = glue::glue("NanoDSF"),
        subtitle = glue::glue("with {summary_var} overlays in red"),
        y = "330/350nm fluorescence barycentric mean",
        x = "temperature (°C)"
      )
  }
  
  return(p)
}
