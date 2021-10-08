
##-------------------------------------------------------------------------
##  Plot spectra ridgelines                                              --
##-------------------------------------------------------------------------

ggridgeline <- function(data, spec_type, dls_type = "intensity", 
                        facet_var, color_var, palette_name = "Set2",
                        show_legend = TRUE, alpha = 0.8) {
  # source("R/util_vars.R", local = TRUE)
  if (!(spec_type %in% c("dls", "corr", "sls", "dsf"))) {
    stop("Warning: invalid spec_type; use 'dls', 'corr', 'sls' or 'dsf'.")
  }
  if (!(dls_type %in% c("intensity", "mass"))) {
    stop("Warning: invalid dls_type; use 'intensity' or 'mass'.")
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
}
