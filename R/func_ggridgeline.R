
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
}
