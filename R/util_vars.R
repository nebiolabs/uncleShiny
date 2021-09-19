
##--------------------------------------------------------------------------
##  Global variables                                                      --
##--------------------------------------------------------------------------

wellOrder <- purrr::map2_chr(rep(c(LETTERS[1:8]), 12), purrr::flatten_chr(purrr::map(c(1:12), rep, 8)), paste0)
uniOrder <- purrr::map2_chr(rep(c(LETTERS[1:16]), 3), purrr::flatten_chr(purrr::map(c(1:3), rep, 16)), paste0)

tooltipVars <- c(
  "buffer",
  "buffer_mM",
  "pH", 
  "salt",
  "salt_mM",
  "additive1",
  "additive1_conc",
  "additive1_unit",
  "additive2",
  "additive2_conc",
  "additive2_unit", 
  "DTT_mM",
  "TCEP_mM",
  "glycerol",
  "albumin",
  "albumin_mgml",
  "comment"
)

xvarChoices <- c(
  "Tagg @ 266nm" = "Tagg266",
  "Tagg @ 473nm" = "Tagg473",
  "Tm" = "Tm1",
  "Z Diameter (nm)" = "Z_D",
  "Peak 1 Diameter (nm)" = "peak1_D",
  "Activity (Rate)" = "act_rate",
  "pH" = "pH",
  "Salt Conc. (mM)" = "salt_mM",
  "Additive 1 Conc." = "additive1_conc",
  "Additive 2 Conc." = "additive2_conc",
  "DTT (mM)" = "DTT_mM",
  "TCEP (mM)" = "TCEP_mM",
  "Glycerol (%)" = "glycerol",
  "Albumin (mg/mL)" = "albumin_mgml"
)

yvarChoices <- c(
  "Tagg @ 266nm" = "Tagg266",
  "Tagg @ 473nm" = "Tagg473",
  "Tm" = "Tm1",
  "Polydispersity Index" = "PdI",
  "Z Modality" = "mode_Z",
  "Activity (Endpoint)" = "act_endpoint",
  "Activity (Rate)" = "act_rate",
  "Buffer" = "buffer",
  "pH" = "pH",
  "Salt" = "salt",
  "Additive 1" = "additive1",
  "Additive 2" = "additive2",
  "DTT (mM)" = "DTT_mM",
  "TCEP (mM)" = "TCEP_mM",
  "Glycerol (%)" = "glycerol",
  "Albumin (mg/mL)" = "albumin_mgml"
)

colorvarChoices <- c(
  "Buffer" = "Buffer",
  "pH" = "pH_pH",
  "Buffer Salt" = "Buffer Salt",
  "[Buffer Salt](mM)" = "Buffer Salt_mM",
  "Metal Sal" = "Metal Salt",
  "[Metal Salt](mM)" = "Metal Salt_mM",
  # "Additive 1" = "additive1",
  # "Additive 1 Conc." = "additive1_conc",
  # "Additive 2" = "additive2",
  # "Additive 2 Conc." = "additive2_conc",
  "Glycerol" = "Glycerol",
  "Amino Acid" = "Amino Acid",
  "[Amino Acid](mM)" = "Amino Acid_mM",
  # "Z Modality" = "mode_Z",
  "Exp. Type" = "exp_type",
  "Well" = "well"
)


palnList <- list(
  "Set1" = 9,
  "Set2" = 8,
  "Dark2" = 8,
  "Set3" = 12,
  "Pastel1" = 9,
  "Pastel2" = 8,
  "Accent" = 8,
  "Paired" = 12,
  "Spectral" = 11,
  "BrBG" = 11,
  "YlGnBu" = 9,
  "YlGn" = 9,
  "RdPu" = 9,
  "PuRd" = 9,
  "GnBu" = 9,
  "BuPu" = 9
)
palChoices <- names(palnList)

# Spectra to plot in the sparkline QuickView..
spec_vars <- c(# previously specList
  "NanoDSF" = "specTm",
  "SLS 266nm" = "specSLS266",
  "SLS 473nm" = "specSLS473",
  "DLS (Intensity)" = "specDLS_I",
  "DLS (Mass)" = "specDLS_M",
  "Correlation Function" = "specDLS_C"
)

# Summary values corresponding to each spectra type
summary_vars <- c(# previously specDerived
  "specTm" = "Tm1",
  "specSLS266" = "Tagg266",
  "specSLS473" = "Tagg473",
  "specDLS_I" = "Z_D",
  "specDLS_M" = "Z_D",
  "specDLS_C" = NA_character_
)

# Spectra X variables
spec_x_switch <- c(
  "specTm" = "temperature",
  "specSLS266" = "temperature",
  "specSLS473" = "temperature",
  "specDLS_I" = "hydrodynamic_diameter",
  "specDLS_M" = "hydrodynamic_diameter",
  "specDLS_C" = "time"
)

# Spectra Y variables
spec_y_switch <- c(
  "specTm" = "bcm",
  "specSLS266" = "sls_266",
  "specSLS473" = "sls_473",
  "specDLS_I" = "amplitude",
  "specDLS_M" = "amplitude",
  "specDLS_C" = "amplitude"
)

spec_tbl_list <- c(
  specTm = "uncle_dsfs",
  specSLS266 = "uncle_sls266s",
  specSLS473 = "uncle_sls473s",
  specDLS_C = "uncle_dls_correlations",
  specDLS_I = "uncle_dls_intensities",
  specDLS_M = "uncle_dls_masses"
)
