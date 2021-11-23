
##--------------------------------------------------------------------------
##  Global variables                                                      --
##--------------------------------------------------------------------------

wellOrder <- purrr::map2_chr(rep(c(LETTERS[1:8]), 12), purrr::flatten_chr(purrr::map(c(1:12), rep, 8)), paste0)
uniOrder <- purrr::map2_chr(rep(c(LETTERS[1:16]), 3), purrr::flatten_chr(purrr::map(c(1:3), rep, 16)), paste0)

## TODO update for tooltip variable check
# tooltipVars <- c(
#   "buffer",
#   "Buffer_unit_value",
#   "pH", 
#   "salt",
#   "salt_mM",
#   "additive1",
#   "additive1_conc",
#   "additive1_unit",
#   "additive2",
#   "additive2_conc",
#   "additive2_unit", 
#   "DTT_mM",
#   "TCEP_mM",
#   "glycerol",
#   "albumin",
#   "albumin_mgml",
#   "comment"
# )

xvarChoices <- c(
  "Tagg @ 266nm" = "Tagg266",
  "Tagg @ 473nm" = "Tagg473",
  "Tm" = "Tm1",
  "Z Diameter (nm)" = "Z_D",
  "Peak 1 Diameter (nm)" = "peak1_D",
  "pH" = "pH_unit_value",
  "Buffer" = "Buffer_condition_name"
)

yvarChoices <- c(
  "Tagg @ 266nm" = "Tagg266",
  "Tagg @ 473nm" = "Tagg473",
  "Tm" = "Tm1",
  "Polydispersity Index" = "PdI",
  # "Z Modality" = "mode_Z",
  "pH" = "pH_unit_value",
  "Buffer" = "Buffer_condition_name"
)

colorvarChoices <- c(
  "Buffer" = "Buffer_condition_name",
  "pH" = "pH_unit_value",
  "Buffer Salt" = "BufferSalt_condition_name",
  "[Buffer Salt]" = "BufferSalt_unit_value",
  "Metal Salt" = "MetalSalt_condition_name",
  "[Metal Salt]" = "MetalSalt_unit_value",
  "Additive" = "Additive_condition_name",
  "Albumin" = "Albumin_condition_name",
  "Amino Acid" = "AminoAcid_condition_name",
  "[Amino Acid]" = "AminoAcid_unit_value",
  "Chelator" = "Chelator_condition_name",
  "ReducingAgent" = "ReducingAgent_condition_name",
  "Sugar" = "Sugar_condition_name",
  "Surfactant" = "Surfactant_condition_name",
  "[Surfactant]" = "Surfactant_unit_value",
  "Thickening Agent" = "ThickeningAgent_condition_name",
  "[Thickening Agent]" = "ThickeningAgent_unit_value",
  # "Z Modality" = "mode_Z",
  "Plate Type" = "plate",
  "Exp. ID" = "exp_id",
  "Well" = "well",
  "Instrument" = "instrument"
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
  "Correlation Function" = "specDLS_C",
  "DLS (Intensity)" = "specDLS_I",
  "DLS (Mass)" = "specDLS_M",
  "SLS 266nm" = "specSLS266",
  "SLS 473nm" = "specSLS473",
  "NanoDSF" = "specTm"
)

# Summary values corresponding to each spectra type
summary_vars <- c(# previously specDerived
  "specDLS_C" = NA_character_,
  "specDLS_I" = "Z_D",
  "specDLS_M" = "Z_D",
  "specSLS266" = "Tagg266",
  "specSLS473" = "Tagg473",
  "specTm" = "Tm1"
)

# Spectra X variables
spec_x_switch <- c(
  "specDLS_C" = "time",
  "specDLS_I" = "hydrodynamic_diameter",
  "specDLS_M" = "hydrodynamic_diameter",
  "specSLS266" = "temperature",
  "specSLS473" = "temperature",
  "specTm" = "temperature"
)

# Spectra Y variables
spec_y_switch <- c(
  "specDLS_C" = "amplitude",
  "specDLS_I" = "amplitude",
  "specDLS_M" = "amplitude",
  "specSLS266" = "sls_266",
  "specSLS473" = "sls_473",
  "specTm" = "bcm"
)

spec_tbl_list <- c(
  specDLS_C = "uncle_dls_correlations",
  specDLS_I = "uncle_dls_intensities",
  specDLS_M = "uncle_dls_masses",
  specSLS266 = "uncle_sls266s",
  specSLS473 = "uncle_sls473s",
  specTm = "uncle_dsfs"
)
