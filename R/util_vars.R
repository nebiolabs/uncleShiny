
##--------------------------------------------------------------------------
##  Global variables                                                      --
##--------------------------------------------------------------------------


##--------------------------------------------------------
##  GENERAL                                             --
##--------------------------------------------------------
wellOrder <- purrr::map2_chr(rep(c(LETTERS[1:8]), 12), purrr::flatten_chr(purrr::map(c(1:12), rep, 8)), paste0)
uniOrder <- purrr::map2_chr(rep(c(LETTERS[1:16]), 3), purrr::flatten_chr(purrr::map(c(1:3), rep, 16)), paste0)

character_conversions <- c(
  "exp_set_id",
  "exp_id",
  "uncle_summary_id",
  "instrument",
  "well_id"
)

excluded_columns <- c(
  "residuals",
  "experiment_condition_id",
  "condition_id",
  "unit_id"
)


##--------------------------------------------------------
##  PLOT OPTIONS                                        --
##--------------------------------------------------------
xvarChoices <- c(
  "Tagg @ 266nm" = "Tagg266",
  "Tagg @ 473nm" = "Tagg473",
  "Tm" = "Tm1",
  "Z Diameter (nm)" = "Z_D",
  "Peak 1 Diameter (nm)" = "peak1_D",
  "pH" = "pH",
  "Buffer" = "Buffer"
)

yvarChoices <- c(
  "Tagg @ 266nm" = "Tagg266",
  "Tagg @ 473nm" = "Tagg473",
  "Tm" = "Tm1",
  "Polydispersity Index" = "PdI",
  "DLS Modality" = "dls_mode",
  "pH" = "pH",
  "Buffer" = "Buffer"
)

colorvarChoices <- c(
  "Buffer" = "Buffer",
  "pH" = "pH",
  "Buffer Salt" = "BufferSalt",
  "Metal Salt" = "MetalSalt",
  "Additive" = "Additive",
  "Albumin" = "Albumin",
  "Amino Acid" = "AminoAcid",
  "Chelator" = "Chelator",
  "ReducingAgent" = "ReducingAgent",
  "Sugar" = "Sugar",
  "Surfactant" = "Surfactant",
  "Thickening Agent" = "ThickeningAgent",
  # "Z Modality" = "mode_Z",
  "Plate Type" = "plate",
  "Exp. Set ID" = "exp_set_id",
  "Notes" = "notes",
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


##--------------------------------------------------------
##  FILTER OPTIONS                                      --
##--------------------------------------------------------

##----------------------------------------
##  Conditions                          --
##----------------------------------------
condition_filters_list <- c(
  "Buffer" = "Buffer",
  "pH" = "pH",
  "Buffer Salt" = "BufferSalt",
  "Reducing Agent" = "ReducingAgent",
  "Sugar" = "Sugar",
  "Plate Type" = "plate",
  # "Exp. Set ID" = "exp_set_id",
  "Notes" = "notes"
  # "Exp. ID" = "exp_id",
  # "Well" = "well",
  # "Instrument" = "instrument"
)


##-----------------------------------------
##  Numeric                              --
##-----------------------------------------
numeric_filters_list <- c(
  "Tm" = "Tm1",
  "Tagg @ 266nm" = "Tagg266",
  "Tagg @ 473nm" = "Tagg473",
  "Polydispersity Index" = "PdI",
  "Z Diameter (nm)" = "Z_D",
  "Peak 1 Diameter (nm)" = "peak1_D",
  "DLS Modality" = "dls_mode"
)



##-------------------------------------------------------
##  SPECTRA                                            --
##-------------------------------------------------------
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


##--------------------------------------------------------
##  TOOLTIPS                                            --
##--------------------------------------------------------
# Expression for generating hover tooltip
short_tooltip_glue_string <- quote(
  glue::glue(
    "<em>Plate Well: {well}</em><br>",
    "<b>Buffer:</b> {Buffer}, pH {pH}<br>",
    "<b>Salt:</b> {`BufferSalt`}<br>"
  )
)

# Expression for generating conditions module summary
long_tooltip_glue_string <- quote(
  glue::glue(
    "<h4><em>Plate Well: {well}</em></h4>",
    "<b>Buffer:</b> {Buffer}, pH {pH}<br>",
    "<b>Salt:</b> {`BufferSalt`}<br>",
    "<br>",
    "<b>Additives:</b><br>",
    "    <em>Metal:</em> {`MetalSalt`}<br>",
    "    <em>Amino Acid:</em> {`AminoAcid`}<br>",
    "    <em>Sugar:</em> {`Sugar`}<br>",
    "    <em>Surfactant:</em> {`Surfactant`}<br>",
    "    <em>Albumin:</em> {`Albumin`}<br>",
    "    <em>Chelator:</em> {`Chelator`}<br>",
    "    <em>Reducing Agent:</em> {`ReducingAgent`}<br>",
    "    <em>Thickener:</em> {`ThickeningAgent`}<br>",
    "    <em>General:</em> {`Additive`}<br>",
    "    <em>Chaperone:</em> {`Chaperone`}<br>",
  )
)
