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
  "Buffer" = "buffer",
  "pH" = "pH",
  "Salt" = "salt",
  "Salt Conc. (mM)" = "salt_mM",
  "Additive 1" = "additive1",
  "Additive 1 Conc." = "additive1_conc",
  "Additive 2" = "additive2",
  "Additive 2 Conc." = "additive2_conc",
  "Glycerol" = "glycerol",
  "Activity (Endpoint)" = "act_endpoint",
  "Activity (Rate)" = "act_rate",
  "Z Modality" = "mode_Z",
  "Timecourse (hr)" = "time_uncle_hr",
  "Plate" = "plate",
  "Comment" = "comment"
)


palnList <- list(
  "Set1" = 9,
  "Set2" = 8,
  "Set3" = 12,
  "Paired" = 12,
  "Dark2" = 8,
  "Accent" = 8,
  "Spectral" = 11,
  "BrBG" = 11,
  "Default" = 0
)
palChoices <- names(palnList)

# Spectra to plot in the sparkline QuickView..
specList <- c(
  "SLS 266nm" = "specSLS266",
  "SLS 473nm" = "specSLS473",
  "NanoDSF" = "specTm",
  "Correlation Function" = "specDLS_C",
  "DLS (Intensity)" = "specDLS_I",
  "DLS (Mass)" = "specDLS_M"
)

specDerived <- c(
  "specSLS266" = "Tagg266",
  "specSLS473" = "Tagg473",
  "specTm" = "Tm1", 
  "specDLS_C" = "none",
  "specDLS_I" = "Z_D",
  "specDLS_M" = "Z_D"
)

spec_tbl_list <- c(
  specTm = "uncle_sls_bcm",
  specSLS266 = "uncle_sls_266",
  specSLS473 = "uncle_sls_473",
  specDLS_C = "uncle_dls_correlation",
  specDLS_I = "uncle_dls_intensity",
  specDLS_M = "uncle_dls_mass"
)