
##--------------------------------------------------------------------------
##  global.R                                                              --
##--------------------------------------------------------------------------

##--------------------------------------------------------
##  PACKAGES                                            --
##--------------------------------------------------------
# Shiny packages
library(shiny)
# library(shinybusy)

# Theme and widget packages
library(shinyWidgets)
library(bslib)
library(thematic)
library(showtext)
library(RColorBrewer)

# Database and support packages
library(DBI)
library(pool)
library(RPostgres)
library(bit64)

# Data manipulation
library(tidyverse)
library(forcats)

# Plotting
library(plotly)
library(DT)
library(cowplot)
library(ggridges)
library(wordcloud2)

# Programming and debugging
library(rlang)
library(glue)
# library(profvis)


##-------------------------------------------------------
##  SETUP                                              --
##-------------------------------------------------------

##----------------------------------------
##  Testing flag                        --
##----------------------------------------
use_testing_mode <- FALSE

if (use_testing_mode) {
  message("TESTING ONE, TWO, THREE. IS THIS THING ON?")
  message("The app is currently in testing mode and will not use real data.")
  test_data <- readr::read_rds("test/test_data.rds") |>
    dplyr::rename_with(
      ~"dls_temperature",
      .cols = tidyselect::any_of(c("temperature"))
    )
}


##-----------------------------------------
##  Theme                                --
##-----------------------------------------
# Reusable variables for below
google_base_font <- "Noto Sans"
google_heading_font <- "Roboto Condensed"
the_weight <- 400
# Using `font_collection` provides backup in the absence of internet connection
the_base_font <- rlang::expr(
  bslib::font_collection(
    bslib::font_google(
      google_base_font,
      local = TRUE
    ),
    "Arial",
    "sans-serif"
  )
)
the_heading_font <- rlang::expr(
  bslib::font_collection(
    bslib::font_google(
      google_heading_font,
      local = TRUE
    ),
    "Arial",
    "sans-serif"
  )
)

# App themes
theme_light <- bslib::bs_theme(
  version = 5,
  bootswatch = "cosmo",
  base_font = rlang::eval_tidy(the_base_font),
  heading_font = rlang::eval_tidy(the_heading_font),
  font_scale = 0.7,
  primary = "#586e75",
  secondary = "#eee8d5"
)
theme_dark <- bslib::bs_theme(
  version = 5,
  bootswatch = "solar",
  base_font = rlang::eval_tidy(the_base_font),
  heading_font = rlang::eval_tidy(the_heading_font),
  font_scale = 0.7
)

# Fonts for plots
sysfonts::font_add_google(google_base_font, regular.wt = the_weight)
showtext::showtext_auto()

# `ggplot2` theme defaults (also controlled by `thematic`)
ggplot2::theme_set(
  ggplot2::theme_bw(base_family = google_base_font) +
    ggplot2::theme(
      panel.grid = ggplot2::element_blank(),
      panel.border = ggplot2::element_blank(),
      axis.line = ggplot2::element_line(),
      axis.text = ggplot2::element_text(face = "bold")
    )
)

# Auto theme plots
thematic::thematic_shiny(
  # font = "auto"
  # to-do: apply global color schemes here
  # qualitative = ,
  # sequential = 
)


##-----------------------------------------
##  Database connection                  --
##-----------------------------------------
if (use_testing_mode) {
  message("Database connection will not be established in testing mode.")
  db_pool_obj <- NULL
} else {
  # Instantiate db pool
  db_pool_obj <- pool::dbPool(
    drv = RPostgres::Postgres(),
    dbname = "ebase_production",
    host = "ebase-db-c.neb.com",
    port = 5432,
    user = Sys.getenv("ebase_uid"),
    password = Sys.getenv("ebase_pwd")
  )
  message("Connection established? : ", DBI::dbIsValid(db_pool_obj))
  print(db_pool_obj)
}


##----------------------------------------
##  Connection cleanup                  --
##----------------------------------------
if (use_testing_mode) {
  shiny::onStop(function() {
    message("Thanks for testing stuff. Goodbye.")
  })
} else {
  shiny::onStop(function() {
    pool::poolClose(db_pool_obj)
    message("Connection closed? : ", !(DBI::dbIsValid(db_pool_obj)))
  })
}
##--------------------------------------------------------------------------
##  end global.R                                                          --
##--------------------------------------------------------------------------
