
##--------------------------------------------------------------------------
##  global.R                                                              --
##--------------------------------------------------------------------------

##--------------------------------------------------------
##  Packages                                            --
##--------------------------------------------------------

# Shiny packages
library(shiny)
library(shinybusy)

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
library(glue)

# Plotting
library(plotly)
library(DT)

# Programming and debugging
library(rlang)
# library(profvis)


##-------------------------------------------------------
##  Setup                                              --
##-------------------------------------------------------

# Reusable variables for below
the_base_font <- "Roboto Condensed"
the_heading_font <- "Oswald"
the_weight <- 300
google_base_font <- rlang::expr(bslib::font_google(
  the_base_font,
  local = TRUE
))
google_heading_font <- rlang::expr(bslib::font_google(
  the_heading_font,
  local = TRUE
))

# App themes
theme_light <- bslib::bs_theme(
  version = 4,
  bootswatch = "flatly",
  base_font = rlang::eval_tidy(google_base_font),
  heading_font = rlang::eval_tidy(google_heading_font),
  font_scale = 1.05
)
theme_dark <- bslib::bs_theme(
  version = 4,
  bootswatch = "solar",
  base_font = rlang::eval_tidy(google_base_font),
  heading_font = rlang::eval_tidy(google_heading_font)
)

# Fonts for plots
sysfonts::font_add_google(the_base_font, regular.wt = the_weight)
showtext::showtext_auto()

# `ggplot2` theme defaults (also controlled by `thematic`)
theme_set(
  theme_bw() +
    theme(
      panel.grid = element_blank(),
      panel.border = element_blank(),
      axis.line = element_line(),
      axis.text = element_text(face = "bold")
    )
)

# Auto theme plots
thematic::thematic_shiny(
  # font = "auto"
  # to-do: apply global color schemes here
  # qualitative = ,
  # sequential = 
)


##--------------------------------------------------------
##  Postgres database connection                        --
##--------------------------------------------------------

# Instantiate db pool
ebase_dev <- pool::dbPool(
  drv = RPostgres::Postgres(),
  dbname = "ebase_dev",
  host = "ebase-db-c.neb.com",
  port = 5432,
  user = Sys.getenv("ebase_uid"),
  password = Sys.getenv("ebase_pwd")
)
message("Connection established? : ", DBI::dbIsValid(ebase_dev))
print(ebase_dev)


##--------------------------------------------------------
##  Connection cleanup                                  --
##--------------------------------------------------------

onStop(function() {
  pool::poolClose(ebase_dev)
  message("Connection closed? : ", !(DBI::dbIsValid(ebase_dev)))
})
