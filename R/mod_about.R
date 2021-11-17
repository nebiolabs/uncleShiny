
##--------------------------------------------------------------------------
##  mod_about.R - 'about the app' module                                  --
##--------------------------------------------------------------------------

##-------------------------------------------------------
##  UI COMPONENTS                                      --
##-------------------------------------------------------
aboutUI <- function(id) {
  ns <- shiny::NS(id)
  
  # Please keep sensitive internal information separate when hosting.
  source("data/mod_about_internal.R", local = TRUE)
  
  shiny::tagList(
    shiny::fluidRow(
      about_internal,
      ##-----------------------------------------
      ##  Source code                          --
      ##-----------------------------------------
      shiny::column(
        width = 4,
        shiny::h2(
          shiny::div(
            style = "display: inline-block",
            shiny::icon("code-branch")
          ),
          shiny::div(
            style = "display: inline-block",
            "Meet the (open)source code:"
          )
        ),
        shiny::tags$em(
          "The source code for this app and for the automated parsing of
          Uncle instrument binary files is open source and freely available
          on GitHub under the GNU Affero General Public License (AGPLv3)."
        ),
        ##-----------------------
        ##  unclePy            --
        ##-----------------------
        shiny::headerPanel(""), # just using as a vertical separator
        shiny::fluidRow(
          shiny::tags$a(
            href = "https://github.com/eric-hunt/unclePy",
            target = "_blank",
            shiny::tags$h3("unclePy")
          ),
          shiny::br(),
          shiny::br(),
          shiny::tags$p(
            shiny::tags$span(shiny::tags$em("unclePy ")),
            "is the backed parser which extracts meaningful experimental
            results and metdata from Uncle instrument binary files (.uni files),
            and delivers that data in a structured format to a locally hosted
            relational database built on PostgreSQL."
          ),
          shiny::br(),
          shiny::br(),
          shiny::helpText("https://github.com/eric-hunt/unclePy")
        ),
        ##----------------------
        ##  uncleShiny        --
        ##----------------------
        shiny::headerPanel(""),
        shiny::fluidRow(
          shiny::tags$a(
            href = "https://github.com/eric-hunt/uncleShiny",
            target = "_blank",
            shiny::tags$h3("uncleShiny")
          ),
          shiny::br(),
          shiny::br(),
          shiny::tags$p(
            shiny::tags$span(shiny::tags$em("uncleShiny ")),
            "is this very dashboard! This app is written in R using the ",
            shiny::tags$span(
              shiny::tags$a(
                href = "https://shiny.rstudio.com/",
                target = "_blank",
                "Shiny"
              )
            ),
            "package. Database access utilizes the ",
            shiny::tags$span(
              shiny::tags$a(
                href = "https://cran.r-project.org/web/packages/RPostgres/index.html",
                target = "_blank",
                "RPostgres"
              )
            ),
            "driver package and all data manipulations are heavily reliant
            on core packages from the ",
            shiny::tags$span(
              shiny::tags$a(
                href = "https://www.tidyverse.org/",
                target = "_blank",
                "Tidyverse"
              )
            ),
            "including 'tidyr', 'dplyr', and 'purrr'. Visualization of data is 
            mostly performed with ",
            shiny::tags$span(
              shiny::tags$a(
                href = "https://plotly.com/r/",
                target = "_blank",
                "Plotly"
              )
            ),
            "and also 'ggplot2'. Theming of this app was done using the ",
            shiny::tags$span(
              shiny::tags$a(
                href = "https://rstudio.github.io/bslib/",
                target = "_blank",
                "bslib, "
              )
            ),
            shiny::tags$span(
              shiny::tags$a(
                href = "https://rstudio.github.io/thematic/",
                target = "_blank",
                "thematic, "
              )
            ),
            "and ",
            shiny::tags$span(
              shiny::tags$a(
                href = "https://cran.rstudio.com/web/packages/showtext/vignettes/introduction.html",
                target = "_blank",
                "showtext"
              )
            ),
            "packages."
          ),
          shiny::br(),
          shiny::br(),
          shiny::helpText("https://github.com/eric-hunt/uncleShiny")
        )
      )
    ),
    shiny::headerPanel("")
  )
}


##-------------------------------------------------------
##  SERVER FUNCTION                                    --
##-------------------------------------------------------
aboutServer <- function(id) {
  shiny::moduleServer(
    id,
    function(input, output, session) {
      
    }
  )
}
