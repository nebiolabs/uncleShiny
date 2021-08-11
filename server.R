
##------------------------------------------------------------------------------
##  server.R                                                                  --
##------------------------------------------------------------------------------

function(input, output, session) {
  
  ##-----------------------------------------------------------
  ##  Interactive theme selection                            --
  ##-----------------------------------------------------------

  # bslib::bs_themer()
  
  
  ##-----------------------------------------------------------
  ##  Performance profiling                                  --
  ##-----------------------------------------------------------
  
  # callModule(profvis::profvis_server, "profiler")
  
  
  
  
  ##----------------------------------------------------------
  ##  Postgres database connection                          --
  ##----------------------------------------------------------
  
  ##--------------------------------------
  ##  Available products                --
  ##--------------------------------------
  
  # Reactive object of available products
  reactive_products <- eventReactive(input$db_refresh, {
    req(ebase_dev)
    DBI::dbGetQuery(
      ebase_dev,
      sql_queries$products
    )
  })
  
  # Render table for db inspection
  output$table_products_available <- renderDT({
    req(reactive_products())
    datatable(
      data = reactive_products(),
      selection = "none",
      # extensions = c("FixedColumns"),
      options = list(
        dom = "tip",
        # f - filter
        # searchHighlight = TRUE,
        # p - pagination
        scrollX = FALSE,
        # scrollY = "250px",
        paging = TRUE,
        pageLength = 10,
        scrollCollapse = TRUE#,
        # t - table
        # fixedColumns = list(leftColumns = 5),
        # order = list(list(3, "asc")),
        # columnDefs = list(list(visible = FALSE, targets = c(1, 2)))
      )
    )
  })
  
  # Update choices for product selection
  observeEvent(reactive_products(), {
    req(reactive_products())
    
    updated_choices <- reactive_products() |>
      dplyr::select(product_name, product_id) |>
      tibble::deframe()
    
    updateSelectInput(
      session,
      "product_selected",
      choices = c(" " = 0, updated_choices),
      selected = 0
    )
  })
  
  # Raw output for debugging
  output$raw_product_selected <- renderPrint({
    input$product_selected
  })
  
  
  ##:::::::::::::::::::::::::::
  ##  Available Experiments  ::
  ##:::::::::::::::::::::::::::
  
  db_exp_sets_table <- eventReactive(input$product_selected, {
    req(reactive_products())
    DBI::dbGetQuery(
      ebase_dev,
      glue::glue_sql(
        sql_queries$exp_sets,
        input = input$product_selected,
        .con = ebase_dev
      )
    )
  })
  
  db_exps_table <- eventReactive(input$product_selected, {
    req(reactive_products())
    exp_sets <- db_exp_sets_table() |> 
      dplyr::pull(set_id) |> 
      unique()
    DBI::dbGetQuery(
      ebase_dev,
      glue::glue_sql(
        "SELECT id AS exp_id, uncle_experiment_set_id AS set_id,
            uncle_instrument_id AS inst_id, plate_side AS side, date
          FROM uncle_experiments
          WHERE uncle_experiment_set_id IN ({input*})",
        input = exp_sets,
        .con = ebase_dev
      )
    )
  })
  
  output$db_exp_sets_available <- renderDT({
    req(db_exp_sets_table())
    datatable(
      data = db_exp_sets_table(),
      selection = "none",
      # extensions = c("FixedColumns"),
      options = list(
        dom = "tip",
        # f - filter
        # searchHighlight = TRUE,
        # p - pagination
        scrollX = TRUE,
        scrollY = "400px",
        paging = FALSE,
        pageLength = 20,
        scrollCollapse = TRUE#,
        # t - table
        # fixedColumns = list(leftColumns = 5),
        # order = list(list(3, "asc")),
        # columnDefs = list(list(visible = FALSE, targets = c(1, 2)))
      )
    )
  })
  
  output$db_exps_available <- renderDT({
    req(db_exps_table())
    datatable(
      data = db_exps_table(),
      selection = "none",
      # extensions = c("FixedColumns"),
      options = list(
        dom = "tip",
        # f - filter
        # searchHighlight = TRUE,
        # p - pagination
        scrollX = TRUE,
        scrollY = "400px",
        paging = FALSE,
        pageLength = 80,
        scrollCollapse = TRUE#,
        # t - table
        # fixedColumns = list(leftColumns = 5),
        # order = list(list(3, "asc")),
        # columnDefs = list(list(visible = FALSE, targets = c(1, 2)))
      )
    )
  })
  
  db_exps_available <- eventReactive(input$product_selected, {
    req(db_exp_sets_table())
    db_exp_sets_table() |>
      tidyr::unite(
        col = "experiment",
        exp_type, gen, set_id, well_set_id,
        sep = "_",
        remove = FALSE
      ) |> 
      dplyr::select(experiment, set_id) |> 
      dplyr::mutate(across(c(set_id), .fns = bit64::as.integer64)) |> 
      tibble::deframe()
  })
  
  observeEvent(db_exps_available(), {
    req(db_exps_available())
    updateSelectInput(
      session,
      "db_exp_sel",
      choices = bit64::c.integer64(" " = 0, db_exps_available())
    )
  })
  
  output$db_exp_set_selected <- renderPrint({
    input$db_exp_sel
  })
  
  
  ##::::::::::::::::::
  ##  PSQL Load-in  ::
  ##::::::::::::::::::
  
  db_data <- eventReactive(input$db_load, {
    summary_data <- DBI::dbGetQuery(
      ebase_dev,
      # glue::glue_sql(
      #   "SELECT sum.*, wells.layout_address AS well
      #   FROM uncle_summaries sum INNER JOIN wells
      #     ON sum.well_id = wells.id
      #         WHERE EXISTS (SELECT *
      #                       FROM uncle_experiments exps
      #                       WHERE exps.uncle_experiment_set_id IN ({input*})
      #                         AND sum.uncle_experiment_id = exps.id)",
      glue::glue_sql(
        "WITH cte_sum AS
        (SELECT wells.layout_address AS well, sum.* 
        FROM uncle_summaries sum
        INNER JOIN wells
          ON sum.well_id = wells.id
        WHERE EXISTS (SELECT *
                      FROM uncle_experiments exps
                      WHERE exps.uncle_experiment_set_id IN ({input*})
                        AND sum.uncle_experiment_id = exps.id)
        )
        SELECT cte_sum.*, exp_conds.well_id, exp_conds.id AS experiment_condition_id, exp_conds.condition_id,
          exp_conds.unit_id, exp_conds.raw_value AS unit_value,
          units.name AS unit_name, conds.name AS cond_name 
        FROM cte_sum
        INNER JOIN experimental_conditions AS exp_conds
          ON cte_sum.well_id = exp_conds.well_id
        INNER JOIN units
          ON exp_conds.unit_id = units.id
        INNER JOIN conditions AS conds
          ON exp_conds.condition_id = conds.id",
        input = input$db_exp_sel,
        .con = ebase_dev
      )
    ) |> 
      dplyr::mutate(sharedKey = id) |> 
      dplyr::rename(
        Tagg266 = t_agg_266,
        Tagg473 = t_agg_473,
        Tm1 = t_m_1,
        Z_D = z_avg_diameter,
        peak1_D = pk_1_mode_diameter,
        PdI = pdi
      ) |> 
      dplyr::mutate(residuals = purrr::map(residuals, parse_float8)) |>
      dplyr::rename(uncle_summary_id = id) |> 
      tibble::as_tibble()
    
    summary_ids <- summary_data |> dplyr::pull(uncle_summary_id)
    
    spec_tbls <- get_spec_tbls(ebase_dev, spec_tbl_list, summary_ids)
    
    # return(nest_spectra(summary_data, spec_tbls))
    return(summary_data)
  })
  
  
  output$db_data_print <- renderDT({
    db_data() |> 
      dplyr::select(-contains("spec"), -contains("residuals"))
  })
  
  
  ##::::::::::::::::::::
  ##  PSQL Plot Data  ::
  ##::::::::::::::::::::
  
  db_summyShared <- eventReactive(c(db_data()), {
    req(db_data())
    highlight_key(
      db_data,
      key = ~sharedKey,
      group = paste("summy", input$product_selected, input$db_exp_sel, sep = "_")
    )
  })
  
  
  ##::::::::::::::::
  ##  PSQL Plots  ::
  ##::::::::::::::::
  
  db_plotOpts <- plotOptsServer("db_summyOpts")
  
  # Plot 1 as a reactive..
  db_p1 <- reactive({
    source("R/plotly.R", local = TRUE)
    db_buildplotly(
      data = db_summyShared(),
      x = db_plotOpts$xvar1(),
      y = db_plotOpts$yvar1(),
      source = "db_summydots",
      color = "well",
      palette = db_plotOpts$palette(),
      customdata = "well_id"
    )
  })
  
  # Plot 2 as a reactive..
  db_p2 <- reactive({
    source("R/plotly.R", local = TRUE)
    db_buildplotly(
      data = db_summyShared(),
      x = db_plotOpts$xvar2(),
      y = db_plotOpts$yvar2(),
      source = "db_summydots",
      color = "well",
      palette = db_plotOpts$palette(),
      # showlegend = FALSE,
      # colorbar = FALSE,
      customdata = "well_id"
    )
  })
  
  # The plotly subplot of both connected plots..
  output$db_summydots <- renderPlotly({
    subplot(
      db_p1(), db_p2(),
      nrows = 1,
      titleX = TRUE,
      titleY = TRUE,
      margin = 0.04
    ) |> 
      layout(
        annotations = list(
          list(
            x = 0, xref = "paper", xanchor = "right",
            y = 1.09, yref = "paper", 
            text = "Plot1", font = list(size = 18),
            showarrow = F
          ),
          list(
            x = 0.54, xref = "paper", xanchor = "right",
            y = 1.09, yref = "paper",
            text = "Plot2", font = list(size = 18),
            showarrow = F
          )
        ),
        legend = legendList
      ) |> 
      highlight(
        on = "plotly_selected", off = "plotly_deselect",
        opacityDim = 0.15,
        selected = attrs_selected(showlegend = FALSE)
      ) |> 
      event_register(event = "plotly_selected") |> 
      event_register(event = "plotly_click") |> 
      event_register(event = "plotly_hover") |> 
      config(displaylogo = FALSE) |> 
      toWebGL()
  })
  
  output$db_summySel <- DT::renderDT({
    req(db_summyShared())
    event_data(event = "plotly_selected", source = "db_summydots")#[["key"]]
    # db_data()[db_data()$sharedKey %in% cd, ]
  })
  
  
  ##================================================================
  ##                      db/Local Visual QC                      ==
  ##================================================================
  
  
  ##:::::::::
  ##  DLS  ::
  ##:::::::::
  
  output$db_dls <- renderPlot({
    req(db_data())
    fun_data <- db_data() |>
      dplyr::select(-c(created_at:cond_name)) |> 
      dplyr::distinct() |> 
      dplyr::filter(between(Z_D, 0, 999)) |> 
      dplyr::mutate(PdI = round(PdI, digits = 2))
    plot <- ggplot(
      data = fun_data,
      aes_string(x = "Z_D", y = "PdI")
    ) +
      geom_point(aes_string(fill = "well"),
                 shape = 21,
                 color = "black",
                 size = 2,
                 alpha = 0.5,
                 show.legend = FALSE) +
      ggrepel::geom_text_repel(aes_string(label = "well"), alpha = 0.5) +
      scale_color_manual(values = mycolors("Spectral", 96)) +
      theme(legend.position = "none") +
      labs(
        subtitle = "(db) DLS Summary"
      ) +
      geom_vline(xintercept = 1, linetype = "dashed", alpha = 0.2) +
      geom_vline(xintercept = 10, linetype = "dashed", alpha = 0.2) +
      scale_x_log10(limits = c(0.1, max(fun_data[["Z_D"]]))) +
      scale_y_continuous(limits = c(min(fun_data[["PdI"]]), max(fun_data[["PdI"]]))) +
      # scale_x_log10(limits = c(1, 1000)) +
      # scale_y_continuous(limits = c(0,1)) +
      annotation_logticks(sides = "b")
    return(plot)
  })
  
  output$local_dls <- renderPlot({
    req(summyData())
    fun_data <- summyData() |> 
      tidyr::drop_na(Z_D)
    plot <- ggplot(
      data = fun_data,
      aes_string(x = "Z_D", y = "PdI")
    ) +
      geom_point(aes_string(fill = "well"),
                 shape = 21,
                 color = "black",
                 size = 2,
                 alpha = 0.5,
                 show.legend = FALSE) +
      ggrepel::geom_text_repel(aes_string(label = "well"), alpha = 0.5) +
      scale_color_manual(values = mycolors("Spectral", 96)) +
      theme(legend.position = "none") +
      labs(
        subtitle = "(local) DLS Summary"
      ) +
      geom_vline(xintercept = 1, linetype = "dashed", alpha = 0.2) +
      geom_vline(xintercept = 10, linetype = "dashed", alpha = 0.2) +
      scale_x_log10(limits = c(0.1, max(fun_data[["Z_D"]]))) +
      scale_y_continuous(limits = c(min(fun_data[["PdI"]]), max(fun_data[["PdI"]]))) +
      # scale_x_log10(limits = c(1, 1000)) +
      # scale_y_continuous(limits = c(0,1)) +
      annotation_logticks(sides = "b")
    return(plot)
  })
  
  
  ##:::::::::
  ##  SLS  ::
  ##:::::::::
  
  output$db_sls <- renderPlot({
    req(db_data())
    fun_data <- db_data() |> 
      dplyr::select(-c(created_at:cond_name)) |> 
      dplyr::distinct()
    plot <- ggplot(
      data = fun_data,
      aes_string(x = "Tm1", y = "Tagg266")
    ) +
      geom_point(aes_string(fill = "well"),
                 shape = 21,
                 color = "black",
                 size = 2,
                 alpha = 0.5,
                 show.legend = FALSE) +
      ggrepel::geom_text_repel(aes_string(label = "well"), alpha = 0.5) +
      # geom_text(aes_string(label = "well"), alpha = 0.5) +
      scale_color_manual(values = mycolors("Spectral", 96)) +
      theme(legend.position = "none") +
      labs(
        subtitle = "(db) SLS Summary"
      ) +
      scale_x_continuous(limits = c(min(fun_data[["Tm1"]]), max(fun_data[["Tm1"]]))) +
      scale_y_continuous(limits = c(min(fun_data[["Tagg266"]]), max(fun_data[["Tagg266"]])))
    # scale_x_continuous(limits = c(20, 80)) +
    # scale_y_continuous(limits = c(20, 80))
    return(plot)
  })
  
  output$local_sls <- renderPlot({
    req(summyData())
    fun_data <- summyData()
    plot <- ggplot(
      data = fun_data,
      aes_string(x = "Tm1", y = "Tagg266")
    ) +
      geom_point(aes_string(fill = "well"),
                 shape = 21,
                 color = "black",
                 size = 2,
                 alpha = 0.5,
                 show.legend = FALSE) +
      ggrepel::geom_text_repel(aes_string(label = "well"), alpha = 0.5) +
      # geom_text(aes_string(label = "well"), alpha = 0.5) +
      scale_color_manual(values = mycolors("Spectral", 96)) +
      theme(legend.position = "none") +
      labs(
        subtitle = "(local) SLS Summary"
      ) +
      scale_x_continuous(limits = c(min(fun_data[["Tm1"]]), max(fun_data[["Tm1"]]))) +
      scale_y_continuous(limits = c(min(fun_data[["Tagg266"]]), max(fun_data[["Tagg266"]])))
    # scale_x_continuous(limits = c(20, 80)) +
    # scale_y_continuous(limits = c(20, 80))
    return(plot)
  })
  
  
  ##===============================================================
  ##                           Load-in                           ==
  ##===============================================================
  
  # creates the reactive list of experiment data tables from selected value in sidebar on button click..
  dataList <- eventReactive(input$loadData, {
    if (input$dataSelection == "") {
      data <- NULL
    } else {
      data <- readRDS(input$dataSelection) %>%
        map(
          function(df) {
            drops <- c("map_color")
            # dummy variables are added for tooltip metadata information if missing
            if (!(identical(setdiff(tooltipVars, names(df)), character(0)))) {
              missing <- setdiff(tooltipVars, names(df))
              df[missing] <- NA
            }
            newdf <- df %>%
              select(-any_of(drops)) %>% 
              modify_at(vars(well), ~ parse_factor(as.character(.x), levels = wellOrder, ordered = TRUE)) %>%
              # a shared key is added for crosstalk compatibility
              unite("sharedKey", c(plate, well, buffer, pH, salt, additive1, additive2), remove = FALSE)
            return(newdf)
          }
        )
    }
    return(data)
  })
  
  
  ##================================================================
  ##                         Print Module                         ==
  ##================================================================
  
  
  observeEvent(input$loadData, {
    req(dataList())
    callModule(printer, "dataListPrinter", dataList())
  }, ignoreInit = TRUE)
  
  
  ##===============================================================
  ##                       Experiment Data                       ==
  ##===============================================================
  
  # this updates the experiment lists when the data selection changes..
  observeEvent(input$loadData, {
    req(dataList())
    updateSelectInput(
      session,
      "expSelection",
      choices = names(dataList()),
      selected = names(dataList())[1]
    )
    updateSelectInput(
      session,
      "blendSelectionL",
      choices = names(dataList()),
      selected = names(dataList())[1]
    )
    updateSelectInput(
      session,
      "blendSelectionR",
      choices = names(dataList()),
      selected = names(dataList())[2]
    )
  })
  
  output$blendL <- renderDT({
    req(dataList(), input$blendSelectionL)
    datatable(
      data = dataList()[[input$blendSelectionL]][!(grepl("spec", names(dataList()[[input$blendSelectionL]])))],
      # data = dataList()[[input$blendSelectionL]] %>% select(!(contains("spec"))),
      selection = "none",
      extensions = c("FixedColumns"),
      options = list(
        dom = "ftip",
        # f - filter
        searchHighlight = TRUE,
        # p - pagination
        scrollX = TRUE,
        # scrollY = "250px",
        paging = TRUE,
        pageLength = 16,
        scrollCollapse = TRUE,
        # t - table
        fixedColumns = list(leftColumns = 5),
        order = list(list(3, "asc")),
        columnDefs = list(list(visible = FALSE, targets = c(1, 2)))
      )
    )
  })
  
  output$blendR <- renderDT({
    req(dataList(), input$blendSelectionR)
    datatable(
      data = dataList()[[input$blendSelectionR]][!(grepl("spec", names(dataList()[[input$blendSelectionR]])))],
      # data = dataList()[[input$blendSelectionR]] %>% select(!(contains("spec"))),
      selection = "none",
      extensions = c("FixedColumns"),
      options = list(
        dom = "ftip",
        # f - filter
        searchHighlight = TRUE,
        # p - pagination
        scrollX = TRUE,
        # scrollY = "250px",
        paging = TRUE,
        pageLength = 16,
        scrollCollapse = TRUE,
        # t - table
        fixedColumns = list(leftColumns = 5),
        order = list(list(3, "asc")),
        columnDefs = list(list(visible = FALSE, targets = c(1, 2)))
      )
    )
  })
  
  data <- debounce(
    eventReactive(c(input$loadData, input$expSelection, input$modeSelection, input$blendData), {
      req(dataList(), input$expSelection, input$blendSelectionL)
      if (input$modeSelection == "blend") {
        expData <- purrr::map_dfr(
          dataList()[c(input$blendSelectionL, input$blendSelectionR)],
          bind_rows#,
          # .id = "exp"
        )
      } else {
        expData <- dataList() %>% pluck(input$expSelection)
      }
      return(expData)
    }),
    millis = 500, priority = -100
  )
  
  output$tableOutput <- renderPrint({
    data()
  })
  
  
  ##===============================================================
  ##                     Plot options module                     ==
  ##===============================================================
  
  
  # Loading in the plot options module as reactive values..
  plotOpts <- plotOptsServer("summyOpts")
  
  
  ##===============================================================
  ##                        Summary plots                        ==
  ##===============================================================
  
  
  ##:::::::::::::::::::::::
  ##  Summary plot data  ::
  ##:::::::::::::::::::::::
  
  
  summyData <- eventReactive(c(data(), plotOpts$color()), {
    req(data(), plotOpts$color())
    fctr <- rlang::sym(c(plotOpts$color()))
    data() %>%  
      # The spectra columns are removed for plotting and displaying event_data..
      select(!(contains("spec"))) %>% 
      # The data is factored by the color variable (coerced to character) for matching between plots..
      mutate(!! fctr := replace_na(!! fctr, "none")) %>% 
      mutate(!! fctr := fct_infreq(factor(!! fctr)))
    # mutate(!! fctr := forcats::fct_infreq(as.character(!! fctr)))
  })
  
  # Creating the `crosstalk` shared data entity for plotting..
  summyShared <- eventReactive(c(summyData(), plotOpts$color()), {
    req(summyData())
    highlight_key(
      summyData,
      key = ~sharedKey,
      group = paste0("summy", input$modeSelection, plotOpts$color())
    )
  })
  
  
  ##::::::::::::::::::
  ##  Plotly plots  ::
  ##::::::::::::::::::
  
  
  # Plot 1 as a reactive..
  p1 <- reactive({
    source("R/plotly.R", local = TRUE)
    buildplotly(
      data = summyShared(),
      x = plotOpts$xvar1(),
      y = plotOpts$yvar1(),
      source = "summydots",
      color = plotOpts$color(),
      palette = plotOpts$palette(),
      customdata = "well"
    )# %>% 
    # event_register(event = "plotly_selected") %>% 
    # event_register(event = "plotly_click") %>% 
    # event_register(event = "plotly_hover")
  })
  
  # Plot 2 as a reactive..
  p2 <- reactive({
    source("R/plotly.R", local = TRUE)
    buildplotly(
      data = summyShared(),
      x = plotOpts$xvar2(),
      y = plotOpts$yvar2(),
      source = "summydots",
      color = plotOpts$color(),
      palette = plotOpts$palette(),
      # showlegend = FALSE,
      # colorbar = FALSE,
      customdata = "well"
    )# %>% 
    # event_register(event = "plotly_selected") %>% 
    # event_register(event = "plotly_click") %>% 
    # event_register(event = "plotly_hover")
  })
  
  # The plotly subplot of both connected plots..
  output$summydots <- renderPlotly({
    subplot(
      p1(), p2(),
      nrows = 1,
      titleX = TRUE,
      titleY = TRUE,
      margin = 0.04
    ) %>%
      layout(
        annotations = list(
          list(
            x = 0, xref = "paper", xanchor = "right",
            y = 1.09, yref = "paper", 
            text = "Plot1", font = list(size = 18),
            showarrow = F
          ),
          list(
            x = 0.54, xref = "paper", xanchor = "right",
            y = 1.09, yref = "paper",
            text = "Plot2", font = list(size = 18),
            showarrow = F
          )
        ),
        legend = legendList
      ) %>%
      highlight(
        on = "plotly_selected", off = "plotly_deselect",
        opacityDim = 0.15,
        selected = attrs_selected(showlegend = FALSE)
      ) %>% 
      event_register(event = "plotly_selected") %>% 
      event_register(event = "plotly_click") %>% 
      event_register(event = "plotly_hover") %>%
      config(displaylogo = FALSE) %>% 
      toWebGL()
  })
  
  
  ##:::::::::::::::::
  ##  Eevent data  ::
  ##:::::::::::::::::
  
  
  # The hover data for the sparkline quick-view spectra, debounced for user input timing..
  summyHov <- debounce(reactive({
    df <- event_data(event = "plotly_hover", source = "summydots")
    if (is.null(df)) {
      return(NULL)
    }
    return(df[["key"]])
  }), 500)
  
  # summyHovData <- debounce(reactive({
  #   cd <- event_data(event = "plotly_hover", source = "summydots")[["key"]]
  #   if (is.null(cd)) {
  #     return(NULL)
  #   } else {
  #     return(data()[data()$sharedKey %in% cd, ][1, ])
  #   }
  # }), 500)
  
  # The click data for the sparkline quick-view spectra plot..
  summyClk <- reactive({
    df <- event_data(event = "plotly_click", source = "summydots")
    if (is.null(df)) {
      return(NULL)
    }
    return(df[["key"]])
  })
  
  # summyClkData <- reactive({
  #   cd <- event_data(event = "plotly_click", source = "summydots")[["key"]]
  #   if (is.null(cd)) {
  #     return(NULL)
  #   } else {
  #     return(data()[data()$sharedKey %in% cd, ][1, ])
  #   }
  # })
  
  # The selected data from the summary plot..
  summySelData <- reactive({
    req(summyData())
    cd <- event_data(event = "plotly_selected", source = "summydots")[["key"]]
    summyData()[summyData()$sharedKey %in% cd, ]
  })
  
  
  ##================================================================
  ##                      Spectra sparklines                      ==
  ##================================================================
  
  
  # The sparkline quick-view spectra plot..
  observeEvent(summyHov(), {
    # req(summyHov())
    # callModule(specSpark, "specSparkHover", summyHovData(), summyClkData(), plotOpts$palette())
    specSparkServer("specSparkHover", data(), summyHov(), summyClk(), plotOpts$palette())
  }, ignoreInit = TRUE, ignoreNULL = FALSE)
  
  
  ##================================================================
  ##                        Zoomed-in plot                        ==
  ##================================================================
  
  
  ##:::::::::::::::::::::::::
  ##  Zoomed-in plot data  ::
  ##:::::::::::::::::::::::::
  
  
  # Creating the `crosstalk` shared data entity for plotting the zoomed data..
  zoomyShared <- eventReactive(c(summySelData(), plotOpts$zoomycolor()), {
    req(summySelData())
    # highlight_key(summySelData, key = ~sharedKey, group = paste0("zoomy", plotOpts$zoomycolor()))
    highlight_key(summySelData, key = ~sharedKey, group = "zoomy")
  })
  
  
  ##:::::::::::::::::
  ##  Plotly plot  ::
  ##:::::::::::::::::
  
  
  # The zoomed-in plot as a reactive..
  zoomy <- reactive({
    req(zoomyShared())
    source("R/plotly.R", local = TRUE)
    if (is.null(summySelData())) {
      p <- NULL
    } else {
      p <- buildplotly(
        data = zoomyShared(),
        x = plotOpts$xvar3(),
        y = plotOpts$yvar3(),
        source = "zoomydots",
        # color = plotOpts$color(),
        color = plotOpts$zoomycolor(),
        palette = plotOpts$palette(),
        # showlegend = FALSE,
        customdata = "well"
      ) %>%
        layout(
          annotations = list(
            list(x = 0,
                 xref = "paper",
                 y = 1.09,
                 yref = "paper",
                 text = "Zoom",
                 showarrow = F,
                 font = list(size = 18),
                 xanchor = "right")
          ),
          legend = legendList
        )
    }
  })
  
  output$zoomydots <- renderPlotly({
    zoomy() %>%
      highlight(
        on = "plotly_select", off = "plotly_deselect",
        opacityDim = 0.15,
        selected = attrs_selected(showlegend = FALSE)
      ) %>%
      config(displaylogo = FALSE) %>%
      event_register(event = "plotly_selected") %>%
      event_register(event = "plotly_click") %>%
      event_register(event = "plotly_hover") %>%
      toWebGL()
  })
  
  
  ##:::::::::::::::::
  ##  Eevent data  ::
  ##:::::::::::::::::
  
  
  # The selected data from the zoomed-in plot..
  zoomySelData <- reactive({
    req(data(), summySelData())
    cd <- event_data(event = "plotly_selected", source = "zoomydots")[["key"]]
    if (is.null(summySelData())) {
      return(NULL)
    } else if (is.null(cd)) {
      return(data()[data()$sharedKey %in% summySelData()$sharedKey, ])
    } else {
      data()[data()$sharedKey %in% cd, ]
    }
  })
  
  
  zoomyDTdata <- reactive({
    req(summySelData())
    cd <- event_data(event = "plotly_selected", source = "zoomydots")[["key"]]
    if (is.null(cd)) {
      summySelData()
    } else {
      summySelData()[summySelData()$sharedKey %in% cd, ]
    }
  })
  
  
  ##:::::::::::::::::::::::::::
  ##  DT of selected values  ::
  ##:::::::::::::::::::::::::::
  
  # output$zoomytable <- renderDT({
  #   zoomyShared()$data(withSelection = TRUE, withFilter = TRUE, withKey = FALSE)
  # })
  
  zoomyDT <- reactive({
    req(summySelData(), zoomyShared())
    datatable(
      data = zoomyShared(),
      selection = "none",
      # extensions = c("Buttons", "FixedColumns"),
      # extensions = c("Select", "Buttons"),
      extensions = c("Buttons"),
      options = list(
        # select = list(style = "multi", items = "row"),
        dom = "Bftip",
        # f - filter
        searchHighlight = TRUE,
        # p - pagination
        scrollX = TRUE,
        # scrollY = "250px",
        paging = TRUE,
        pageLength = 20,
        scrollCollapse = TRUE,
        # t - table
        # fixedColumns = list(leftColumns = 4),
        order = list(list(1, "desc")),
        columnDefs = list(list(visible = FALSE, targets = c(1, 2))),
        # B - Buttons
        buttons =
          list('copy', list(
            extend = 'collection',
            buttons = c('csv', 'excel', 'pdf'),
            text = 'Download'
          ))
      )
    )
  })
  
  output$zoomyDT <- renderDT({
    req(zoomyDT())
    zoomyDT()
  }, server = FALSE)
  
  
  output$testysquid <- renderPrint({
    # zoomyDTdata()
    zoomyShared()$data()
  })
  
  
  ##===============================================================
  ##                    Spectra viewer module                    ==
  ##===============================================================
  
  observeEvent(c(zoomySelData(), summySelData()), {
    req(zoomySelData())
    callModule(spectraViewer, "spectraViewer", zoomySelData(), plotOpts$palette())
  }, ignoreNULL = FALSE)
  # observe({
  #   req(zoomySelData())
  #   callModule(spectraViewer, "spectraViewer", zoomySelData(), plotOpts$palette())
  # })
  
  
  ##===============================================================
  ##                    Activity assay module                    ==
  ##===============================================================
  
  observeEvent(data(), {
    req(data())
    callModule(activityAssay, "activityAssay", data(), plotOpts$palette())
  }, ignoreNULL = FALSE)
  
  
  ##================================================================
  ##                        Raw input data                        ==
  ##================================================================
  
  
  # For the summary plot..
  output$summyHov <- renderPrint({
    summyHov()
  })
  
  output$summyClk <- renderPrint({
    summyClk()
  })
  
  output$summySel <- renderPrint({
    df <- event_data(event = "plotly_selected", source = "summydots")
    if (is.null(df)) {
      return(NULL)
    }
    return(df[["key"]])
  })
  
  # For the zoomed plot..
  output$zoomyHov <- renderPrint({
    df <- event_data(event = "plotly_hover", source = "zoomydots")
    if (is.null(df)) {
      return(NULL)
    }
    return(df[["customdata"]])
  })
  
  output$zoomyClk <- renderPrint({
    df <- event_data(event = "plotly_click", source = "zoomydots")
    if (is.null(df)) {
      return(NULL)
    }
    return(df[["customdata"]])
  })
  
  output$zoomySel <- renderPrint({
    df <- event_data(event = "plotly_selected", source = "zoomydots")
    if (is.null(df)) {
      return(NULL)
    }
    return(df[["key"]])
  })
  
}