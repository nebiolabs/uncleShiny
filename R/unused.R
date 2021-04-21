# ### SCATTER MODULE ###
# 
# # Call the scatter plot module..
# observe({
#   req(data())
#   scatterEvents <- callModule(scatter, "scatter", data(), input$loadData, input$expSelection)
#   
#   # And render its event_data returns to the output..
#   output$summy_hover <<- renderPrint({
#     scatterEvents$summyHov()
#   })
#   output$summy_click <<- renderPrint({
#     scatterEvents$summyClk()
#   })
#   output$summy_select <<- renderPrint({
#     scatterEvents$summySel()
#   })
#   output$zoomy_hover <<- renderPrint({
#     scatterEvents$zoomyHov()
#   })
#   output$zoomy_click <<- renderPrint({
#     scatterEvents$zoomyClk()
#   })
#   output$zoomy_select <<- renderPrint({
#     scatterEvents$zoomySel()
#   })
# })


### PROXY FOR DT UPDATE ###
### Does not work well with `crosstalk` SharedData client side processing (server = FALSE)
# output$zoomyDT <- renderDT({
#   req(zoomySelData())
#   datatable(
#     data = isolate(zoomyDTdata()),
#     selection = "none",
#     # extensions = c("Buttons", "FixedColumns"),
#     # extensions = c("Select", "Buttons"),
#     extensions = c("Buttons"),
#     options = list(
#       # select = list(style = "multi", items = "row"),
#       dom = "Bftip",
#       # f - filter
#       searchHighlight = TRUE,
#       # p - pagination
#       scrollX = TRUE,
#       # scrollY = "250px",
#       paging = TRUE,
#       pageLength = 20,
#       scrollCollapse = TRUE,
#       # t - table
#       # fixedColumns = list(leftColumns = 4),
#       order = list(list(1, "desc")),
#       columnDefs = list(list(visible = FALSE, targets = c(1, 2))),
#       # B - Buttons
#       buttons =
#         list('copy', list(
#           extend = 'collection',
#           buttons = c('csv', 'excel', 'pdf'),
#           text = 'Download'
#         ))
#     )
#   )
# })

# zoomyDTproxy <- dataTableProxy("zoomyDT", session = session)
# 
# observeEvent(event_data("plotly_selected", "zoomydots"), {
#   replaceData(zoomyDTproxy, isolate(zoomyDTdata()))
#   # reloadData(zoomyDTproxy)
# })
