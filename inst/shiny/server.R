server <- function(input, output, session) {

  # Settings -----------------------------------
  opts <- reactive({
    input$opts_response
    input$opts_method
    input$opts_b
    input$opts_alpha
    input$opts_wscale
    input$opts_sightability


    switch_response(input$opts_response)
    mc_options(method = input$opts_method)
    mc_options(B = input$opts_b)
    mc_options(alpha = as.numeric(input$opts_alpha))

    mc_options(wscale = input$opts_wscale)
    mc_options(sightability = input$opts_sightability)
    #mc_options(srv = "")
    #mc_options(MAXCELL = "")
    #mc_options(area_srv = "")

    # Grab options to print
    getOption("moose_options")
  })

  output$opts <- renderTable({
    data.frame(option = names(opts()),
               values = sapply(opts(),
                               FUN = function(x) paste(x, collapse = ", ")))
  })
}

