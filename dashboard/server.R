library(shiny)
source("R/dashboard_tables.R")

shinyServer(function(input, output, session) {
    # observe({
    #     shinyjs::toggleState("default_inputs", !is.null(input$xlsx_file))
    # })

    observe({
        if (is.null(input$xlsx_file)) {
            shinyjs::disable("default_inputs")
        } else {
            shinyjs::enable("default_inputs")
        }
    })
    output$balance_sheet <- DT::renderDT(DT::datatable(
        tibble(balance_sheet, bs_base, bs_premiums),
        options = list(paging = FALSE, searching = FALSE))
        )
    output$income_statement <- DT::renderDT(DT::datatable(
        tibble(income_statement, is_base, is_premiums),
        options = list(paging = FALSE, searching = FALSE))
        )
})
