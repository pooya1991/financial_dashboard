library(shiny)
source("../R/dashboard_tables.R")

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
        colnames = c("Balance Sheet", "Base", "Premiums"), rownames = FALSE,
        options = list(paging = FALSE, searching = FALSE)) %>% 
            DT::formatRound(c(2, 3), digits = 0, mark = ",") %>% 
            DT::formatStyle(c(2, 3), c(2, 3), target = "cell", 
                            color = DT::styleInterval(-1e-5, c('red', 'black')))
        )
    output$income_statement <- DT::renderDT(DT::datatable(
        tibble(income_statement, is_base, is_premiums),
        colnames = c("Income Statement", "Base", "Premiums"), rownames = FALSE,
        options = list(paging = FALSE, searching = FALSE)) %>% 
            DT::formatRound(c(2, 3), digits = 0, mark = ",") %>% 
            DT::formatStyle(c(2, 3), c(2, 3), target = "cell", 
                            color = DT::styleInterval(-1e-5, c('red', 'black')))
        )
})
