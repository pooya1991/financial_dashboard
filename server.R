library(shiny)
source("R/dashboard_tables.R")
source("globals.R")

shinyServer(function(input, output, session) {
    observe({
        shinyjs::toggleState("run_settings", !is.null(input$xlsx_file))
    })
    
    observe(print(bot_is$`Profit or Loss`))
    
    observeEvent(input$xlsx_file, {
        input_assumptions <- readxl::read_excel(input$xlsx_file$datapath, sheet = "Input Assumptions")
        df_run_settings <- input_assumptions[c(2:9, 11), c(1,3)]
        default_inputs <- recode_run_settings(df_run_settings)
        updateDateInput(session, "proj_start_date", value = default_inputs$proj_start_date)
        updateNumericInput(session, "proj_year", value = default_inputs$proj_year)
        updateSelectInput(session, "proj_quarter", selected = default_inputs$proj_quarter)
        updateNumericInput(session, "proj_start_time", value = default_inputs$proj_start_time)
        updateSelectInput(session, "currency", choices = default_inputs$currency)
        updateNumericInput(session, "projection_period", value = default_inputs$projection_period)
        updateNumericInput(session, "uw_year", value = default_inputs$uw_year)
        updateSelectInput(session, "uw_quarter", selected = default_inputs$uw_quarter)
    })
    
    output$balance_sheet <- DT::renderDT({
        if (is.null(input$xlsx_file)) {
            balance_sheet <- character(); bs_base <- double(); bs_premiums <- double()
        }
        
        DT::datatable(balance_sheet2,
        colnames = c("Balance Sheet", "Base"), class = "strip",
        options = list(paging = FALSE, searching = FALSE, rowCallback = JS(rowCallback_bs))) #%>% 
            # DT::formatRound(c(2, 3), digits = 0, mark = ",") %>%
            # DT::formatStyle(c(2, 3), c(2, 3), target = "cell",
            #                 color = DT::styleInterval(-1e-5, c('red', 'black')))
        })
    
    output$income_statement <- DT::renderDT({
        if (is.null(input$xlsx_file)) {
            income_statement <- character(); is_base <- double(); is_premiums <- double()
        }
        
        DT::datatable(income_statement2,
        colnames = c("Income Statement", "Base"), class = "strip",
        options = list(paging = FALSE, searching = FALSE, rowCallback = JS(rowCallback_is)))
            # DT::formatRound(c(2, 3), digits = 0, mark = ",") %>% 
            # DT::formatStyle(c(2, 3), c(2, 3), target = "cell", 
            #                 color = DT::styleInterval(-1e-5, c('red', 'black')))
        })
})
