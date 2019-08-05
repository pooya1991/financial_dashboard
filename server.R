library(shiny)
source("R/dashboard_tables.R")
source("globals.R")

shinyServer(function(input, output, session) {
    observe({
        shinyjs::toggleState("run_settings", !is.null(input$xlsx_file))
    })
    
    observe(print(input$run1_base))
    
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
    

    observeEvent(input$run_onerousity, {
        showModal(modalDialog(
            "Onerousity test ran successfully",
            easyClose = TRUE,
            footer = NULL
        ))
    })
    
    observeEvent(input$coh_form, {
        showModal(modalDialog(
            "Cohort Formation ran successfully",
            easyClose = TRUE,
            footer = NULL
        ))
    })
    
    observeEvent(input$run1_base, {
        showModal(modalDialog(
            "Run1 base ran successfully",
            easyClose = TRUE,
            footer = NULL
        ))
    })
    
    dashboard_data <- reactiveValues(out_bs = tibble(balance_sheet = character(), bs_base = double()),
                                     out_is = tibble(income_statement = character(), is_base = double()))
    observe({
        if (is.null(input$input_file) | is.null(input$pas_data)) return()
        pth_input_assumptions <- input$input_file$datapath
        pth_contract_details <- input$pas_data$datapath
        source("R/ifrs_base.R", local = TRUE)
        dashboard_data$out_bs <- balance_sheet2
        dashboard_data$out_is <- income_statement2
    })
    
    output$balance_sheet <- DT::renderDT({
        DT::datatable(dashboard_data$out_bs,
        colnames = c("Balance Sheet", "Base"), class = "strip", 
        options = list(paging = FALSE, searching = FALSE, info = FALSE,
                       rowCallback = JS(rowCallback_bs)))
        })
    
    output$income_statement <- DT::renderDT({
        DT::datatable(dashboard_data$out_is,
        colnames = c("Income Statement", "Base"), class = "strip",
        options = list(paging = FALSE, searching = FALSE, info = FALSE,
                       rowCallback = JS(rowCallback_is)))
        })
})
