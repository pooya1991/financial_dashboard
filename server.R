library(shiny)
source("R/dashboard_tables.R")

shinyServer(function(input, output, session) {
    observe({
        shinyjs::toggleState("run_settings", !is.null(input$xlsx_file))
    })
    
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
    
    pth_input_assumptions <- reactive({
        if (is.null(input$input_file)) return(NULL)
        input$input_file$datapath
    })
    
    pth_contract_details <- reactive({
        if (is.null(input$pas_data)) return(NULL)
        input$pas_data$datapath        
    })
    
    test_data <- reactiveValues(onerosity_df = NULL)
    event_status <- reactiveValues(onerousity_test = FALSE, cohort_test = FALSE)
    
    observeEvent(input$run_onerousity, {
        if (is.null(input$input_file) | is.null(input$pas_data)) {
            showModal(modalDialog(
                "Please submit input assumptions and PAS data",
                easyClose = TRUE,
                footer = NULL
            ))
            return()
        }
        
        pth_input_assumptions <- pth_input_assumptions()
        pth_contract_details <- pth_contract_details()
        source("R/inputs.R", local = TRUE)
        test_data$onerosity_df <- onerosity_test(contract_details, onerosity_criteria)
        event_status$onerousity_test <- TRUE
        showModal(modalDialog(
            "Onerousity test ran successfully",
            easyClose = TRUE,
            footer = NULL
        ))
    })
    
    observeEvent(input$coh_form, {
        if (!event_status$onerousity_test) {
            showModal(modalDialog(
                "Please run onerousity test first",
                easyClose = TRUE,
                footer = NULL
            ))
            return()
        }
        event_status$cohort_test <- TRUE
        showModal(modalDialog(
            "Cohort Formation ran successfully",
            easyClose = TRUE,
            footer = NULL
        ))
    })
    
    observeEvent(input$run1_base, {
        if (!event_status$cohort_test) {
            showModal(modalDialog(
                "Please run cohort formation first",
                easyClose = TRUE,
                footer = NULL
            ))
            return()
        }
        
        pth_input_assumptions <- pth_input_assumptions()
        pth_contract_details <- pth_contract_details()
        source("R/inputs.R", local = TRUE)
        contract_summed <- group_by(test_data$onerosity_df, group_type) %>%
            summarise(
                total_premium = sum(PREMIUM),
                total_claims_incurred = sum(`CLAIMS INCURRED`),
                total_commission = sum(COMMISSION)
            )
        source("R/ifrs_base.R", local = TRUE)
        showModal(modalDialog(
            "Run1 base ran successfully",
            easyClose = TRUE,
            footer = NULL
        ))
        # browser()
        dashboard_data$out_bs <- balance_sheet2
        dashboard_data$out_is <- income_statement2
    })
    
    dashboard_data <- reactiveValues(out_bs = tibble(balance_sheet = character(), bs_base = double()),
                                     out_is = tibble(income_statement = character(), is_base = double()))
    
    output$balance_sheet <- DT::renderDT({
        DT::datatable(dashboard_data$out_bs,
                      colnames = c("Balance Sheet", "Base"), class = "strip", 
                      options = list(paging = FALSE, searching = FALSE, info = FALSE,
                                     rowCallback = DT::JS(rowCallback_bs)))
    })
    
    output$income_statement <- DT::renderDT({
        DT::datatable(dashboard_data$out_is,
                      colnames = c("Income Statement", "Base"), class = "strip",
                      options = list(paging = FALSE, searching = FALSE, info = FALSE,
                                     rowCallback = DT::JS(rowCallback_is)))
    })
    
    output$dl_on_df <- downloadHandler(
        filename = "onerousity_df.csv",
        content = function(file) {
            write.csv(test_data$onerosity_df, file)
        }
    )
})
