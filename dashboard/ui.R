library(shiny)
library(dplyr)
library(tidyr)
library(purrr)
source("R/inputAssumptions.R")
# set the maximum upload size to 30 MB
options(shiny.maxRequestSize = 30*1024^2)

default_inputs <- recode_run_settings(df_run_settings)

shinyUI(fluidPage(
    shinyjs::useShinyjs(),
    titlePanel("PAA Unearned Model"),
    sidebarLayout(
        sidebarPanel(
            width = 3,
            dateInput("proj_start_date", "Projection Start Date", default_inputs$proj_start_date),
            numericInput("proj_year", "Projection Year", default_inputs$proj_year),
            selectInput("proj_quarter", "Projection Quarter", default_inputs$proj_quarter),
            numericInput("proj_start_time", "Projection Start Time (Years)", default_inputs$proj_start_time),
            selectInput("currency", "Currency", default_inputs$currency),
            numericInput("projection_period", "Projection Period (Years)", default_inputs$projection_period),
            numericInput("uw_year", "Underwriting Year", default_inputs$uw_quarter),
            selectInput("uw_quarter", "Underwriting Quarter", default_inputs$uw_quarter),
            selectInput("contract_type", "Type of Contract", default_inputs$contract_type)
        ),
        
        mainPanel(
            tabsetPanel(
                tabPanel(title = "balance_sheet",
                         DT::DTOutput("balance_sheet")
                ),
                tabPanel(title = "income_statement",
                         DT::DTOutput("income_statement")
                )
            )
        )
    )
))
