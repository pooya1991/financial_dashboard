library(shiny)
library(dplyr)
library(tidyr)
library(purrr)
source("../R/inputAssumptions.R")
# set the maximum upload size to 30 MB
options(shiny.maxRequestSize = 30*1024^2)

qts <- c("Q1", "Q2", "Q3", "Q4")

shinyUI(fluidPage(
    shinyjs::useShinyjs(),
    titlePanel("PAA Unearned Model"),
    # sidebarLayout(
    sidebarPanel(
        width = 3,
        fileInput("xlsx_file", "Input File"),
        div(id = "run_settings", h3("Run Settings"),
            dateInput("proj_start_date", "Projection Start Date", NA_integer_),
            numericInput("proj_year", "Projection Year", NA_integer_),
            selectInput("proj_quarter", "Projection Quarter", choices = qts),
            numericInput("proj_start_time", "Projection Start Time (Years)", NA_real_),
            selectInput("currency", "Currency", NA_character_),
            numericInput("projection_period", "Projection Period (Years)", NA_integer_),
            numericInput("uw_year", "Underwriting Year", NA_integer_),
            selectInput("uw_quarter", "Underwriting Quarter", qts),
            selectInput("contract_type", "Type of Contract", c("Inwards", "Outwards"))
        )
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
    # )
))
