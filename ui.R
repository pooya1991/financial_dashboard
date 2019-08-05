library(shiny)
library(shinydashboard)
library(dplyr)
library(tidyr)
library(purrr)
source("R/inputAssumptions.R")
# set the maximum upload size to 30 MB
options(shiny.maxRequestSize = 30*1024^2)

qts <- c("Q1", "Q2", "Q3", "Q4")

shinyUI(dashboardPage(
    dashboardHeader(title = "PAA Unearned Model"),
    dashboardSidebar(
        tags$style(type="text/css", "#dl_on_df {color: black;font-family: Courier New}"),
        tabsetPanel(
            tabPanel(title = "Run Settings",
                     div(id = "run_settings",
                         dateInput("proj_start_date", "Projection Start Date", NA_integer_),
                         numericInput("proj_year", "Projection Year", NA_integer_),
                         selectInput("proj_quarter", "Projection Quarter", choices = qts),
                         numericInput("proj_start_time", "Projection Start Time (Years)", NA_real_),
                         selectInput("currency", "Currency", NA_character_),
                         numericInput("projection_period", "Projection Period (Years)", NA_integer_),
                         numericInput("uw_year", "Underwriting Year", NA_integer_),
                         selectInput("uw_quarter", "Underwriting Quarter", qts)
                     )
            ),
            tabPanel(title = "Input Files",
                     fileInput("input_file", "Upload Input Assumptions"),
                     fileInput("pas_data", "Upload Pas Data"),
                     br(),
                     tags$div(
                         HTML("<h3 style = 'padding-left:15px'>Run:</h3>")
                     ),
                     actionButton("run_onerousity", "Onerousity Test", width = 150),
                     br(),
                     actionButton("coh_form", "Cohort Formation", width = 150),
                     br(),
                     actionButton("run1_base", "Run1 - Base", width = 150),
                     br(),br(),br(),
                     actionButton("console", "Go to console", width = 150)
            ),
            tabPanel(title = "Downloads",
                     # downloadButton("dl_att_payments", "Attritional Payments", width = 150,  class ="butt1"),
                     downloadButton("dl_on_df", "Onerousity Data", width = 150)
            )
        )
    ),
    
    dashboardBody(
        shinyjs::useShinyjs(),
        tabsetPanel(
            tabPanel(title = "balance_sheet",
                     DT::DTOutput("balance_sheet"),
                     downloadButton("dl_btn", "download")
            ),
            tabPanel(title = "income_statement",
                     DT::DTOutput("income_statement")
            )
        )
    )
))
