library(readxl)
library(dplyr)
library(tidyr)

CashflowsRun1Base <- read_excel("data/PAA_Unearned_Model_dev.xlsx", sheet = "CashflowsRun1Base")
df_cashflow_base <- CashflowsRun1Base[c(10:171), c(1:113)]

year <- as.numeric(df_cashflow_base[1, c(6:113)])
time <- as.numeric(df_cashflow_base[2, c(6:113)])

df_cashflow_base_input <- df_cashflow_base[c(3:11), c(6:113)]
quarterly_cashflow_base <- df_cashflow_base[c(3:11), c(1:5)]

sign_detector <- function(cashflow_type, type_of_contract){
  cashflow_type <- pull(cashflow_type)
  type_of_contract <- as.character(type_of_contract)
  oposite_sign <- c("Premiums Received", "Premiums Paid", "Earned Premium (gross of commission)", "Earned Premium (net of commission)",
                    "Unearned Premium Reserve (gross of commission)", "Unearned Premium Reserve (net of commission)")
  m <- length(cashflow_type)
  x <- ifelse(cashflow_type %in% oposite_sign, -1, 1)
  sign <- x * ifelse(type_of_contract == "Outwards", 1, -1)
  return(sign)
}

time_type_detector <- function(cashflow_type) {
  cashflow_type <- pull(cashflow_type)
  m <- length(cashflow_type)
  time <- rep(NA, m)
  for(i in 1:m) {
    time[i] <- as.character(df_cashflow_timing[df_cashflow_timing[,1] == cashflow_type[i],2])
  }
  time <- replace(time, "character(0)", NA)
  return(time)
}

time_cashflow <- time_type_detector(cashflow_type = quarterly_cashflow_base[,2])
sign_cashflow <- sign_detector(cashflow_type = quarterly_cashflow_base[,2], type_of_contract = df_run_settings[9,2])
total_cashflow <- rowSums(df_cashflow_base_input)

quarterly_cashflow_base[,5] <- total_cashflow


