library(readxl)
library(dplyr)
library(tidyr)

CashflowsRun1premiums <- read_excel("data/PAA_Unearned_Model_dev.xlsx", sheet = "CashflowsRun2Premiums")
df_cashflow_premiums <- CashflowsRun1premiums[c(10:171), c(1:113)]

year <- as.numeric(df_cashflow_premiums[1, c(6:113)])
time <- as.numeric(df_cashflow_premiums[2, c(6:113)])

sign_detector <- function(cashflow_type, type_of_contract){
  cashflow_type <- pull(cashflow_type)
  type_of_contract <- as.character(type_of_contract)
  oposite_sign <- c("Premiums Received", "Premiums Paid", "Earned Premium (gross of commission)", "Earned Premium (net of commission)",
                    "Unearned Premium Reserve (gross of commission)", "Unearned Premium Reserve (net of commission)")
  m <- length(cashflow_type)
  x <- ifelse(cashflow_type %in% oposite_sign, -1, 1)
  sign <- ifelse(is.na(cashflow_type), NA,x * ifelse(type_of_contract == "Outwards", 1, -1))
  return(sign)
}

time_type_detector <- function(cashflow_type) {
  cashflow_type <- pull(cashflow_type)
  m <- length(cashflow_type)
  time <- rep(NA, m)
  for(i in 1:m) {
    cashflow_type_dummy <- strsplit(cashflow_type[i], "_")[[1]][1]
    dummy <- df_cashflow_timing[df_cashflow_timing[,1] == cashflow_type_dummy,2]
    if(nrow(dummy) > 0){
      time[i] <- pull(dummy)
    }else {
      time[i] <- NA
    }
  }
  return(time)
}

outpu_calculator <- function(input, table) {
  m <- nrow(input)
  output <- input
  for (i in 1:m) {
    output[i, ] <- input[i, ] * as.numeric(table[i, 4])
  }
  return(output)
}

discounting <- function(input, discount, table) {
  output <- input
  m <- nrow(input)
  for (i in 1:m) {
    output[i, ] <- input[i, ] * discount[pull(table[i, 3]),]
  }
  return(output)
}

############################ Quarterly Cashflow ###################################
df_cashflow_premiums_input <- df_cashflow_premiums[c(3:11), c(6:113)]
quarterly_cashflow_premiums <- df_cashflow_premiums[c(3:11), c(1:5)]

quarterly_cashflow_premiums[,3] <- time_type_detector(cashflow_type = quarterly_cashflow_premiums[,2])
quarterly_cashflow_premiums[,4] <- sign_detector(cashflow_type = quarterly_cashflow_premiums[,2], type_of_contract = df_run_settings[9,2])
quarterly_cashflow_premiums[,5] <- rowSums(df_cashflow_premiums_input)



############################ LIC Reserve #########################################
df_LIC_reserves_premiums_input <- df_cashflow_premiums[c(13:86), c(6:113)]
LIC_reserves_premiums <- df_cashflow_premiums[c(13:86), c(1:5)]

LIC_reserves_premiums[,3] <- time_type_detector(cashflow_type = LIC_reserves_premiums[,2])
LIC_reserves_premiums[,4] <- sign_detector(cashflow_type = LIC_reserves_premiums[,2], type_of_contract = df_run_settings[9,2])
LIC_reserves_premiums[,5] <- rowSums(df_LIC_reserves_premiums_input)


############################ LIC Reserves deterioration ###########################
df_LIC_reserves_deterioration_premiums_input <- df_cashflow_premiums[c(88:161), c(6:113)]
LIC_reserves_deterioration_premiums <- df_cashflow_premiums[c(88:161), c(1:5)]

LIC_reserves_deterioration_premiums[,3] <- time_type_detector(cashflow_type = LIC_reserves_deterioration_premiums[,2])
LIC_reserves_deterioration_premiums[,4] <- sign_detector(cashflow_type = LIC_reserves_deterioration_premiums[,2], type_of_contract = df_run_settings[9,2])
LIC_reserves_deterioration_premiums[,5] <- rowSums(df_LIC_reserves_deterioration_premiums_input)

############################ Quarterly Cashflows - Undiscounted ###################
quarterly_cashflow_undiscounted_premiums_rows <- pull(CashflowsRun1premiums[c(177:179), 2])
quarterly_cashflow_undiscounted_premiums <- quarterly_cashflow_premiums[pull(quarterly_cashflow_premiums[,2]) %in% quarterly_cashflow_undiscounted_premiums_rows,]

df_cashflow_cashflow_undiscounted_premiums_output <- rbind((df_cashflow_premiums_input[3, ] * as.numeric(quarterly_cashflow_undiscounted_premiums[1,4])), 
                                                       (df_cashflow_premiums_input[8, ] * as.numeric(quarterly_cashflow_undiscounted_premiums[2,4])),
                                                       (df_cashflow_premiums_input[9, ] * as.numeric(quarterly_cashflow_undiscounted_premiums[3,4])))

quarterly_cashflow_undiscounted_premiums[,5] <- rep(NA, nrow(quarterly_cashflow_undiscounted_premiums))



############################ LIC Reserves - Undiscounted #########################
quarterly_LIC_reserves_undiscounted_premiums <- LIC_reserves_premiums
df_quarterly_LIC_reserves_undiscounted_premiums_output <- outpu_calculator(input = df_LIC_reserves_premiums_input, table = quarterly_LIC_reserves_undiscounted_premiums)
quarterly_LIC_reserves_undiscounted_premiums[, 5] <- rowSums(df_cashflow_cashflow_undiscounted_premiums_output)


############################ Discount Rates ######################################
discount_rates_premiums <- CashflowsRun1premiums[c(333:335), c(2:4)]
df_discount_rates_premiums <- df_discount


############################ Quarterly Cashflows - Discounted ###################
df_discount_cashflow_premiums <- discounting(input = df_cashflow_cashflow_undiscounted_premiums_output, discount = df_discount_rates_premiums, 
                                         table = quarterly_cashflow_undiscounted_premiums)
discount_cashflow_premiums <- quarterly_cashflow_undiscounted_premiums


############################ LIC Reserves - Discounted ###################
df_discount_LIC_reserves_premiums <- discounting(input = df_quarterly_LIC_reserves_undiscounted_premiums_output, discount = df_discount_rates_premiums, 
                                         table = quarterly_LIC_reserves_undiscounted_premiums)
discount_LIC_reserves_premiums <- quarterly_LIC_reserves_undiscounted_premiums

