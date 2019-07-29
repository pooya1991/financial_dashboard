library(readxl)
library(dplyr)
library(tidyr)
library(FSA)

CashflowsRun1Base <- read_excel("data/PAA_Unearned_Model_dev.xlsx", sheet = "CashflowsRun1Base")
df_cashflow_base <- CashflowsRun1Base[c(10:171), c(1:113)]

year <- as.numeric(df_cashflow_base[1, c(6:113)])
time <- as.numeric(df_cashflow_base[2, c(6:113)])

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

pv_calculator <- function(input) {
  otput <- input
  m <- nrow(input)
  for(i in 1:m) {
    output[i, ] <- -rcumsum(as.numeric(input[i, ]))
  }
  return(output)
}

############################ Quarterly Cashflow ##############################################
df_cashflow_base_input <- df_cashflow_base[c(3:11), c(6:113)]
quarterly_cashflow_base <- df_cashflow_base[c(3:11), c(1:5)]

quarterly_cashflow_base[,3] <- time_type_detector(cashflow_type = quarterly_cashflow_base[,2])
quarterly_cashflow_base[,4] <- sign_detector(cashflow_type = quarterly_cashflow_base[,2], type_of_contract = df_run_settings[9,2])
quarterly_cashflow_base[,5] <- rowSums(df_cashflow_base_input)



############################ LIC Reserve #####################################################
df_LIC_reserves_base_input <- df_cashflow_base[c(13:86), c(6:113)]
LIC_reserves_base <- df_cashflow_base[c(13:86), c(1:5)]

LIC_reserves_base[,3] <- time_type_detector(cashflow_type = LIC_reserves_base[,2])
LIC_reserves_base[,4] <- sign_detector(cashflow_type = LIC_reserves_base[,2], type_of_contract = df_run_settings[9,2])
LIC_reserves_base[,5] <- rowSums(df_LIC_reserves_base_input)


############################ LIC Reserves deterioration #######################################
df_LIC_reserves_deterioration_base_input <- df_cashflow_base[c(88:161), c(6:113)]
LIC_reserves_deterioration_base <- df_cashflow_base[c(88:161), c(1:5)]

LIC_reserves_deterioration_base[,3] <- time_type_detector(cashflow_type = LIC_reserves_deterioration_base[,2])
LIC_reserves_deterioration_base[,4] <- sign_detector(cashflow_type = LIC_reserves_deterioration_base[,2], type_of_contract = df_run_settings[9,2])
LIC_reserves_deterioration_base[,5] <- rowSums(df_LIC_reserves_deterioration_base_input)


############################ Quarterly Cashflows - Undiscounted ################################
quarterly_cashflow_undiscounted_base_rows <- pull(CashflowsRun1Base[c(177:179), 2])
quarterly_cashflow_undiscounted_base <- quarterly_cashflow_base[pull(quarterly_cashflow_base[,2]) %in% quarterly_cashflow_undiscounted_base_rows,]

df_cashflow_cashflow_undiscounted_base_output <- rbind((df_cashflow_base_input[3, ] * as.numeric(quarterly_cashflow_undiscounted_base[1,4])), 
                                                       (df_cashflow_base_input[8, ] * as.numeric(quarterly_cashflow_undiscounted_base[2,4])),
                                                       (df_cashflow_base_input[9, ] * as.numeric(quarterly_cashflow_undiscounted_base[3,4])))

quarterly_cashflow_undiscounted_base[,5] <- rep(NA, nrow(quarterly_cashflow_undiscounted_base))



############################ LIC Reserves deterioration - Undiscounted #########################
quarterly_LIC_reserves_undiscounted_base <- LIC_reserves_base
df_quarterly_LIC_reserves_undiscounted_base_output <- outpu_calculator(input = df_LIC_reserves_deterioration_base_input, table = quarterly_LIC_reserves_undiscounted_base)
quarterly_LIC_reserves_undiscounted_base[, 5] <- rowSums(df_cashflow_cashflow_undiscounted_base_output)


############################ LIC Reserves - Undiscounted ########################################
#quarterly_LIC_reservesdeterioration__undiscounted_base <- LIC_reserves_deterioration_base
#df_quarterly_LIC_reserves_deterioration_undiscounted_base_output <- outpu_calculator(input = LIC_reserves_deterioration_base, table = quarterly_LIC_reservesdeterioration__undiscounted_base)
#quarterly_LIC_reservesdeterioration__undiscounted_base[, 5] <- rowSums(df_cashflow_cashflow_undiscounted_base_output)



############################ Discount Rates ####################################################
discount_rates_base <- CashflowsRun1Base[c(333:335), c(2:4)]
df_discount_rates_base <- df_discount


############################ Quarterly Cashflows - Discounted ###################################
discount_cashflow_base <- quarterly_cashflow_undiscounted_base
discount_cashflow_base[1, 3] <- "EoP"
df_discount_cashflow_base <- discounting(input = df_cashflow_cashflow_undiscounted_base_output, discount = df_discount_rates_base, 
                                         table = discount_cashflow_base)


############################ LIC Reserves - Discounted ##########################################
df_discount_LIC_reserves_base <- discounting(input = df_quarterly_LIC_reserves_undiscounted_base_output, discount = df_discount_rates_base, 
                                         table = quarterly_LIC_reserves_undiscounted_base)
discount_LIC_reserves_base <- quarterly_LIC_reserves_undiscounted_base



############################ LIC Reserves deterioration - Discounted #############################
#df_discount_LIC_reserves_deterioration_base <- discounting(input = df_quarterly_LIC_reserves_deterioration_undiscounted_base_output, discount = df_discount_rates_base, 
#                                            table = quarterly_LIC_reserves_deterioration_undiscounted_base)
#discount_LIC_reserves_deterioration_base <- quarterly_LIC_reserves_deterioration_undiscounted_base





#################################################################################################
#################################### PV of cashflows ############################################
#################################################################################################

############################ Quarterly Cashflows - PV ###########################################
df_pv_cashflow_base <- pv_calculator(input = df_discount_cashflow_base)
pv_cashflow_base <- discount_cashflow_base

############################ LIC Reserves deterioration - PV ##################################################
df_pv_LIC_reserves_base <- pv_calculator(input = df_discount_LIC_reserves_base)
pv_LIC_reserves_base <- discount_LIC_reserves_base

############################ LIC Reserves - PV ##################################################
df_pv_LIC_reserves_deterioration_base <- pv_calculator(input = df_discount_LIC_reserves_deterioration_base)
pv_LIC_reserves_deterioration_base <- discount_LIC_reserves_deterioration_base









