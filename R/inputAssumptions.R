library("readxl")
library(dplyr)
library(tidyr)
library(purrr)

input_assumptions <- read_excel("../data/PAA_Unearned_Model_dev.xlsx", sheet = "Input Assumptions")

recode_run_settings <- function(df) {
 df <- mutate(df, `Run Settings` = recode(`Run Settings`,
                                     "Projection Start Date" = "proj_start_date",
                                     "Projection Year" = "proj_year",
                                     "Projection Quarter" = "proj_quarter",
                                     "Projection Start Time (Years)" = "proj_start_time",
                                     "Currency" = "currency",
                                     "Projection Period (Years)" = "projection_period",
                                     "Underwriting Year" = "uw_year",
                                     "Underwriting Quarter" = "uw_quarter",
                                     "Type of contract (inwards vs outwards)" = "contract_type"
                                     ))
out_ls <- as.list(df[[2]]) %>% 
   set_names(df[[1]])
 out_ls$proj_start_date <- as.Date(as.integer(out_ls$proj_start_date), origin = "1899-12-31")
 out_ls
}

df_run_settings <- input_assumptions[c(2:9, 11), c(1,3)]
df_discount_assumptions_Q4_17_PFR <- input_assumptions[c(29, 31:37), c(2,4:111)]
df_discount_assumptions_Q1_18_PFR <- input_assumptions[c(29, 39:45), c(2,4:111)]
df_cashflow_timing <- input_assumptions[c(68:79), c(1,2)]



l_run_settings <- list(ProjectionStartDate = as.Date("2018-01-01"), ProjectionYear = 2018, ProjectionQuarter = "Q1",
                       ProjectionStartTimeYears = 0.25, Currency = "GBP", ProjectionPeriodYears = 25, UnderwritingYear = 25,
                       UnderwritingQuarter = "Q1", Typeofcontract = "Inwards")

term_quarters <- c(1:108)
n <- length(term_quarters)
year_updater <- function(term, initial_year) {
  year <- vector(length = n)
  year[1] <- initial_year
  for (i in 2:n) {
    j <- i - 1
    year[i] <- ifelse((term[i]/0.25) <= (4*(year[j] - initial_year + 1)), year[j], year[j] + 1)
  }
  return(year)
}
year <- year_updater(term = term_quarters, initial_year = 2018)
spot_rate_initial <- 0.02
spot_rate <- rep(spot_rate_initial, n)

forward_rate_updater <- function(term, spot) {
  forward <- vector(length = n)
  forward[1] <- spot[1]
  for (i in 2:n) {
    j <- i - 1
    forward[i] <- (((1 + spot[i])^term[i]) / ((1 +spot[j])^term[j])) - 1
  }
  return(forward)
}

forward_rate_updater(term_quarters, spot_rate)

t_year <- seq(0.25, by = 0.25, length.out = n)

discount_factor_EoP_updater <- function(spot, t, projection_quarter){
  discount <- (1 + spot)^(-t + ifelse(projection_quarter == "Q4", 0.75, ifelse(projection_quarter == "Q3", 0.5, ifelse(projection_quarter == "Q2", 025, 0))))
  return(discount)
}

projection_quarter <- "Q1"
discount_factor_MoP_updater <- function(spot, t, projection_quarter){
  discount <- (1 + spot)^(-t + ifelse(projection_quarter == "Q4", 0.75, ifelse(projection_quarter == "Q3", 0.5, ifelse(projection_quarter == "Q2", 025, 0))) + (0.25 / 4))
  return(discount)
}

discount_factor_SoP_updater <- function(EoP){
  discount <- c(1, EoP[-length(EoP)])
  return(discount)
}

discount_factor_SoP_updater(discount_factor_EoP_updater(spot_rate, t_year, projection_quarter))

df_discount <- t(data.frame(discount_factor_SoP_updater(discount_factor_EoP_updater(spot_rate, t_year, projection_quarter)),
                            discount_factor_EoP_updater(spot_rate, t_year, projection_quarter),
                            discount_factor_MoP_updater(spot_rate, t_year, projection_quarter)))
rownames(df_discount) <- c("SoP", "EoP", "MoP")




