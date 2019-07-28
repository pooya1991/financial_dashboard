library(dplyr)
library("readxl")

calcs_base <- read_excel("data/PAA_Unearned_Model_dev.xlsx", sheet = "Calcs - Base")

year <- rep(2018:2044, each = 4)[-length(year)]
time <- seq(from = 0.25, to = 26.75, by = 0.25)


assetCashFlowRows <- c("Assets brought forward", "Profits declared", "Premium Received", "Premiums Paid", "Initial Expenses",
                       "Maintenance Expenses", "Initial Commission", "Renewal Commission", "Attritional Payments", 
                       "Large Payments", "Cat Payments", "ENIDs Payments", "Investment income", "Net Cashflow", "Asset EoP")

assetCashFlowRowstimeType <- c(NA, rep("SoP", 3), rep("EoP", 8), rep(NA, 3))
assetCashFlowRows <- c("time_type", "start position", time)

assetCashFlows <- 

  




  

