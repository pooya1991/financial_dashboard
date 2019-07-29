library(dplyr)
library(readxl)

calcs_base <- read_excel("data/PAA_Unearned_Model_dev.xlsx", sheet = "Calcs - Base")


year <- rep(2018:2044, each = 4)[-length(year)]
time <- seq(from = 0.25, to = 26.75, by = 0.25)



############################################ assetCashFlows ########################################################
assetCashFlows_base <- calcs_base[7:21, c(1,3)]
df_asset_cashflow_base <- calcs_base[7:21, c(5:111)]



############################################ Liability Cashflows ###################################################
liabilityCashflows_base <- calcs_base[24:39, c(1,3)]
df_liabilityCashflows_base <- calcs_base[24:39, c(5:111)]



############################################ Risk Adjustment #######################################################
riskAdjustment_base <- calcs_base[c(42,44,46,48,50,52,53,55,56,58), c(2,3)]
df_riskAdjustment_base <- calcs_base[c(42,44,46,48,50,52,53,55,56,58), c(5:111)]



############################################ Liability for incurred claims #########################################
Liability_for_incurred_claims_base <- calcs_base[c(61:67, 69:75, 77:83, 85:91, 93:99, 101:107,109:115, 117:123, 126:131, 133:139), 3]
df_Liability_for_incurred_claims_base <- calcs_base[c(61:67, 69:75, 77:83, 85:91, 93:99, 101:107,109:115, 117:123, 126:131, 133:139), c(5:111)]



############################################ Liability for remaining coverage ######################################
gross_of_acquisition_expenses_base <- calcs_base[145:149, 3]
df_gross_of_acquisition_expenses_base <- calcs_base[145:149, c(5:111)]

net_of_acquisition_expenses_base <- calcs_base[153:157, 3]
df_net_of_acquisition_expenses_base <- calcs_base[153:157, c(5:111)]



############################################ Amortisation of acquisition expenses ##################################
amortisation_of_acquisition_expenses_base <- calcs_base[161:163, 3]
df_amortisation_of_acquisition_expenses_base <- calcs_base[161:163, c(5:111)]


  

