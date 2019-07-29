library(dplyr)

pth <- "data/PAA_Unearned_Model_dev.xlsx"
sh_calcs_base <- readxl::read_excel(pth, sheet = "Calcs - Base")

time <- as.matrix(sh_calcs_base[4, -1:-4]) %>% drop()
year <- as.matrix(sh_calcs_base[3, -1:-4]) %>% 
  drop() %>% as.integer() %>% set_names(time)

idxr_asset_cashflows <- 7:21 
asset_cashflows <- as.matrix(sh_calcs_base[idxr_asset_cashflows, c(5:111)])
rownames(asset_cashflows) <- sh_calcs_base[idxr_asset_cashflows, 3][[1]]
colnames(asset_cashflows) <- time

idxr_liability_cashflows <- 24:39
liability_cashflows <- as.matrix(sh_calcs_base[idxr_liability_cashflows, c(5:111)])
rownames(liability_cashflows) <- sh_calcs_base[idxr_liability_cashflows, 3][[1]]
colnames(liability_cashflows) <- time

idxr_risk_adjustment <- c(42,44,46,48,50,52,53,55,56,58)
risk_adjustment <- as.matrix(sh_calcs_base[idxr_risk_adjustment, c(5:111)])
rownames(risk_adjustment) <- sh_calcs_base[idxr_risk_adjustment, 3][[1]]
colnames(risk_adjustment) <- time

idxr_liability_for_incurred_claims <- c(61:67, 69:75, 77:83, 85:91, 93:99,
                                             101:107,109:115, 117:123, 126:131, 133:139)
liability_for_incurred_claims <- sh_calcs_base[idxr_liability_for_incurred_claims, c(5:111)]

############################################ Liability for incurred claims #########################################
liability_for_incurred_claims_base <- calcs_base[, 3]
df_Liability_for_incurred_claims_base <- calcs_base[c(61:67, 69:75, 77:83, 85:91, 93:99, 101:107,109:115, 117:123, 126:131, 133:139), c(5:111)]



############################################ Liability for remaining coverage ######################################
gross_of_acquisition_expenses_base <- calcs_base[145:149, 3]
df_gross_of_acquisition_expenses_base <- calcs_base[145:149, c(5:111)]

net_of_acquisition_expenses_base <- calcs_base[153:157, 3]
df_net_of_acquisition_expenses_base <- calcs_base[153:157, c(5:111)]



############################################ Amortisation of acquisition expenses ##################################
amortisation_of_acquisition_expenses_base <- calcs_base[161:163, 3]
df_amortisation_of_acquisition_expenses_base <- calcs_base[161:163, c(5:111)]


  

