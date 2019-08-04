source("R/calcs_base.R")


ifrs_base <- new.env()
asset_bs <- new.env(parent = ifrs_base)
liabilities_bs <- new.env(parent = ifrs_base)
net_asset_bs <- new.env(parent = ifrs_base)
capital_and_reserves_bs <- new.env(parent = ifrs_base)
profit_loss_bs <- new.env(parent = ifrs_base)

top_is <- new.env(parent = ifrs_base)
mid_is <- new.env(parent = ifrs_base)
bot_is <- new.env(parent = ifrs_base)

# Balance Sheet  ----------------------------------------------------------

local(envir = asset_bs, {
    Invested <- 0
    FVTOCI <- 0
    `FVTP&L` <- ac_cf$`Net Cashflow`[1]
    `Total Assets` <- FVTOCI + `FVTP&L`
})

local(envir = liabilities_bs, {
    `Risk Adjustment` <- ra_cf$`Risk Adjustment EoP`[1]
    `Liability for incurred claims` <- lfic_cf$`Liability at EoP df`["Liability at EoP", 1]
    `Liability for remaining coverage` <- lfrc_cf$`Net of acquisition expenses`["Liability at EoP", 1]
    `Loss Component` <- 0
    `Total Liability` <- sum(`Risk Adjustment`, `Liability for incurred claims`, `Liability for remaining coverage`,
                              `Loss Component`)
})

local(envir = net_asset_bs, {
    `Net assets` <- asset_bs$`Total Assets` - liabilities_bs$`Total Liability`
})

local(envir = capital_and_reserves_bs, {
    `Liabilities & Equity` <- net_asset_bs$`Net assets` + liabilities_bs$`Total Liability `
})

local(envir = profit_loss_bs, {
    `Profit (Loss) in year` <- asset_bs$`Total Assets` - liabilities_bs$`Total Liability`
})
 



# Income statement  -------------------------------------------------------

local(envir = top_is, {
    `Allocation of premium for coverage period` <- -lfrc_cf$`Gross of acquisition expenses`["Release", 1]
    `Adjustment for loss component` <- 0
    `Total Insurance contract revenue` <- sum(`Allocation of premium for coverage period`, `Adjustment for loss component`)
})


local(envir = mid_is, {
    `Incurred claims emerging during the quarter` <- -sum(lfic_cf$`Incurred claims emerging during the quarter df`[c("Attritional Payments", 
                                                                                                                     "Large Payments", 
                                                                                                                     "Cat Payments", 
                                                                                                                     "ENIDs Payments"),1], lfic_cf$`Deterioration on incurred claims emerging during the quarter df`[c("Attritional Payments", 
                                                                                                                                                                                                                       "Large Payments", 
                                                                                                                                                                                                                       "Cat Payments", 
                                                                                                                                                                                                                       "ENIDs Payments"),1])
    
    `Expenses incurred` <- -sum(lfic_cf$`Deterioration on incurred claims emerging during the quarter df`[c("Maintenance Expenses", "Renewal Commission"),1], lfic_cf$`Incurred claims emerging during the quarter df`[c("Maintenance Expenses", "Renewal Commission"),1])
    
    `Amortisation of acquisition costs` <- aoae_cf$`Amortisation of acquisition expenses`[1]
    
    `Release / Emergence of risk adjustment` <- -sum(ra_cf$risk_adjustment[c("Emergence of risk adjustment during the quarter", "Release in RA", 
                                                                             "Change in RA"), 1])
    
    `Changes in estimates of future cash flows` <- 0
    `Loss Component` <- 0
    `Insurance Service Expense` <- sum(`Loss Component`, `Changes in estimates of future cash flows`, 
                                       `Release / Emergence of risk adjustment`, `Amortisation of acquisition costs`, 
                                       `Expenses incurred`, `Incurred claims emerging during the quarter`)
    `Insurance Service Result` <- `Insurance Service Expense` + top_is$`Total Insurance contract revenue`
})


local(envir = bot_is, {
    `Investment income` <- ac_cf$`Investment income`[1]
    `Interest on insurance liability` <- -(lfic_cf$`Interest accrued on starting reserve`[1] * 0 + lfic_cf$`Interest accrued on liability converted to LIC`[1] + 
        lfic_cf$`Revaluation of future cashflows for Emerging experience`[1] + lfic_cf$`Interest on deteriorations`[1] + 0)
    `Insurance Finance Expenses` <- `Investment income` + `Interest on insurance liability`
    `Profit or Loss` <- `Insurance Finance Expenses` + mid_is$`Insurance Service Result`
})











