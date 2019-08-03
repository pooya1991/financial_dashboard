source("R/calcs_base.R")


ifrs_base <- new.env()
asset_bs <- new.env(parent = ifrs_base)
liabilities_bs <- new.env(parent = ifrs_base)
net_asset_bs <- new.env(parent = ifrs_base)
capital_and_reserves_bs <- new.env(parent = ifrs_base)
profit_loss_bs <- new.env(parent = ifrs_base)

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
 


















