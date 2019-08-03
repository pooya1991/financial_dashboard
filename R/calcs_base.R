library(dplyr)

source("R/cashflows_run1_base.R")


calc_base <- new.env()
ac_cf <- new.env(parent = calc_base)
lc_cf <- new.env(parent = calc_base)
lfic_cf <- new.env(parent = calc_base)
lfrc_cf <- new.env(parent = calc_base)
aoae_cf <- new.env(parent = calc_base)
# Asset Cashflows --------------------------------------------------------

local(envir = ac_cf, {
    `Premiums Received` <- qt_cf$cashflows["Premiums Received", ]
    `Initial Expenses` <- rep(0, length(`Premium Received`))
    `Maintenance Expenses` <- rep(0, length(`Premium Received`))
    `Initial Commission` <- -qt_cf$cashflows["Initial Commission", ]
    `Renewal Commission` <- rep(0, length(`Premium Received`))
    `Attritional Payments` <- colSums(qt_cf_undiscounted$attr_payments_mat, na.rm = T)
    `Large Payments` <- rep(0, length(`Renewal Commission`))
    `Cat Payments` <- rep(0, length(`Renewal Commission`))
    `ENIDs Payments` <- rep(0, length(`Renewal Commission`))
    `Investment income` <- rep(0, length(`Renewal Commission`))
    asset_cashflow <- rbind(`Premiums Received`, `Initial Expenses`, `Maintenance Expenses`, `Initial Commission`,
                            `Renewal Commission`, `Attritional Payments`, `Large Payments`, `Cat Payments`,
                            `ENIDs Payments`, `Investment income`)
    
    `Net Cashflow` <- colSums(asset_cashflow, na.rm = T)
    `Asset EoP` <- cumsum(`Net Cashflow`)
    
    asset_cashflow <- rbind(asset_cashflow, `Net Cashflow`, `Asset EoP`)
})


# liability Cashflows --------------------------------------------------------

local(envir = lc_cf, {
    `Premiums Received` <- qt_cf$cashflows["Premiums Received", ]
    `Initial Expenses` <- rep(0, length(`Premium Received`))
    `Maintenance Expenses` <- rep(0, length(`Premium Received`))
    `Initial Commission` <- -qt_cf$cashflows["Initial Commission", ]
    `Renewal Commission` <- rep(0, length(`Premium Received`))
    `Attritional Payments` <- colSums(qt_cf_undiscounted$attr_payments_mat, na.rm = T)
    `Large Payments` <- rep(0, length(`Renewal Commission`))
    `Cat Payments` <- rep(0, length(`Renewal Commission`))
    `ENIDs Payments` <- rep(0, length(`Renewal Commission`))
    `Investment income` <- rep(0, length(`Renewal Commission`))
    Unwind <- rep(0, length(`Renewal Commission`)) #TODO fix this issue
    liability_cashflow <- rbind(`Premiums Received`, `Initial Expenses`, `Maintenance Expenses`, `Initial Commission`,
                            `Renewal Commission`, `Attritional Payments`, `Large Payments`, `Cat Payments`,
                            `ENIDs Payments`, `Investment income`)
    `Net Cashflow (SoP)` <- timing_sum(liability_cashflow, "SoP")
    `Net Cashflow (MoP)` <- timing_sum(liability_cashflow, "MoP")
    `Net Cashflow (EoP)` <- timing_sum(liability_cashflow, "EoP")
    `Net Cashflow` <- colSums(liability_cashflow, na.rm = T)
    `Asset EoP` <- cumsum(`Net Cashflow`)
    
    liability_cashflow <- rbind(liability_cashflow, `Net Cashflow (SoP)`, `Net Cashflow (MoP)`, `Net Cashflow (EoP)`, `Net Cashflow`, `Asset EoP`)
})


# Risk Adjustment Cashflows --------------------------------------------------------

local(envir = ra_cf, {
    `Risk Adjustment` <- rep(0, 16)
    `Interest accrued on opening RA` <- rep(0, 16)
    `Emergence of risk adjustment during the quarter` <- rep(0, 16)
    `Interest accrued on emerging risk adjustment` <- rep(0, 16)
    `Release in RA` <- rep(0, 16)
    `Change in estimate of risk adjustment` <- rep(0, 16)
    `Revaluation of risk adjustment for change in economic assumptions` <- rep(0, 16)
    `Change in RA` <- rep(0, 16)
    `Interest on change in RA` <- rep(0, 16)
    `Risk Adjustment` <- rep(0, 16)
    risk_adjustment <- rbind(`Risk Adjustment`, `Interest accrued on opening RA`, `Emergence of risk adjustment during the quarter`,
                             `Interest accrued on emerging risk adjustment`, `Release in RA`, `Change in estimate of risk adjustment`,
                             `Revaluation of risk adjustment for change in economic assumptions`, `Change in RA`,
                             `Interest on change in RA`, `Risk Adjustment`)
})


# Liability for incurred claims --------------------------------------------------------

local(envir = lfic_cf, {
    # Incurred claims emerging during the quarter
    `Maintenance Expenses` <- rep(0, ncol(pv_of_cf$attr_payments_mat))
    `Renewal Commission` <- rep(0, ncol(pv_of_cf$attr_payments_mat))
    `Attritional Payments` <- c(diag(pv_of_cf$attr_payments_mat), rep(0, ncol(pv_of_cf$attr_payments_mat) - min(dim(pv_of_cf$attr_payments_mat))))
    `Large Payments` <- rep(0, ncol(pv_of_cf$attr_payments_mat))
    `Cat Payments` <- rep(0, ncol(pv_of_cf$attr_payments_mat))
    `ENIDs Payments` <- rep(0, ncol(pv_of_cf$attr_payments_mat))
    `Incurred claims emerging during the quarter df` <- rbind(`Maintenance Expenses`, `Renewal Commission`, `Attritional Payments`,
                                                              `Large Payments`, `Cat Payments`, `ENIDs Payments`)
    
    `Incurred claims emerging during the quarter` <- colSums(`Incurred claims emerging during the quarter df`, na.rm = TRUE)
    `Incurred claims emerging during the quarter df` <- rbind(`Incurred claims emerging during the quarter df`, `Incurred claims emerging during the quarter`)
    
    
    # Interest accrued on liability converted to LIC
    `Maintenance Expenses` <- `Incurred claims emerging during the quarter df`["Maintenance Expenses",] * (((1 + disc_assump["Spot Rate", ])^0.25) - 1)
    `Renewal Commission` <- `Incurred claims emerging during the quarter df`["Renewal Commission",] * (((1 + disc_assump["Spot Rate", ])^0.25) - 1)
    `Attritional Payments` <- `Incurred claims emerging during the quarter df`["Attritional Payments",] * (((1 + disc_assump["Spot Rate", ])^0.25) - 1)
    `Large Payments` <- `Incurred claims emerging during the quarter df`["Large Payments",] * (((1 + disc_assump["Spot Rate", ])^0.25) - 1)
    `Cat Payments` <- `Incurred claims emerging during the quarter df`["Cat Payments",] * (((1 + disc_assump["Spot Rate", ])^0.25) - 1)
    `ENIDs Payments` <- `Incurred claims emerging during the quarter df`["ENIDs Payments",] * (((1 + disc_assump["Spot Rate", ])^0.25) - 1)
    `Interest accrued on liability converted to LIC df` <- rbind(`Maintenance Expenses`, `Renewal Commission`, `Attritional Payments`,
                                                                 `Large Payments`, `Cat Payments`, `ENIDs Payments`)
    
    `Interest accrued on liability converted to LIC` <- colSums(`Interest accrued on liability converted to LIC df`, na.rm = TRUE)
    `Interest accrued on liability converted to LIC df` <- rbind(`Interest accrued on liability converted to LIC df`, `Interest accrued on liability converted to LIC`)
    colnames(`Interest accrued on liability converted to LIC df`) <- NULL
    
    # Deterioration on incurred claims emerging during the quarter
    `Maintenance Expenses` <- rep(0, ncol(pv_of_cf$attr_payments_mat))
    `Renewal Commission` <- rep(0, ncol(pv_of_cf$attr_payments_mat))
    `Attritional Payments` <- rep(0, ncol(pv_of_cf$attr_payments_mat))
    `Large Payments` <- rep(0, ncol(pv_of_cf$attr_payments_mat))
    `Cat Payments` <- rep(0, ncol(pv_of_cf$attr_payments_mat))
    `ENIDs Payments` <- rep(0, ncol(pv_of_cf$attr_payments_mat))
    `Deterioration on incurred claims emerging during the quarter df` <- rbind(`Maintenance Expenses`, `Renewal Commission`, `Attritional Payments`,
                                                                               `Large Payments`, `Cat Payments`, `ENIDs Payments`)
    
    `Deterioration on incurred claims emerging during the quarter` <- colSums(`Deterioration on incurred claims emerging during the quarter df`, na.rm = TRUE)
    `Deterioration on incurred claims emerging during the quarter df` <- rbind(`Deterioration on incurred claims emerging during the quarter df`, `Deterioration on incurred claims emerging during the quarter`)
    
    
    # Interest on deteriorations
    `Maintenance Expenses` <- rep(0, ncol(pv_of_cf$attr_payments_mat))
    `Renewal Commission` <- rep(0, ncol(pv_of_cf$attr_payments_mat))
    `Attritional Payments` <- rep(0, ncol(pv_of_cf$attr_payments_mat))
    `Large Payments` <- rep(0, ncol(pv_of_cf$attr_payments_mat))
    `Cat Payments` <- rep(0, ncol(pv_of_cf$attr_payments_mat))
    `ENIDs Payments` <- rep(0, ncol(pv_of_cf$attr_payments_mat))
    `Interest on deteriorations df` <- rbind(`Maintenance Expenses`, `Renewal Commission`, `Attritional Payments`,
                                             `Large Payments`, `Cat Payments`, `ENIDs Payments`)
    
    `Interest on deteriorations` <- colSums(`Interest on deteriorations df`, na.rm = TRUE)
    `Interest on deteriorations df` <- rbind(`Interest on deteriorations df`, `Interest on deteriorations`)
    
    
    # Revaluation of future cashflows for Emerging experience
    `Maintenance Expenses` <- rep(0, ncol(pv_of_cf$attr_payments_mat))
    `Renewal Commission` <- rep(0, ncol(pv_of_cf$attr_payments_mat))
    `Attritional Payments` <- rep(0, ncol(pv_of_cf$attr_payments_mat))
    `Large Payments` <- rep(0, ncol(pv_of_cf$attr_payments_mat))
    `Cat Payments` <- rep(0, ncol(pv_of_cf$attr_payments_mat))
    `ENIDs Payments` <- rep(0, ncol(pv_of_cf$attr_payments_mat))
    `Revaluation of future cashflows for Emerging experience df` <- rbind(`Maintenance Expenses`, `Renewal Commission`, `Attritional Payments`,
                                                                          `Large Payments`, `Cat Payments`, `ENIDs Payments`)
    
    `Revaluation of future cashflows for Emerging experience` <- colSums(`Revaluation of future cashflows for Emerging experience df`, na.rm = TRUE)
    `Revaluation of future cashflows for Emerging experience df` <- rbind(`Revaluation of future cashflows for Emerging experience df`, `Revaluation of future cashflows for Emerging experience`)
    
    
    # Revaluation of future cashflows for change in economic assumptions
    `Maintenance Expenses`  <- rep(0, ncol(pv_of_cf$attr_payments_mat))
    `Renewal Commission` <- rep(0, ncol(pv_of_cf$attr_payments_mat))
    `Attritional Payments` <- rep(0, ncol(pv_of_cf$attr_payments_mat))
    `Large Payments` <- rep(0, ncol(pv_of_cf$attr_payments_mat))
    `Cat Payments` <- rep(0, ncol(pv_of_cf$attr_payments_mat))
    `ENIDs Payments` <- rep(0, ncol(pv_of_cf$attr_payments_mat))
    `Revaluation of future cashflows for change in economic assumptions df` <- rbind(`Maintenance Expenses`, `Renewal Commission`, `Attritional Payments`,
                                                                                     `Large Payments`, `Cat Payments`, `ENIDs Payments`)
    
    `Revaluation of future cashflows for change in economic assumptions` <- colSums(`Revaluation of future cashflows for change in economic assumptions df`, na.rm = TRUE)
    `Revaluation of future cashflows for change in economic assumptions df` <- rbind(`Revaluation of future cashflows for change in economic assumptions df`, `Revaluation of future cashflows for change in economic assumptions`)
    
    
    # Payments
    `Maintenance Expenses` <- ac_cf$`Maintenance Expenses`
    `Renewal Commission` <- ac_cf$`Renewal Commission`
    `Attritional Payments` <- ac_cf$`Attritional Payments`
    `Large Payments` <- ac_cf$`Large Payments`
    `Cat Payments` <- ac_cf$`Cat Payments`
    `ENIDs Payments` <- ac_cf$`ENIDs Payments`
    `Payments df` <- rbind(`Maintenance Expenses`, `Renewal Commission`, `Attritional Payments`,
                           `Large Payments`, `Cat Payments`, `ENIDs Payments`)
    
    `Payments` <- colSums(`Payments df`, na.rm = TRUE)
    `Payments df` <- rbind(`Payments df`, `Payments`)
    
    
    # Liability at EoP
    `Maintenance Expenses` <- `Payments df`["Maintenance Expenses", ] + `Revaluation of future cashflows for change in economic assumptions df`["Maintenance Expenses", ] + `Revaluation of future cashflows for Emerging experience df`["Maintenance Expenses", ] +
        `Interest on deteriorations df`["Maintenance Expenses", ] + `Deterioration on incurred claims emerging during the quarter df`["Maintenance Expenses", ] + `Interest accrued on liability converted to LIC df`["Maintenance Expenses", ] +
        `Incurred claims emerging during the quarter df`["Maintenance Expenses", ]
    `Renewal Commission` <- `Payments df`["Renewal Commission", ] + `Revaluation of future cashflows for change in economic assumptions df`["Renewal Commission", ] + `Revaluation of future cashflows for Emerging experience df`["Renewal Commission", ] +
        `Interest on deteriorations df`["Renewal Commission", ] + `Deterioration on incurred claims emerging during the quarter df`["Renewal Commission", ] + `Interest accrued on liability converted to LIC df`["Renewal Commission", ] +
        `Incurred claims emerging during the quarter df`["Renewal Commission", ]
    `Attritional Payments` <- `Payments df`["Attritional Payments", ] + `Revaluation of future cashflows for change in economic assumptions df`["Attritional Payments", ] + `Revaluation of future cashflows for Emerging experience df`["Attritional Payments", ] +
        `Interest on deteriorations df`["Attritional Payments", ] + `Deterioration on incurred claims emerging during the quarter df`["Attritional Payments", ] + `Interest accrued on liability converted to LIC df`["Attritional Payments", ] +
        `Incurred claims emerging during the quarter df`["Attritional Payments", ]
    `Large Payments` <- `Payments df`["Large Payments", ] + `Revaluation of future cashflows for change in economic assumptions df`["Large Payments", ] + `Revaluation of future cashflows for Emerging experience df`["Large Payments", ] +
        `Interest on deteriorations df`["Large Payments", ] + `Deterioration on incurred claims emerging during the quarter df`["Large Payments", ] + `Interest accrued on liability converted to LIC df`["Large Payments", ] +
        `Incurred claims emerging during the quarter df`["Large Payments", ]
    `Cat Payments` <- `Payments df`["Cat Payments", ] + `Revaluation of future cashflows for change in economic assumptions df`["Cat Payments", ] + `Revaluation of future cashflows for Emerging experience df`["Cat Payments", ] +
        `Interest on deteriorations df`["Cat Payments", ] + `Deterioration on incurred claims emerging during the quarter df`["Cat Payments", ] + `Interest accrued on liability converted to LIC df`["Cat Payments", ] +
        `Incurred claims emerging during the quarter df`["Cat Payments", ]
    `ENIDs Payments` <- `Payments df`["ENIDs Payments", ] + `Revaluation of future cashflows for change in economic assumptions df`["ENIDs Payments", ] + `Revaluation of future cashflows for Emerging experience df`["ENIDs Payments", ] +
        `Interest on deteriorations df`["ENIDs Payments", ] + `Deterioration on incurred claims emerging during the quarter df`["ENIDs Payments", ] + `Interest accrued on liability converted to LIC df`["ENIDs Payments", ] +
        `Incurred claims emerging during the quarter df`["ENIDs Payments", ]
    `Liability at EoP df` <- rbind(`Maintenance Expenses`, `Renewal Commission`, `Attritional Payments`,
                                   `Large Payments`, `Cat Payments`, `ENIDs Payments`)
    
    `Liability at EoP` <- colSums(`Liability at EoP df`, na.rm = TRUE)
    `Liability at EoP df` <- rbind(`Liability at EoP df`, `Liability at EoP`)
    
    
    # Liability at SoP
    `Liability at SoP df` <- cbind(rep(0, nrow(`Liability at EoP df`)), `Liability at EoP df`)[, -(ncol(`Liability at EoP df`)+1)]
    
    # Update EoP with SoP
    `Liability at EoP df` <- `Liability at EoP df` + `Liability at SoP df`
    
    
    # Interest accrued on starting reserve
    `Maintenance Expenses` <- `Liability at SoP df`["Maintenance Expenses",] * (((1 + disc_assump["Spot Rate", ])^0.25) - 1)
    `Renewal Commission` <- `Liability at SoP df`["Renewal Commission",] * (((1 + disc_assump["Spot Rate", ])^0.25) - 1)
    `Attritional Payments` <- `Liability at EoP df`["Attritional Payments",] * (((1 + disc_assump["Spot Rate", ])^0.25) - 1)
    `Large Payments` <- `Liability at SoP df`["Large Payments",] * (((1 + disc_assump["Spot Rate", ])^0.25) - 1)
    `Cat Payments` <- `Liability at SoP df`["Cat Payments",] * (((1 + disc_assump["Spot Rate", ])^0.25) - 1)
    `ENIDs Payments` <- `Liability at SoP df`["ENIDs Payments",] * (((1 + disc_assump["Spot Rate", ])^0.25) - 1)
    `Interest accrued on starting reserve df` <- rbind(`Maintenance Expenses`, `Renewal Commission`, `Attritional Payments`,
                                                       `Large Payments`, `Cat Payments`, `ENIDs Payments`)
    
    `Interest accrued on starting reserve` <- colSums(`Interest accrued on starting reserve df`, na.rm = TRUE)
    `Interest accrued on starting reserve df` <- rbind(`Interest accrued on starting reserve df`, `Interest accrued on starting reserve`)
    colnames(`Interest accrued on starting reserve df`) <- NULL
    
})


# Liability for remaining coverage ----------------------------------------

local(envir = lfrc_cf, {
    Release <- -qt_cf$`Earned Premium (gross of commission)`
    `New Premium` <- c(initial_premium_received, rep(0, (length(Release)-1)))
    `Liability at EoP` <- floor(cumsum(`New Premium` + Release))
    `Interest Accrued` <- rep(0, length(Release))
    `Liability at SoP` <- c(0, `Liability at EoP`)[-(length(Release)+1)]
    `Gross of acquisition expenses` <- rbind(`Liability at SoP`, `New Premium`, Release, `Interest Accrued`, `Liability at EoP`)
    
    
    Release <- -qt_cf$`Earned Premium (gross of commission)`
    `New Premium` <- c(382500, rep(0, (length(Release)-1)))
    `Liability at EoP` <- floor(cumsum(`New Premium` + Release))
    `Interest Accrued` <- rep(0, length(Release))
    `Liability at SoP` <- c(0, `Liability at EoP`)[-(length(Release)+1)]
    `Net of acquisition expenses` <- rbind(`Liability at SoP`, `New Premium`, Release, `Interest Accrued`, `Liability at EoP`)
    
})



# Amortisation of acquisition expenses ------------------------------------

local(envir = lfrc_cf, {
    `LRC release gross of acquisition expenses` <- lfrc_cf$`Gross of acquisition expenses`["Release", ]
    `LRC release net of acquisition expenses` <- lfrc_cf$`Net of acquisition expenses`["Release", ]
    `Amortisation of acquisition expenses` <- `LRC release gross of acquisition expenses` - `LRC release net of acquisition expenses`
    
    `Amortisation of acquisition expenses` <- rbind(`LRC release gross of acquisition expenses`, 
                                                    `LRC release net of acquisition expenses`,
                                                    `Amortisation of acquisition expenses`)
})








