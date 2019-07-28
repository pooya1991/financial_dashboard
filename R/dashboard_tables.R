balance_sheet <- c("Assets", "Invested", "FVTOCI", "FVTP&L", "Total Assets", 
                   "Liabilities", "Risk Adjustment", "Liability for incurred claims",
                   "Liability for remaining coverage", "Loss Component", "Total Liability",
                   "Net assets",
                   "Capital and Reserves", "Liabilities & Equity", "Profit (Loss) in year")

bs_base <- c(NA_real_, NA_real_, NA_real_, NA_real_, 378000, NA_real_, NA_real_, 17779, 344250, NA_real_,
          362029, 15971, NA_real_, 378000, 15971)

bs_premiums <- c(NA_real_, NA_real_, NA_real_, 420000, 420000, NA_real_, NA_real_, 19754, 382500,
              NA_real_, 402254, 17746, NA_real_, 420000, 17746)

income_statement <- c("Allocation of premium for coverage period", "Adjustment to revenue for loss component",
                      "Total Insurance contract revenue", "Claims incurred", "Expenses incurred",
                      "Amortisation of acquisition costs", "Release of risk margin",
                      "Change in estimate of future cashflows", "Loss Component",
                      "Insurance Service Expense", "Insurance Service Result",
                      "Investment Income", "Interest on insurance liability",
                      "Insurance Finance Expenses", "Profit or Loss")

is_base <- c(45000, 0, 45000, -22169, 0, -6750, 0, 0, NA_real_, 28919, 16081, 0, -110, -110, 15971)
is_premiums <- c(50000, 0, 50000, -24632, 0, -7500, 0, 0, NA_real_, 32132, 17868, 0, -122, -122, 17746)