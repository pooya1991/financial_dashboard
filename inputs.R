pth_contract_details <- "data/files/CONTRACT_DETAILS_PAS.xlsx"
pth_grouping <- "data/files/GROUPING.xlsx"
pth_input_assumptions <- "data/files/INPUT_ASSUMPTIONS.xlsx"

contract_details <- readxl::read_xlsx(pth_contract_details)
grouping <- contract_details %>% 
    mutate(
        `UNDERWRITING YEAR` = lubridate::year(`POLICY EFFECTIVE DATE`),
        `ONEROUS` = ifelse(AGE >= 50, TRUE, FALSE),
        NON_ONEROUS = ifelse(AGE <= 40, TRUE, FALSE),
        DOUBTFUL = !(`ONEROUS` & `NON_ONEROUS`),
        group_type = case_when(
            `ONEROUS` ~ "onerous",
            `NON_ONEROUS` ~ "non_onerous",
            `DOUBTFUL` ~ "doubtful"
        )
    )

contract_summed <- group_by(grouping, group_type) %>% 
    summarise(
        total_premium = sum(PREMIUM),
        total_claims_incurred = sum(`CLAIMS INCURRED`),
        total_commission = sum(COMMISSION)
        )

year <- read_xlsx(pth_input_assumptions, range = "E2:T2",
                  col_names = FALSE, col_types = "numeric") %>% 
    as.matrix() %>% drop() %>% unname()

term <- read_xlsx(pth_input_assumptions, range = "E3:T3",
                  col_names = FALSE, col_types = "numeric") %>% 
    as.matrix() %>% drop() %>% unname()

cashflow_timings <- read_xlsx(pth_input_assumptions, range = "B11:C23", col_types = "text") %>% 
    mutate(sign = ifelse(Timings == "SoP", 1L, -1L))

signs <- cashflow_timings[["sign"]]
names(signs) <- cashflow_timings[["Cashflows item"]]

loss_ratios <- read_xlsx(pth_input_assumptions, range = "B27:D30", 
                         col_types = c("text", "numeric", "numeric"))

earning_pattern <- read_xlsx(pth_input_assumptions, range = cell_rows(33:36)) %>%
    gather("term", "earning", -`Cohort Name`)

# read_xlsx(pth_input_assumptions, range = cell_rows(33:36)) %>% 
#     as.matrix() %>% 
#     (function(x) {rownames(x) <- x[, 1]; x[, -1]})

payment_pattern <- read_xlsx(pth_input_assumptions, range = cell_rows(39:42)) %>% 
    gather("term", "payment", -`Cohort Name`)

onerosity_criteria <- read_xlsx(pth_input_assumptions, range = "B45:E47")
