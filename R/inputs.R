library(readxl)
library(dplyr)
library(purrr)
library(stringr)
library(tidyr)

pth_contract_details <- "data/files/CONTRACT_DETAILS_PAS.xlsx"
pth_grouping <- "data/files/GROUPING.xlsx"
pth_input_assumptions <- "data/files/INPUT_ASSUMPTIONS.xlsx"

contract_details <- readxl::read_xlsx(pth_contract_details)

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
timings <- cashflow_timings[["Timings"]]
names(timings) <- cashflow_timings[["Cashflows item"]]

loss_ratios <- read_xlsx(pth_input_assumptions, range = "B27:D30", 
                         col_types = c("text", "numeric", "numeric"))

earning_pattern <- read_xlsx(pth_input_assumptions, range = cell_rows(33:36)) %>%
    gather("term", "earning", -`Cohort Name`)

payment_pattern <- read_xlsx(pth_input_assumptions, range = cell_rows(39:42)) %>% 
    gather("term", "payment", -`Cohort Name`)

onerosity_criteria <- read_xlsx(pth_input_assumptions, range = "B45:E47")


# discounting assumptions -------------------------------------------------

spot_rate_20181231 <- read_xlsx(pth_input_assumptions, range = "E4:T4",
                                col_names = FALSE, col_types = "numeric") %>% 
    as.matrix() %>% drop() %>% set_names(term)

spot_rate_20190331 <- read_xlsx(pth_input_assumptions, range = "E6:T6",
                                col_names = FALSE, col_types = "numeric") %>% 
    as.matrix() %>% drop() %>% set_names(term)

n <- length(term)
disc_assump <- local({
    `Spot Rate` <- spot_rate_20181231
    `Forward Rate (start-qarter)` <- double(n)
    `Forward Rate (start-qarter)`[1] <- `Spot Rate`[1]
    for (i in 2:n) {
        `Forward Rate (start-qarter)`[i] <- ((1 + `Spot Rate`[i]) ^ i /
                                                 (1 + `Spot Rate`[i - 1]) ^ (i - 1)) -1
    }
    
    proj_qt_factor <- c(Q1 = 0, Q2 = .25, Q3 = .5, Q4 = .75)
    proj_qt <- "Q1"
    `Discount Factor (EoP)` <- (1 + `Spot Rate`) ^ (-term + proj_qt_factor[proj_qt])
    `Discount Factor (SoP)` <- lag(`Discount Factor (EoP)`, default = 1)
    `Discount Factor (MoP)` <- (1 + `Spot Rate`) ^ (-term + proj_qt_factor[proj_qt] + .25/4)
    rbind(`Spot Rate`, `Forward Rate (start-qarter)`, `Discount Factor (EoP)`,
          `Discount Factor (SoP)`, `Discount Factor (MoP)`)
})

disc_factor <- c("SoP" = "Discount Factor (SoP)",
                 "EoP" = "Discount Factor (EoP)",
                 "MoP" = "Discount Factor (MoP)")



