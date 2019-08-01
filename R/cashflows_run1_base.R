
# unearned_cashflows ------------------------------------------------------

earning_doubtful <- filter(earning_pattern, 
                           `Cohort Name` == "2019_MEDICAL_INDIVIDUAL_DOUBTFUL") %>% 
    arrange(term) %>% pull(earning)

n <- length(term)
`Premiums Received` <- double(n)
`Premiums Received`[1] <- filter(contract_summed, group_type %in% c("doubtful", "non_onerous")) %>% 
    pull(total_premium) %>% sum()

`Premiums Paid` <- double(n)
`Earned Premium (gross of commission)` <- earning_doubtful * `Premiums Received`[1]

`Earned Premium (net of commission)` <- rep(`Premiums Received`[1], n) -
    cumsum(lag(`Earned Premium (gross of commission)`, default = 0))

`Initial Commission` <- double(n)
`Initial Commission`[1] <- filter(contract_summed, group_type %in% c("doubtful", "non_onerous")) %>% 
    pull(total_commission) %>% sum()

`Unearned Premium Reserve (net of commission)` <- rep(`Premiums Received`[1] - `Initial Commission`[1], n) 
`Earned Premium (net of commission)` <- earning_doubtful * `Unearned Premium Reserve (net of commission)`[1]
`Unearned Premium Reserve (net of commission)` <- `Unearned Premium Reserve (net of commission)` - 
    cumsum(lag(`Earned Premium (net of commission)`, default = 0))

`Unearned Premium Reserve (gross of commission)` <- rep(`Premiums Received`[1], n) -
    cumsum(lag(`Earned Premium (gross of commission)`, default = 0))

`Deferred Acquisition costs` <- `Unearned Premium Reserve (gross of commission)` - 
    `Unearned Premium Reserve (net of commission)`

cashflows <- rbind(`Premiums Received`,
      `Earned Premium (gross of commission)`, 
      `Earned Premium (net of commission)`,
      `Unearned Premium Reserve (gross of commission)`,
      `Unearned Premium Reserve (net of commission)`,
      `Deferred Acquisition costs`,
      `Initial Commission`)

C29 <- filter(loss_ratios, `Cohort Name` == "2019_MEDICAL_INDIVIDUAL_NON_ONEROUS") %>% 
    pull(BASE_RUN_LOSS)

total_cashflows <- apply(cashflows, 1, sum, na.rm = TRUE)
total_cashflows["Premiums Received"] * 0.5 * .1

attr_payments_qs <- 1:11