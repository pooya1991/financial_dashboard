source("R/inputs.R")
source("R/utils.R")

# setting things up -------------------------------------------------------

cf_run1_base <- new.env()
qt_cf <- new.env(parent = cf_run1_base)
qt_cf_undiscounted <- new.env(parent = cf_run1_base)
qt_cf_discounted <- new.env(parent = cf_run1_base)
pv_of_cf <- new.env(parent = cf_run1_base)

earning_doubtful <- filter(earning_pattern,
                           `Cohort Name` == "2019_MEDICAL_INDIVIDUAL_DOUBTFUL") %>%
    arrange(term) %>% pull(earning)

initial_premium_received <- filter(contract_summed, group_type %in% c("doubtful", "non_onerous")) %>%
    pull(total_premium) %>% sum()

initial_commission <- filter(contract_summed, group_type %in% c("doubtful", "non_onerous")) %>%
    pull(total_commission) %>% sum()

C29 <- filter(loss_ratios, `Cohort Name` == "2019_MEDICAL_INDIVIDUAL_NON_ONEROUS") %>%
    pull(BASE_RUN_LOSS)

payment_doubtful <- filter(payment_pattern,
                           `Cohort Name` == "2019_MEDICAL_INDIVIDUAL_DOUBTFUL") %>%
    arrange(term) %>% pull(payment)



# quarterly cashflows -----------------------------------------------------

local(envir = qt_cf, {
    `Premiums Received` <- double(n)
    `Premiums Received`[1] <- initial_premium_received
    
    `Premiums Paid` <- double(n)
    `Earned Premium (gross of commission)` <- earning_doubtful * initial_premium_received
    
    `Earned Premium (net of commission)` <- rep(initial_premium_received, n) -
        cumsum(lag(`Earned Premium (gross of commission)`, default = 0))
    
    `Initial Commission` <- double(n)
    `Initial Commission`[1] <-initial_commission
    
    `Unearned Premium Reserve (net of commission)` <- rep(initial_premium_received - initial_commission, n)
    `Earned Premium (net of commission)` <- earning_doubtful * `Unearned Premium Reserve (net of commission)`[1]
    `Unearned Premium Reserve (net of commission)` <- `Unearned Premium Reserve (net of commission)` -
        cumsum(lag(`Earned Premium (net of commission)`, default = 0))
    
    `Unearned Premium Reserve (gross of commission)` <- rep(initial_premium_received, n) -
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
    
    
    total_cashflows <- apply(cashflows, 1, sum, na.rm = TRUE)
    total_cashflows["Premiums Received"] * 0.5 * .1
    
    attr_payments_qs <- 1:12
    attr_payments_totals <- (earning_doubtful[attr_payments_qs] *
                                 total_cashflows["Premiums Received"] * C29) %>%
        set_names(paste0("Attritional Payments_Q", attr_payments_qs))
    
    attr_payments_mat <- matrix(NA_real_, length(attr_payments_qs), n)
    rownames(attr_payments_mat) <- names(attr_payments_totals)
    for (i in attr_payments_qs) {
        x <- attr_payments_totals[i] * payment_doubtful
        x <- x[!is.na(x)]
        if (length(x) == 0) next()
        attr_payments_mat[i, i:(i + length(x) - 1)] <- x
    }
})

# quarterly cashflows undiscounted ----------------------------------------

local(envir = qt_cf_undiscounted, {
    `Premiums Paid` <- qt_cf$`Earned Premium (gross of commission)` * signs["Premiums Paid"]
    `Initial Commission` <- initial_commission * signs["Initial Commission"]
    attr_payments_qs <- 1:12
    attr_payments_mat <- signs["Attritional Payments"] * qt_cf$attr_payments_mat
    rownames(attr_payments_mat) <- paste0("Attritional Payments_Q", attr_payments_qs)
    attr_payments_totals <- rowSums(attr_payments_mat, na.rm = TRUE)
})

# quarterly cashflows discounted ------------------------------------------

local(envir = qt_cf_discounted, {
    #TODO check this with pooya. it does not make sense
    `Premiums Paid` <- qt_cf_undiscounted$`Premiums Paid` *
        disc_assump["Discount Factor (EoP)", ]
    `Initial Commission` <- initial_commission * disc_assump["Discount Factor (EoP)", ][1]
    dtfr <- disc_assump[disc_factor[timings["Attritional Payments"]], ]
    attr_payments_mat <- qt_cf_undiscounted$attr_payments_mat
    for (i in 1:nrow(attr_payments_mat)) {
        x <- attr_payments_mat[i, ]
        idx <- !is.na(x)
        x <- x[!is.na(x)]
        if (length(x) == 0) next()
        x <- x * dtfr[1:length(x)]
        attr_payments_mat[i, idx] <- x
    }
    
    attr_payments_totals <- rowSums(attr_payments_mat, na.rm = TRUE)
})

# PV of cashflows ---------------------------------------------------------

local(envir = pv_of_cf, {
    `Premiums Paid` <- rcumsum_weird(qt_cf_discounted$`Premiums Paid`)
    `Initial Commission` <- initial_commission
    attr_payments_mat <- -apply(qt_cf_discounted$attr_payments_mat,
                                1, rcumsum_weird) %>% t()
})
