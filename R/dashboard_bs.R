bs_exp <- readxl::read_xlsx("data/files/PAA_Unearned_model_dev.xlsx", sheet = "Dashboard",
                            range = "E6:E20", col_types = "numeric", col_names = FALSE) %>%
    as.matrix() %>% drop()

bs_act_ass <- readxl::read_xlsx("data/files/PAA_Unearned_model_dev.xlsx", sheet = "Dashboard",
                                range = "F6:F20", col_types = "numeric", col_names = FALSE) %>%
    as.matrix() %>% drop()

bs_qt_end <- readxl::read_xlsx("data/files/PAA_Unearned_model_dev.xlsx", sheet = "Dashboard",
                               range = "G6:G20", col_types = "numeric", col_names = FALSE) %>%
    as.matrix() %>% drop()

