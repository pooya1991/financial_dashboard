is_exp <- readxl::read_xlsx("data/files/PAA_Unearned_model_dev.xlsx", sheet = "Dashboard",
                            range = "E23:E37", col_types = "numeric", col_names = FALSE) %>%
    as.matrix() %>% drop()

is_act_ass <- readxl::read_xlsx("data/files/PAA_Unearned_model_dev.xlsx", sheet = "Dashboard",
                                range = "F23:F37", col_types = "numeric", col_names = FALSE) %>%
    as.matrix() %>% drop()

is_qt_end <- readxl::read_xlsx("data/files/PAA_Unearned_model_dev.xlsx", sheet = "Dashboard",
                               range = "G23:G37", col_types = "numeric", col_names = FALSE) %>%
    as.matrix() %>% drop()