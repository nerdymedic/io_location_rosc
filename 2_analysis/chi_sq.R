load("1_import_and_clean/output.RData")

dataset <- data_no_iv_coadmin

#chi  sq
dataset %>% 
  chisq_test(rosc_outcome ~ site_upper_lower)

dataset <- data_with_iv_coadmin

dataset %>% 
  chisq_test(rosc_outcome ~ site_upper_lower)
