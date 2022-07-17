load("1_import_and_clean/output.RData")

dataset <- data_with_iv_coadmin

##analysis
#chi  sq
library(infer)
dataset %>% 
  chisq_test(rosc_outcome ~ site_upper_lower)
