load("1_import_and_clean/output.RData")

library(tidymodels)

dataset <- 
  data_with_iv_coadmin %>%
  select(rosc_outcome, site_upper_lower, arrestwitnessed, est_time_collapsetocpr,
         aed_used_pta, cpr_pta, initial_shockable, age_yrs, gender, weight_kg)

#regression
dataset$rosc_outcome <- relevel(dataset$rosc_outcome, ref = "ROSC") 

set.seed(42)
splits <- initial_split(dataset, strata = site_upper_lower, prop = 0.8)

data_train <- training(splits)
data_test <- testing(splits)

#the most basic regression
fitted_lr_basic <- 
  logistic_reg() %>% 
  set_engine("glm") %>% 
  set_mode("classification") %>% 
  fit(rosc_outcome ~ site_upper_lower + arrestwitnessed + est_time_collapsetocpr +
        aed_used_pta + cpr_pta + initial_shockable + age_yrs + gender + weight_kg,
      data = data_train)

#view the model
tidy(fitted_lr_basic, exponentiate = TRUE)

#predict on the test set
dataset_results <- augment(fitted_lr_basic, data_test)

#confusion matrix and test chars
conf_mat(dataset_results, truth = rosc_outcome, estimate = .pred_class) %>% 
  summary() %>% 
  bind_rows(
    roc_auc(dataset_results, truth = rosc_outcome, estimate = .pred_ROSC)
  )

#plot the ROC curve
dataset_results %>% 
  roc_curve(truth = rosc_outcome, estimate = .pred_ROSC) %>% 
  autoplot()
