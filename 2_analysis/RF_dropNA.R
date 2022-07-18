library(tidymodels)

load("1_import_and_clean/output.RData")

dataset <- data_no_iv_coadmin %>% 
  select(rosc_outcome, site_upper_lower, arrestwitnessed, est_time_collapsetocpr,
         aed_used_pta, cpr_pta, initial_shockable, age_yrs, gender, weight_kg) %>% 
  drop_na()


#regression prep
dataset$rosc_outcome <- relevel(dataset$rosc_outcome, ref = "ROSC") 

set.seed(42)
splits <- initial_split(dataset, strata = site_upper_lower, prop = 0.8)

data_train <- training(splits)
data_test <- testing(splits)

folds <- vfold_cv(data_train, strata = site_upper_lower, v = 10)


#random forests
rf_spec <-
  rand_forest() %>% 
  set_engine("ranger") %>% 
  set_mode("classification")

rec_basic <-
  recipe(rosc_outcome ~ site_upper_lower + arrestwitnessed + est_time_collapsetocpr +
           aed_used_pta + cpr_pta + initial_shockable + age_yrs + gender + weight_kg,
         data = data_train
  ) %>% 
  step_naomit()

wf_rf <- 
  workflow() %>% 
  add_model(rf_spec) %>% 
  add_recipe(rec_basic)

ctrl_preds <- control_resamples(save_pred = TRUE)

fit_rf <- wf_rf %>% 
  fit_resamples(folds, control = ctrl_preds)

collect_metrics(fit_rf)

fit_rf %>% 
  augment() %>%
  roc_curve(truth = rosc_outcome, .pred_ROSC) %>% 
  autoplot()

collect_predictions(fit_rf) %>% 
  conf_mat(truth = rosc_outcome, .pred_class) %>% 
  summary()
