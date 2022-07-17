library(tidymodels)

load("1_import_and_clean/output.RData")

dataset <- data_no_iv_coadmin %>% 
  select(rosc_outcome, site_upper_lower, arrestwitnessed, est_time_collapsetocpr,
         aed_used_pta, cpr_pta, initial_shockable, age_yrs, gender, weight_kg)


#regression
dataset$rosc_outcome <- relevel(dataset$rosc_outcome, ref = "ROSC") 

set.seed(42)
splits <- initial_split(dataset, strata = site_upper_lower, prop = 0.8)

data_train <- training(splits)
data_test <- testing(splits)

folds <- vfold_cv(data_train, strata = site_upper_lower, v = 10)


#LR via julia silge - 10 fold cross validation
glm_spec <- 
  logistic_reg() %>% 
  set_engine("glm") %>% 
  set_mode("classification")

rec_basic <-
  recipe(rosc_outcome ~ site_upper_lower + arrestwitnessed + est_time_collapsetocpr +
           aed_used_pta + cpr_pta + initial_shockable + age_yrs + gender + weight_kg,
         data = data_train
  )
  # step_dummy(all_nominal_predictors())

wf_basic <- 
  workflow() %>% 
  add_model(glm_spec) %>% 
  add_recipe(rec_basic)

ctrl_preds <- control_resamples(save_pred = TRUE)

fit_basic <- wf_basic %>% 
  fit_resamples(folds, control = ctrl_preds)

collect_metrics(fit_basic)

fit_basic %>% 
  augment() %>%
  roc_curve(truth = rosc_outcome, .pred_ROSC) %>% 
  autoplot()


## add some interactions
## interactions sex by io location. 2x2 table looking at arrest, witnessed, aed
rec_interact <-
  rec_basic %>%
  step_interact(~ gender + site_upper_lower + weight_kg) %>%
  step_interact(~ gender + weight_kg) %>% 
  step_interact(~ arrestwitnessed + aed_used_pta + cpr_pta)

wf_interact <- 
  workflow() %>% 
  add_model(glm_spec) %>% 
  add_recipe(rec_interact)

fit_interact <- 
  wf_interact %>% 
  fit_resamples(folds, control = ctrl_preds)

collect_metrics(fit_interact)


#do the final fit on the best model on the test data
final <-
   wf_interact %>% 
  last_fit(splits)

collect_metrics(final)

collect_predictions(final) %>% 
  conf_mat(truth = rosc_outcome, .pred_class) %>% 
  summary()
