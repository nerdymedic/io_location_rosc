load("1_import_and_clean/output.RData")

library(gtsummary)

dataset <- data_with_iv_coadmin

table1 <- dataset %>% 
  select(site_upper_lower, age_yrs, gender, ethnicity, race, arrestwitnessedby, 
         cprinitiatedby, aed_used_pta, airway_class, initialekg, rosc_outcome) %>% 
  tbl_summary(by = site_upper_lower,
              label = list(
                age_yrs ~ "Age",
                gender ~ "Gender",
                ethnicity ~ "Ethnicity",
                race ~ "Race",
                arrestwitnessedby ~ "Witnessed By", 
                cprinitiatedby ~ "CPR Started By", 
                aed_used_pta ~ "AED Prior to Arrival",
                initialekg ~ "Initial Rhythm", 
                rosc_outcome ~ "ROSC",
                airway_class ~ "Airway"
              )
  )
#add_p()

table1 %>% 
  as_flex_table() %>% 
  flextable::save_as_docx(path = "2_analysis/table1.docx")
