#library(tidyverse)
library(disk.frame)
library(forcats)

path_to_ESO <- "/Users/mat/Downloads/ESO dataset 2020/CSV"

path_df <- "./1_import_and_clean"

setup_disk.frame()

#cases that received IO meds
fc_meds.df <-  csv_to_disk.frame(
  infile = file.path(path_to_ESO, "2020_ESO_FC_Meds_2.csv"),
  outdir =  file.path(path_df, "/diskframes/fc_meds"),
  overwrite = TRUE
)

meds_io_acls.df <- fc_meds.df %>% 
  filter(route == "intraosseous" | route == "intraosseous (io)") %>% 
  mutate(
    acls_med = case_when(
      str_detect(treatment_name, "atropine") ~ TRUE,
      str_detect(treatment_name, "amiodarone") ~ TRUE,
      str_detect(treatment_name, "calcium") ~ TRUE,
      str_detect(treatment_name, "epi") ~ TRUE,
      str_detect(treatment_name, "lidocaine") ~ TRUE,
      str_detect(treatment_name, "sodium bicarb") ~ TRUE
    )
  ) %>% 
  filter(acls_med == TRUE) %>% 
  chunk_distinct(pcrid, .keep_all = TRUE)

meds_iv_acls.df <- fc_meds.df %>% 
  filter(route == "intravenous" | route == "intravenous (iv)") %>% 
  mutate(
    acls_med = case_when(
      str_detect(treatment_name, "atropine") ~ TRUE,
      str_detect(treatment_name, "amiodarone") ~ TRUE,
      str_detect(treatment_name, "calcium") ~ TRUE,
      str_detect(treatment_name, "epi") ~ TRUE,
      str_detect(treatment_name, "lidocaine") ~ TRUE,
      str_detect(treatment_name, "sodium bicarb") ~ TRUE
    )
  ) %>% 
  filter(acls_med == TRUE) %>% 
  chunk_distinct(pcrid, .keep_all = TRUE)
  

#find the codes
cpr.df <-  csv_to_disk.frame(
  infile = file.path(path_to_ESO, "2020_ESO_CPR.csv"),
  outdir =  file.path(path_df, "/diskframes/cpr"),
  overwrite = TRUE
) %>% 
  filter(cardiacarrest != "no") %>% 
  filter(resusattempted == "yes") %>% 
  #create binary outcome
  mutate(
    rosc_outcome = case_when(
      str_detect(endofevent, "expired") ~ FALSE,
      str_detect(endofevent, "pronounced") ~ FALSE,
      str_detect(endofevent, "rosc") ~ TRUE
    )
  ) %>% 
  #ignores "ongoing resuscitation" cases and DNRs
  filter(!is.na(rosc_outcome))

#need this to get the IO location
fc_iv_io_only.df <-  csv_to_disk.frame(
  infile = file.path(path_to_ESO, "2020_ESO_FC_IV_2.csv"),
  outdir =  file.path(path_df, "/diskframes/fc_iv"),
  overwrite = TRUE
  ) %>% 
  filter(treatment_name  == "intraosseous" & successful == "yes") %>% 
  mutate(
    site_upper_lower = case_when(
      str_detect(site, "femoral") ~ "Lower",
      str_detect(site, "humeral") ~ "Upper",
      str_detect(site, "tibia") ~ "Lower",
      TRUE ~ "other"
    )
  ) %>% 
  filter(site_upper_lower != "other") %>% 
  collect() %>% 
  group_by(pcrid) %>% 
  #find cases that had only upper or lower extremity IO
  mutate(
    site_only_upper_or_lower = n_distinct(site_upper_lower),
    site_only_upper_or_lower = case_when(
      site_only_upper_or_lower == 1 ~ TRUE,
      site_only_upper_or_lower != 1 ~ FALSE
    )
  ) %>% 
  ungroup()

#remove records that are documented as given by IO but do not have an IO documented
meds_io_acls_with_documented_io.df <- meds_io_acls.df %>% 
  left_join(fc_iv_io_only.df, by = "pcrid") %>% 
  filter(!is.na(site)) %>% 
  chunk_distinct(pcrid, .keep_all = TRUE) %>% 
  select(pcrid, site, size, site_upper_lower, site_only_upper_or_lower)

#filter out anything besides protocol or med con -- these probably are TOR
#remainder includes DNR, obvious death -- to anti join later
cpr_codes_dnr_etc.df <- cpr.df %>% 
  filter(rosc_outcome == FALSE) %>% 
  filter(
    discontinuereason != "protocol/policy requirements completed",
    discontinuereason != "medical control order"
  ) %>% 
  chunk_distinct(pcrid)

cpr_codes_outcome_minusDNRetc_meds_acls_w_doc_io <- cpr.df %>% 
  collect() %>% 
  dplyr::anti_join(cpr_codes_dnr_etc.df %>% collect(), by = "pcrid") %>%  #remove DNRs, obvious death
  dplyr::left_join(meds_io_acls_with_documented_io.df %>% collect(), by = "pcrid") %>% #only with ACLS meds via documented IO
  filter(site_only_upper_or_lower == TRUE) %>% #only an upper or lower IO
  distinct(pcrid, .keep_all = TRUE) #remove multiple meds -- ie only one per cases

#cant forget about the airway data
airways <- c("combitube", "igel", "king airway", "laryngeal mask airway", 
             "nasotracheal intubation", "needle cricothyroidotomy", "orotracheal intubation",
             "rapid sequence intubation (rsi)", "surgical cricothyroidotomy",
             "sedation assist intubation(sai)", "npa", "opa")

airways_basic <- c("npa", "opa")
airways_sga <- c("combitube", "igel", "king airway", "laryngeal mask airway")
airways_ett <- c("nasotracheal intubation", "orotracheal intubation", 
                 "rapid sequence intubation (rsi)", "sedation assist intubation(sai)")
airways_surgical <- c("needle cricothyroidotomy", "surgical cricothyroidotomy")

fc_airway <-  csv_to_disk.frame(
  infile = file.path(path_to_ESO, "2020_ESO_FC_Airway_2.csv"),
  outdir =  file.path(path_df, "/diskframes/airway"),
  overwrite = TRUE
  ) %>% 
  filter(treatment_name %in% airways) %>% 
  rename(airway = treatment_name) %>% 
  #select(pcrid, airway = treatment_name) %>% 
  mutate(airway_class = 
           case_when(
             airway %in% airways_basic ~ "OPA/NPA",
             airway %in% airways_sga ~ "Supraglottic Airway",
             airway %in% airways_ett ~ "Intubation",
             airway %in% airways_surgical ~ "Surgical"
           )
  ) %>% 
  collect() %>% 
  #keep only the last airway
  group_by(pcrid) %>% 
  arrange(desc(treatment_date)) %>% 
  slice(1) %>% 
  ungroup()
  

#dont forget the 911 call / agency data
incident.df <-  csv_to_disk.frame(
  infile = file.path(path_to_ESO, "2020_ESO_Incident_2.csv"),
  outdir =  file.path(path_df, "/diskframes/incident"),
  overwrite = TRUE
  ) %>% 
  select(pcrid, agencyid, runtype) %>% 
  filter(runtype == "911 response")

#pull in the demographics
patient.df <-  csv_to_disk.frame(
  infile = file.path(path_to_ESO, "2020_ESO_Patient.csv"),
  outdir =  file.path(path_df, "/diskframes/patient"),
  overwrite = TRUE
) %>% 
  select(pcrid, age_yrs, gender, weight_kg)

patient_race.df <- csv_to_disk.frame(
  infile = file.path(path_to_ESO, "2020_ESO_Patient_Race.csv"),
  outdir =  file.path(path_df, "/diskframes/patient_race"),
  overwrite = TRUE
)

#join demographics
data_with_demo <- cpr_codes_outcome_minusDNRetc_meds_acls_w_doc_io %>% 
  dplyr::left_join(patient.df %>% collect(), by = "pcrid") %>% 
  dplyr::left_join(patient_race.df %>% collect(), by =  "pcrid") %>% 
  dplyr::left_join(fc_airway, by = "pcrid") %>% 
  dplyr::left_join(incident.df %>% collect(), by = "pcrid")

#clean it up
data_with_demo_clean <- data_with_demo %>% 
  mutate(
    arrestwitnessedby = fct_collapse(arrestwitnessedby,
                                     "Witnessed by Lay Person" = c("bystander",
                                                                   "witnessed by family member"),
                                     "Unknown" = c("")
                                     ),
    cprinitiatedby = fct_collapse(cprinitiatedby,
                                  "Bystander" = c("bystander - compressions & ventilations",
                                                  "bystander - compressions only",
                                                  "bystander - ventilations only",
                                                  "family",
                                                  "family member",
                                                  "healthcare professional (non-ems)",
                                                  "lay person (non-family)",
                                                  "medical/health care provider",
                                                  "other ems professional (not part of the dispatched response)"),
                                  "First Responder" = c("first responder",
                                                        "law enforcement"),
                                  "Unknown" = c("", "other")
                                  ),
    initialekg = fct_collapse(initialekg,
                              "Unknown"  = c("", "other", "bradycardia"),
                              "PEA/Asystole" = c("pea", "asystole"),
                              "VF/VT" = c("ventricular fibrillation", "ventricular tachycardia")
                              ),
    aed_used_pta =  fct_collapse(aed_used_pta,
                                 "Unknown" = c("")
                                 ),
    gender = fct_other(gender,
                       keep = c("male", "female"),
                       other_level = "Unknown"
                       ),
    gender = fct_explicit_na(gender,
                             na_level = "Unknown"
                             ),
    gender = fct_recode(gender, 
                        "Male" = "male", 
                        "Female" = "female"),
    ethnicity = fct_explicit_na(ethnicity,
                                na_level = "Unknown"
                                ),
    ethnicity = fct_other(ethnicity,
                          drop  = "",
                          other_level = "Unknown"
                          ),
    ethnicity = fct_recode(ethnicity,
                           "Hispanic or Latino" = "hispanic or latino",
                           "Not Hispanic or Latino" = "not hispanic or latino"),
    race_sum = rowSums(
      select(.,
        starts_with("race_")
        )
      ),
    race = case_when(
      race_sum != 1 ~  "Unknown",
      is.na(race_sum) ~ "Unknown",
      race_amind_ak == 1 ~ "American Indian Alaskan Native",
      race_asian ==1 ~ "Asian",
      race_black == 1 ~ "Black or African American",
      race_hispanic == 1 ~ "Hispanic or Latino",
      race_hipacisland == 1 ~ "Hawaii Native or Other Pacific Islander",
      race_other == 1 ~ "Unknown",
      race_white == 1 ~ "White"
    ),
    arrestwitnessedby = fct_recode(arrestwitnessedby,
                                   "Not Witnessed" = "not witnessed",
                                   "Witnessed by Healthcare Provider" = "witnessed by healthcare provider",
                                   "Witnessed by Lay Person" = "witnessed by lay person"
                                   ),
    cprinitiatedby = fct_recode(cprinitiatedby,
                                "EMS" = "ems"
                                ),
    aed_used_pta = fct_recode(aed_used_pta,
                              "No" = "no",
                              "Yes" = "yes"
                              ),
    initialekg = fct_recode(initialekg,
                            "Unknown AED Non-Shockable Rhythm" = "unknown aed non-shockable rhythm",
                            "Unknown AED Shockable Rhythm" = "unknown aed shockable rhythm"
                            ),
    site_upper_lower = fct_recode(site_upper_lower,
                                  "Lower Extremity" = "Lower",
                                  "Upper Extremity" = "Upper"
                                  )
  ) %>% 
  mutate(
    arrestwitnessed = case_when(
      arrestwitnessedby == "Unknown" ~ "Unknown",
      arrestwitnessedby == "Not Witnessed" ~ "Not Witnessed",
      arrestwitnessedby == "Witnessed by Lay Person" ~ "Witnessed",
      arrestwitnessedby == "Witnessed by Healthcare Provider" ~ "Witnessed"
    ),
    rosc_outcome = case_when(
      rosc_outcome == TRUE ~ "ROSC",
      rosc_outcome == FALSE ~ "No ROSC"
    ),
    cpr_pta = case_when(
      cprinitiatedby == "Bystander" ~ TRUE,
      cprinitiatedby == "Unknown" ~ NA,
      cprinitiatedby == "EMS" ~ FALSE,
      cprinitiatedby == "First Responder" ~ FALSE
    ),
    weight_kg = case_when(
      weight_kg == -1 ~ NA_real_,
      weight_kg > 20 ~ weight_kg,
      weight_kg < 500 ~ weight_kg,
      TRUE ~ NA_real_
    ),
    initial_shockable = case_when(
      initialekg == "PEA/Asystole" ~ "No Shock",
      initialekg == "Unknown AED Non-Shockable Rhythm" ~ "No Shock",
      initialekg == "Unknown AED Shockable Rhythm" ~ "Shock",
      initialekg == "VF/VT" ~ "Shock"
    )
  ) %>% 
  select(-starts_with("race_")) %>% 
  mutate(
    #age cutoff chosen from manually looking at age distribution, suspect 119 and 120 are entry errors
    age_yrs = case_when(
      age_yrs > 110 ~ NA_integer_,
      TRUE ~ age_yrs
    )
  ) %>% 
  tidyr::replace_na(
    list(airway_class = "Unknown")
  ) %>% 
  filter(age_yrs >= 18) %>% 
  filter(runtype == "911 response") %>% 
  filter(est_time_collapsetocpr < 60) %>% 
  distinct(pcrid, .keep_all = TRUE) %>% 
  mutate_at(vars(-pcrid, -starts_with("est_time"), est_time_arrest, -firstcprtime, 
                 -rosctime, -endofevent, -resusdiscontinuetime,
                 -age_yrs, -weight_kg, -agencyid, endofevent
                 ), as.factor) %>% 
  select(
    -resusattempted, - starts_with("attempted_"), -chestcompressions, -starts_with("notattempted_"),
    -rosc, -roscoccurred,  -rhythmatdestintation, -runtype, -site_only_upper_or_lower,
    -starts_with("field"), -cardiacarrest, weight_kg, -starts_with("race_")
  ) %>% 
#turns out we wanted NAs instead of Unknown
  mutate(across(
    where(is.factor),
    ~ na_if(.x, "Unknown"),
    ~ na_if(.x, "unknown")
  )) %>%
  mutate(across(
    where(is.factor),
    fct_drop
  ))

#make datasets with and without IV co-admin
data_with_iv_coadmin <- data_with_demo_clean

data_no_iv_coadmin <- data_with_demo_clean %>% 
  dplyr::anti_join(meds_iv_acls.df %>% collect(), by = "pcrid")


#save everything
save.image(file = "1_import_and_clean/output_everything.RData")
  
#cleanup
remove(cpr_codes_dnr_etc.df, cpr_codes_outcome_minusDNRetc_meds_acls_w_doc_io,
       cpr.df, data_with_demo, data_with_demo_clean, fc_meds.df,
       meds_iv_acls.df, meds_io_acls_with_documented_io.df, fc_iv_io_only.df,
       meds_io_acls.df,  patient_race.df, patient.df, path_df, path_to_ESO, fc_airway,
       airways, airways_basic, airways_ett, airways_sga, airways_surgical, incident.df)

#save
save.image(file = "1_import_and_clean/output.RData")
