# shared strains settings
min_overlap = 5000
min_similarity = 0.999

# studies
fmt.studies = c('ALM', 'FRICKE')
rcdi.studies = c('ALM', 'FRICKE')
control.studies <- c("ASCINARF", "HMP", "LOUISS","RAYMONDF")

# cases
cases_failed <- paste0('ALM_Case_', c(6, 9, 13, 15, 18, 20))

# Cases with Full R/P/D sets or more than 1 Control Sample
rcdi.full <- c("ALM_Case_12", "ALM_Case_13", "ALM_Case_15", "ALM_Case_17", "ALM_Case_18", "ALM_Case_20", "ALM_Case_21", "ALM_Case_22", "ALM_Case_23", "ALM_Case_24", "ALM_Case_25", "ALM_Case_26", "ALM_Case_3", "ALM_Case_4", "ALM_Case_6", "ALM_Case_8", "ALM_Case_9", "FRICKE_Case_16", "FRICKE_Case_19", "FRICKE_Case_20", "FRICKE_Case_23", "FRICKE_Case_28", "FRICKE_Case_29", "FRICKE_Case_54", "FRICKE_Case_9")

control.full <- c("ASCINARF_Case_4", "ASCINARF_Case_5", "HMP_Case_1", "HMP_Case_10", "HMP_Case_11", "HMP_Case_12", "HMP_Case_13", "HMP_Case_14", "HMP_Case_15", "HMP_Case_16", "HMP_Case_17", "HMP_Case_18", "HMP_Case_19", "HMP_Case_2", "HMP_Case_20", "HMP_Case_21", "HMP_Case_22", "HMP_Case_23", "HMP_Case_24", "HMP_Case_25", "HMP_Case_26", "HMP_Case_27", "HMP_Case_28", "HMP_Case_29", "HMP_Case_3", "HMP_Case_30", "HMP_Case_31", "HMP_Case_32", "HMP_Case_33", "HMP_Case_34", "HMP_Case_35", "HMP_Case_36", "HMP_Case_37", "HMP_Case_38", "HMP_Case_39", "HMP_Case_4", "HMP_Case_40", "HMP_Case_41", "HMP_Case_42", "HMP_Case_43", "HMP_Case_5", "HMP_Case_6", "HMP_Case_7", "HMP_Case_8", "HMP_Case_9", "LOUISS_Case_43", "LOUISS_Case_44", "LOUISS_Case_45", "LOUISS_Case_50", "LOUISS_Case_51", "LOUISS_Case_53", "LOUISS_Case_56", "LOUISS_Case_58", "LOUISS_Case_60", "LOUISS_Case_61", "LOUISS_Case_62", "LOUISS_Case_63", "LOUISS_Case_64", "LOUISS_Case_65", "LOUISS_Case_66", "LOUISS_Case_68", "RAYMONDF_Case_23", "RAYMONDF_Case_25", "RAYMONDF_Case_38", "RAYMONDF_Case_6", "RAYMONDF_Case_7", "RAYMONDF_Case_8")
cases.full <- c(control.full, rcdi.full)

# mapping alm cases
convert_to_alm_cases <- function(x) {
  tibble(Case_Name = x) %>% 
    mutate(Case_Number = str_split_fixed(Case_Name, pattern = 'ALM_Case_', 2)[,2] %>% as.numeric()) %>% 
    mutate(ALM_Case = case_when(
      Case_Number == 4 ~ '5R',
      Case_Number == 6 ~ '7R',
      Case_Number == 9 ~ '9R',
      Case_Number == 13 ~ '11R',
      Case_Number == 15 ~ '12R',
      Case_Number == 18 ~ '14R',
      Case_Number == 20 ~ '16R',
      
      Case_Number == 3 ~ '2R',
      Case_Number == 8 ~ '8R',
      Case_Number == 12 ~ '10R',
      Case_Number == 17 ~ '13R',
      Case_Number == 21 ~ '17R',
      Case_Number == 22 ~ '18R',
      Case_Number == 23 ~ '19R',
      Case_Number == 24 ~ '4E',
      Case_Number == 25 ~ '5E',
      Case_Number == 26 ~ '6E', 
      T ~ Case_Name
    )) %>% 
    pull(ALM_Case) %>% 
    return()
}