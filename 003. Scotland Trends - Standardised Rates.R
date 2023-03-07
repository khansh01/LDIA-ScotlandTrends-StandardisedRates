################################################################################
# Name: Learning disability (LD) - Scotland trends - European age-sex...
#   standardised rates (EASRs)
#
# Original Author: Nikos Alexandrou
# Original Date: 03/12/2019
# Latest Update Author: Scot Tweddle
# Latest Update Date: Nov 2021
# Updates to script (if any):
#
#
# Written on: RStudio Pro Server
# Written for: R version 3.6.1
#
# Type: Data analysis
# Output: Scotland-level trends for the period 1997/98 - 2021/22 (discharges,...
#   stays and patients) - EASRs
# Approximate run time: 15 minutes
#
################################################################################


### 1. Load the necessary libraries ----
#library("plyr")
library("dplyr")
library("tidyr")
library("haven")

### 2. Update the publication date, filepaths and year range ----
pub_date <- "TEST20230224"
input <- paste0("//PHI_conf/MentalHealth1/Inpatient care/Publications/",
                "Learning-Disability-Publication/", pub_date, "/Data/")
output <- paste0("//PHI_conf/MentalHealth1/Inpatient care/Publications/",
                 "Learning-Disability-Publication/", pub_date, "/Output/")
pop_folder <- paste0("//PHI_conf/MentalHealth1/Inpatient care/Publications/",
                     "Learning-Disability-Publication/", pub_date, "/Data/",
                     "Lookups/")
min_year <- "1997"
max_year <- "2021"

### 3. Read in the basefile ----
data <- readRDS(paste0(input, "SMR01_SMR04_basefile.rds"))

### 4. Remove SMR01 data ----
data <- data %>% filter(SMR04 == 1)

### 5. Recode missing values ----
data <- data %>% mutate(specialty = replace_na(specialty, "NA"))

### 6. Create five-year age groups ----
data <- data %>%
  mutate(AgeGroup =
           case_when(age_on_discharge >= 0 & age_on_discharge <= 4 ~ 1,
                     age_on_discharge >= 5 & age_on_discharge <= 9 ~ 2,
                     age_on_discharge >= 10 & age_on_discharge <= 14 ~ 3,
                     age_on_discharge >= 15 & age_on_discharge <= 19 ~ 4,
                     age_on_discharge >= 20 & age_on_discharge <= 24 ~ 5,
                     age_on_discharge >= 25 & age_on_discharge <= 29 ~ 6,
                     age_on_discharge >= 30 & age_on_discharge <= 34 ~ 7,
                     age_on_discharge >= 35 & age_on_discharge <= 39 ~ 8,
                     age_on_discharge >= 40 & age_on_discharge <= 44 ~ 9,
                     age_on_discharge >= 45 & age_on_discharge <= 49 ~ 10,
                     age_on_discharge >= 50 & age_on_discharge <= 54 ~ 11,
                     age_on_discharge >= 55 & age_on_discharge <= 59 ~ 12,
                     age_on_discharge >= 60 & age_on_discharge <= 64 ~ 13,
                     age_on_discharge >= 65 & age_on_discharge <= 69 ~ 14,
                     age_on_discharge >= 70 & age_on_discharge <= 74 ~ 15,
                     age_on_discharge >= 75 & age_on_discharge <= 79 ~ 16,
                     age_on_discharge >= 80 & age_on_discharge <= 84 ~ 17,
                     age_on_discharge >= 85 & age_on_discharge <= 89 ~ 18,
                     age_on_discharge >= 90 ~ 19,
                     TRUE ~ 0)
  )

### 7. Discharges at the Scotland level by year, age and sex ----
discharges_scot <- data %>%
  filter(specialty == "G5") %>%
  filter(fyear_pt1_dis >= min_year & fyear_pt1_dis <= max_year) %>%
  group_by(fyear_pt1_dis, AgeGroup, sex) %>%
  summarise(Discharges = sum(count_var))

### 8. Stays at the Scotland level by year, age and sex ----
data_stays <- data %>%
  arrange(link_no, cis_marker, admission_date, discharge_date)

cis_scot <- data_stays %>%
  group_by(link_no, cis_marker) %>%
  summarise(fyear_pt1_dis = max(fyear_pt1_dis),
            specialty = last(specialty),
            AgeGroup = last(AgeGroup),
            sex = last(sex)) %>%
  filter(fyear_pt1_dis >= min_year & fyear_pt1_dis <= max_year) %>%
  filter(specialty == "G5") %>%
  mutate(Stays = 1)

cis_scot <- cis_scot %>%
  group_by(fyear_pt1_dis, AgeGroup, sex) %>%
  summarise(Stays = sum(Stays))

### 9. Patients at the Scotland level by year, age and sex ----
patients_scot <- data %>%
  arrange(link_no, cis_marker, admission_date, discharge_date) %>%
  filter(specialty == "G5") %>%
  group_by(link_no, fyear_pt1_dis) %>%
  summarise(AgeGroup = last(AgeGroup),
            sex = last(sex)) %>%
  mutate(count_var = 1)

patients_scot <- patients_scot %>%
  group_by(fyear_pt1_dis, AgeGroup, sex) %>%
  summarise(Patients = sum(count_var)) %>%
  filter(fyear_pt1_dis >= min_year & fyear_pt1_dis <= max_year)

### 10. Calculate Scottish population by year, age and sex ----
# Read in the health board population file and aggregate it to get the...
# total population of Scotland broken down by year, age group and sex. After..
# you aggregate, select the correct years and rename a few variables to...
# prepare for matching with the data files.
pop_match <- readRDS(
  paste0(pop_folder, "HB2019_pop_est_5year_agegroups_1981_2021.rds")
)

pop_match <- pop_match %>%
  mutate(age_group = ifelse(age_group == 0, 1, age_group)) %>%
  group_by(year, age_group, sex) %>%
  summarise(pop = sum(pop)) %>%
  filter(year > 1996) %>%
  rename("AgeGroup" = "age_group", "fyear_pt1_dis" = "year")

### 11. Match the Scottish population file with the discharges file ----
discharges_with_pops <- merge(discharges_scot, pop_match,
                              by = c("fyear_pt1_dis", "AgeGroup", "sex"),
                              all = T)

discharges_with_pops <- discharges_with_pops %>%
  mutate(Discharges = replace_na(Discharges, 0))

### 12. Match the new file with the European population file ----
discharges_with_pops <- discharges_with_pops %>% arrange(AgeGroup)

esp_2013 <- read_spss(paste0(pop_folder, "ESP2013.sav"))

discharges_with_pops_and_ESP <- merge(discharges_with_pops,
                                      esp_2013,
                                      by.x = "AgeGroup",
                                      by.y = "agegroup",
                                      all = T)

### 13. Calculate EASRs for discharges ----
discharges_standardised_rates <- discharges_with_pops_and_ESP %>%
  mutate(discharges_EASR = Discharges * ESP2013 / pop) %>%
  group_by(fyear_pt1_dis) %>%
  summarise(discharges_EASR = sum(discharges_EASR)) %>%
  mutate(discharges_EASR = discharges_EASR / 2) %>%
  mutate(discharges_EASR = round(discharges_EASR, digits = 2))


### 14. Repeat the process for stays ----
cis_with_pops <- merge(cis_scot, pop_match,
                       by = c("fyear_pt1_dis", "AgeGroup", "sex"),
                       all = T)

cis_with_pops <- cis_with_pops %>%
  mutate(Stays = replace_na(Stays, 0))

cis_with_pops <- cis_with_pops %>% arrange(AgeGroup)

cis_with_pops_and_ESP <- merge(cis_with_pops,
                               esp_2013,
                               by.x = "AgeGroup",
                               by.y = "agegroup",
                               all = T)

cis_standardised_rates <- cis_with_pops_and_ESP %>%
  mutate(stays_EASR = Stays * ESP2013 / pop) %>%
  group_by(fyear_pt1_dis) %>%
  summarise(stays_EASR = sum(stays_EASR)) %>%
  mutate(stays_EASR = stays_EASR / 2) %>%
  mutate(stays_EASR = round(stays_EASR, digits = 2))


### 15. And again for patients ----
patients_with_pops <- merge(patients_scot, pop_match,
                            by = c("fyear_pt1_dis", "AgeGroup", "sex"),
                            all = T)

patients_with_pops <- patients_with_pops %>%
  mutate(Patients = replace_na(Patients, 0))

patients_with_pops <- patients_with_pops %>% arrange(AgeGroup)

patients_with_pops_and_ESP <- merge(patients_with_pops,
                                    esp_2013,
                                    by.x = "AgeGroup",
                                    by.y = "agegroup",
                                    all = T)

patients_standardised_rates <- patients_with_pops_and_ESP %>%
  mutate(patients_EASR = Patients * ESP2013 / pop) %>%
  group_by(fyear_pt1_dis) %>%
  summarise(patients_EASR = sum(patients_EASR)) %>%
  mutate(patients_EASR = patients_EASR / 2) %>%
  mutate(patients_EASR = round(patients_EASR, digits = 2))

### 16. Add all the EASR files together ----
scot_trend_standardised_rates <- merge(discharges_standardised_rates,
                                       cis_standardised_rates,
                                       by = "fyear_pt1_dis",
                                       all = T)
scot_trend_standardised_rates <- merge(scot_trend_standardised_rates,
                                       patients_standardised_rates,
                                       by = "fyear_pt1_dis",
                                       all = T)

### 17. Remove datasets no longer needed ----
rm(discharges_scot)
rm(data_stays)
rm(cis_scot)
rm(patients_scot)
rm(discharges_with_pops)
rm(cis_with_pops)
rm(patients_with_pops)
rm(discharges_with_pops_and_ESP)
rm(cis_with_pops_and_ESP)
rm(patients_with_pops_and_ESP)
rm(esp_2013)
rm(pop_match)
rm(data)
rm(discharges_standardised_rates)
rm(cis_standardised_rates)
rm(patients_standardised_rates)

### 18. Change the year format to financial years ----
scot_trend_standardised_rates <- scot_trend_standardised_rates %>%
  mutate(fyear_pt1_dis = as.numeric(fyear_pt1_dis),
         fyear_part_2 = fyear_pt1_dis + 1,
         fyear_part_2 = as.character(fyear_part_2),
         fyear_pt1_dis = as.character(fyear_pt1_dis),
         fyear_pt1_dis = paste0(fyear_pt1_dis, "/", substr(fyear_part_2,
                                                           3, 4))) %>%
  select(-fyear_part_2)

### 19. Save output as CSV ----
write.csv(scot_trend_standardised_rates,
          file = paste0(output, "Scotland trends - Standardised rates ",
                        min_year, "-", max_year, ".csv"),
          row.names = FALSE)

### 20. Clear global environment ----
rm(list = ls())
