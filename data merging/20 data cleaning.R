library(readxl)
library(dplyr)
library(ggplot2)
library(pscl)
library(pROC)
library(tidyr)

# This coding serves the goal for merging data from NHANES
# For the description of new variables, please review the code book
# first import the data set and set the last column to be numeric

ENARdata <- read_xlsx("merged_99-20_data.xlsx")

# mutate all the yes and no to 1 and 0
ENARdata_01 <- ENARdata %>% 
  mutate(across(where(is.character), ~ ifelse(. == "Yes", 1, ifelse(. == "No", 0, .))))

# import all the data set
# the following 3 dataset is for left join
AlcoholUse <- read_xlsx("AlcoholUse_pre2020.xlsx")
FoodSecLevel <- read_xlsx("FoodSecLevel_pre2020.xlsx")
Insulin <- read_xlsx("INS_pre2020.xlsx")

#drop ...1
Insulin <- Insulin %>% 
  select(-`...1`)


# merge the alcohol, food secure, and Insulin into the big dataset 
ENARdata_01 <- ENARdata_01 %>%
  dplyr::rename(SEQN = svy_id)

ENARdata_01$SEQN <- as.numeric(ENARdata_01$SEQN)

ENARdata_Merge <- left_join(ENARdata_01, Insulin, by = "SEQN")
ENARdata_Merge <- left_join(ENARdata_Merge, AlcoholUse, by = "SEQN")
ENARdata_Merge <- left_join(ENARdata_Merge, FoodSecLevel, by = "SEQN")

ENARdata_Merge <- ENARdata_Merge %>% select(-svy_year.x.x, -svy_year.y.y, -svy_year.y)

ENARdata_Merge <- ENARdata_Merge %>% 
  dplyr::rename(svy_year = svy_year.x)


# define severe or extreme cases: if sys_mean > 180 or dia_mean > 120
# creating a variable that indicating whether the individual is under which stage of hypertension
# stages: 
# Normal: SBP < 120 and DBP < 80
# Elevated: SBP 120-129 and DBP < 80
# Stage 1 Hypertension: SBP 130-139 or DBP 80-89
# Stage 2 Hypertension: SBP >= 140 or DBP >= 90
# Hypertensive Crisis: SBP > 180 or DBP >= 120


# NA removing
ENARdata_Merge_noNA <- drop_na(ENARdata_Merge)
ENARdata_clean_20 <- drop_na(ENARdata_Merge, insulin_uUmL, insulin_SI_pmolL, FoodSec_Level, more12drinks_per_year, avg_num_daysdrink_lastyr, chol_ldl)

ENARdata_Merge <- ENARdata_Merge %>%
  mutate(days_over_5_drinks = case_when(
    days_over_5_drinks == 0 ~ 0,
    days_over_5_drinks %in% 1:2 ~ 10,
    days_over_5_drinks %in% 3:6 ~ 9,
    days_over_5_drinks %in% 7:11 ~ 8,
    days_over_5_drinks %in% 12:23 ~ 7,
    days_over_5_drinks %in% 24:35 ~ 6,
    days_over_5_drinks %in% 36:51 ~ 5,
    days_over_5_drinks %in% 52:103 ~ 4,
    days_over_5_drinks %in% 104:155 ~ 3,
    days_over_5_drinks %in% 156:312 ~ 2,
    days_over_5_drinks >= 313 ~ 1,
  ))

ENARdata_clean_20 <- ENARdata_clean_20 %>%
  mutate(
    Highbp = case_when(
       #If bp_cat_meds_ixcluded is NA, keep it NA
      is.na(bp_cat_meds_included) ~ NA_real_,
      
       #If bp_cat_meds_ixcluded == "SBP < 120 and DBP < 80", Highbp = 0
      bp_cat_meds_included == "SBP <120 and DBP <80 mm Hg" ~ 0,
      
      #bp_cat_meds_included == "	taking antihypertensive medications" ~ 0,
       #Otherwise, Highbp = 1
      TRUE ~ 1
    )
  )

table(ENARdata_clean_20$Highbp)



#NA checking for clean
NA_check <- ENARdata_clean_20 %>%
  select(-average_physical_activity) %>%  #excluding physical activity
  summarise(across(everything(), ~ sum(is.na(.))))

# drop dublicated hypertension indicator and bp values
ENARdata_clean_20_drop <- ENARdata_clean_20 %>% 
  select(-starts_with("bp"), -starts_with("htn"), -svy_subpop_htn, -svy_weight_mec, -svy_strata, -svy_psu, -svy_subpop_chol)

# modify character variable to categorical number:
# modify BMI
ENARdata_clean_20_drop <- ENARdata_clean_20_drop%>%
  mutate(cc_bmi = case_when(
    cc_bmi == "<25" ~ 0,
    cc_bmi == "25 to <30" ~ 1,
    cc_bmi == "30 to <35" ~ 2,
    cc_bmi == "35+" ~ 3, 
    TRUE ~ NA_real_
  ))

#table(ENARdata_clean_20_drop$bmi)
table(ENARdata_clean_20_drop$cc_bmi)
sum(is.na(ENARdata_clean_20_drop$cc_bmi))

#ENARdata_clean_20_drop <- ENARdata_clean_20_drop %>% select(-bmi)


# modify gender:
ENARdata_clean_20_drop <- ENARdata_clean_20_drop %>%
  mutate(demo_gender = case_when(
    demo_gender == "Men" ~ 1,
    demo_gender == "Women" ~ 2,
    TRUE ~ as.numeric(demo_gender)
  ))

#table(ENARdata_clean_20_drop$gender)
table(ENARdata_clean_20_drop$demo_gender)
sum(is.na(ENARdata_clean_20_drop$demo_gender))
#ENARdata_clean_20_drop <- ENARdata_clean_20_drop %>% select(-gender)

# modify race
ENARdata_clean_20_drop <- ENARdata_clean_20_drop%>%
  mutate(demo_race = case_when(
    demo_race == "Hispanic" ~ 1,
    demo_race == "Non-Hispanic White" ~ 2,
    demo_race == "Non-Hispanic Black" ~ 3,
    demo_race == "Non-Hispanic Asian" ~ 4,
    demo_race == "Other" ~ 5,
    TRUE ~ NA_real_
  ))
#table(ENARdata_clean_20_drop$race)
table(ENARdata_clean_20_drop$demo_race)
#ENARdata_clean_20_drop <- ENARdata_clean_20_drop %>% select(-race)
class(ENARdata_clean_20_drop$demo_race)

# modify age 
ENARdata_clean_20_drop <- ENARdata_clean_20_drop %>%
  mutate(demo_age_cat = case_when(
    demo_age_cat %in% c("18 to 44", "18-44") ~ 1,
    demo_age_cat %in% c("45 to 64", "45-64") ~ 2,
    demo_age_cat %in% c("65 to 74", "65-74") ~ 3,
    demo_age_cat %in% c("75+", "75 and above") ~ 4,
    TRUE ~ NA_real_
  ))

#table(ENARdata_clean_20_drop$age_cat)
table(ENARdata_clean_20_drop$demo_age_cat)
#ENARdata_clean_20_drop <- ENARdata_clean_20_drop %>% select(-age_cat)

# modify smoke
ENARdata_clean_20_drop <- ENARdata_clean_20_drop %>%
  mutate(cc_smoke = case_when(
    cc_smoke == "Never" ~ 0,
    cc_smoke == "Current" ~ 1,
    cc_smoke == "Former" ~ 2,
  ))


#table(ENARdata_clean_20_drop$smoke)
table(ENARdata_clean_20_drop$cc_smoke)
#ENARdata_clean_20_drop <- ENARdata_clean_20_drop %>% select(-smoke)


# output to data file
write.csv(ENARdata_Merge, "ENAR_Merge_Raw.csv")
write.csv(ENARdata_clean_20_drop, "ENAR_Merge_Clean.csv")


# variable check
table(ENAR_Merge_Clean_20_drop$cc_bmi)
table(ENAR_Merge_Clean_20_drop$Highbp)
table(ENAR_Merge_Clean_20_drop$demo_age_cat)

table(ENARdata_clean_20$cc_bmi)
sum(is.na(ENARdata_clean_20$cc_bmi))

