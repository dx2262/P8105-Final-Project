library(tidyverse)
library(plotly)
library(readxl)
library(janitor)
library(sf)
library(patchwork)
df_parks <- read_csv("data/Parks_Properties_20251104.csv") |>
  select(ACQUISITIONDATE: multipolygon) |>
  clean_names() 

df_chp <- read_excel("data/2022-chp-pud.xlsx", sheet = "CHP_all_data", skip = 1) |>
  clean_names()

df_prem_death <- read_excel("data/2022-chp-pud.xlsx", sheet = "Cause_of_premature_death") |>
  clean_names()

df_cancer <- read_excel("data/2022-chp-pud.xlsx", sheet = "Cancer_data_ranked") |>
  clean_names()
df_parks <- 
  df_parks |>
  rename(id = communityboard) 

df_parks_commubity <-
  df_parks |>
  mutate(
    acres = as.numeric(acres),
    id = as.factor(id)
  ) |>
  group_by(id) |>
  summarise(
    sum_acres = sum(acres, na.rm = TRUE),
    mean_acres = mean(acres, na.rm = TRUE),
    n_parks = n()
  ) |>
  filter(sum_acres > 0) |> # only use observations with positive areas
  mutate(
    id = as.character(id),
    n_split = lengths(strsplit(id, ","))
  ) |>
  separate_rows(id, sep = ",") |>
  mutate(
    id = trimws(id),
    sum_acres_adj = sum_acres / n_split,
    n_parks_adj = n_parks / n_split
  ) |>
  group_by(id) |>
  summarise(
    sum_acres = sum(sum_acres_adj, na.rm = TRUE),
    n_parks = sum(n_parks_adj, na.rm = TRUE),
    mean_acres = sum_acres / n_parks
  )

df_parks_borough <-
  df_parks |>
  filter(
    borough == c("B", "M", "Q", "R", "X")
  ) |>
  mutate(
    borough = as.factor(borough),
    acres = as.numeric(acres)
  ) |>
  group_by(borough) |>
  summarise(
    sum_acres = sum(acres, na.rm = TRUE),
    mean_acres = mean(acres, na.rm = TRUE),
    n_parks = n()
  ) |>
  rename(id = borough)

df_parks_all_dist <-
  bind_rows(df_parks_borough, df_parks_commubity)
df_chp_clean <- df_chp |>
  select(-contains("95cl"), -contains("nyc_comparison"), -contains("reliability_note")) |>
  mutate(
    id = recode(as.character(id),
                "1" = "M",
                "2" = "X",
                "3" = "B",
                "4" = "Q",
                "5" = "R"),
    
  )
df_chp_clean <- df_chp_clean |>
  mutate(uninsured = as.numeric(uninsured),
         uninsured = round(uninsured, 2))

# create a variable map for df_chp_clean
var_map <- tribble(
  ~variable,                 ~category,
  
  "id",                      "geo",
  "borough",                 "geo",
  "name",                    "geo",
  "overall_pop",             "population",
  
  
  "race_white",              "demographic_race",
  "race_black",              "demographic_race",
  "race_latino",             "demographic_race",
  "race_asian",              "demographic_race",
  "race_other",              "demographic_race",
  "age0to17",                "demographic_age",
  "age18to24",               "demographic_age",
  "age25to44",               "demographic_age",
  "age45to64",               "demographic_age",
  "age65plus",               "demographic_age",
  "born_outside_us",         "demographic_migration",
  "ltd_eng_prof",            "demographic_language",
  
  
  "edu_did_not_complete_hs",      "SES_education",
  "edu_hs_grad_some_college",     "SES_education",
  "edu_college_degree_and_higher","SES_education",
  "poverty",                      "SES_economic",
  "unemployment",                 "SES_economic",
  "rent_burden",                  "SES_economic",
  "school_absent",                "education_outcome",
  "on_time_hs_grad",              "education_outcome",
  
  
  "homes_ac",                     "housing_environment",
  "homes_no_defects",             "housing_environment",
  "air_pollution",                "physical_environment",
  "ratio_bodega_supermarket",     "food_environment",
  "farmers_markets",              "food_environment",
  
  
  "helpful_neighbor",             "social_environment",
  "assault_hosp",                 "safety_violence",
  "jail_incarceration",           "safety_justice",
  "pedestrian_hosp",              "safety_transport",
  
  
  "physical_activity",            "behavior",
  "fruit_veg",                    "behavior",
  "sugary_drink",                 "behavior",
  "smoking",                      "behavior",
  "binge_drink",                  "behavior",
  
  
  "uninsured",                    "access_care",
  "unmet_med_care",               "access_care",
  "hpv_vaccination_all",          "preventive_care",
  "flu_vaccination",              "preventive_care",
  
  
  "preterm_births",               "maternal_child",
  "teen_births",                  "maternal_child",
  "child_obesity",                "maternal_child",
  "infant_mort",                  "maternal_child",
  
  
  "obesity",                      "chronic_disease",
  "diabetes",                     "chronic_disease",
  "hypertension",                 "chronic_disease",
  "hiv_diagnoses",                "infectious_disease",
  "hep_c_reports",                "infectious_disease",
  "avoidable_child_hosp",         "hospitalization",
  "avoidable_adult_hosp",         "hospitalization",
  "falls_hosp",                   "injury_hospitalization",
  "psych_hosp",                   "mental_health",
  
  
  "avertable_death",              "mortality",
  "premature_mort_rate",          "mortality",
  "premature_mort_number",        "mortality",
  "life_expectancy",              "global_health",
  "self_rep_health",              "global_health"
)
# demographic data
var_demo <- var_map |>
  filter(category %in% c("geo", "population", "demographic_race", "demographic_age","demographic_migration", "demographic_language")) |>
  pull(variable)

df_demographic <-
  df_chp_clean |>
  select(all_of(var_demo)) |>
  mutate(
    age0to24 = age0to17 + age18to24,
    age25to64 = age25to44 + age45to64,
    race_others = race_asian + race_other
  ) |>
  select(-race_asian, -race_other, -age0to17, -age18to24, -age25to44, -age45to64) |>
  relocate(
    c(age0to24, age25to64), .before = age65plus
  ) |>
  relocate(race_others, .after = race_latino)


# education condition
var_edu <- var_map |>
  filter(category %in% c("geo", "SES_education")) |>
  pull(variable)

df_education <- df_chp_clean |>
  select(all_of(var_edu)) |>
  mutate(edu_no_college_degree = edu_did_not_complete_hs + edu_hs_grad_some_college) |>
  select(-edu_did_not_complete_hs, -edu_hs_grad_some_college)


# economic condition
var_eco <- var_map |>
  filter(category %in% c("geo","SES_economic"))|>
  pull(variable)

df_economy <- df_chp_clean |>
  select(all_of(var_eco)) |>
  select(-rent_burden)


# food condition
var_food <- var_map |>
  filter(category %in% c("geo", "food_environment", "behavior")) |>
  pull(variable)

df_food <- df_chp_clean |>
  select(all_of(var_food))|>
  select(-ratio_bodega_supermarket, -physical_activity, -smoking, -binge_drink)


# smoking and drinking
df_smoke_drink <- df_chp_clean |>
  select(id, borough, name, smoking, binge_drink)


# activity
df_activity <- df_chp_clean |>
  select(id, borough, name, physical_activity)


# safety condition
var_safety <- var_map |>
  filter(category %in% c("geo", "safety_violence", "social_environment", "safety_transport")) |>
  pull(variable)

df_safety <- df_chp_clean |>
  select(all_of(var_safety))


# medical availability
df_med_avail <- df_chp_clean |>
  select(id, borough, name, uninsured) 


# health conditions
df_health <- df_chp_clean |>
  select(id, obesity, life_expectancy)


dfs <- list(df_demographic, df_health, df_parks_all_dist, df_education, df_economy, df_food, df_smoke_drink, df_activity, df_safety, df_med_avail)


# get a final dataframe for analysis
df_analysis <- reduce(dfs, left_join, by = "id") |>
  select(-contains("borough.x.x"), -contains("name.x.x"), -contains("borough.y"), -contains("name.y")) |>
  rename(
    borough = borough.x,
    name = name.x
  ) |>
  filter(overall_pop != 0) |>
  mutate(
    area_per_capita = (sum_acres / overall_pop) * 100000,
    log_area = log(area_per_capita + 1)) 

