### run a regression to estimate what would have happened in the absence of the pandemic for race/ethnicity groups state level

# load libraries
{
  library(tidyverse)
  # library(tidylog)
  library(lubridate)
  library(readxl)
  library(viridis)
  library(lme4)
  library(choroplethr)
  library(NatParksPalettes)
  library(MetBrewer)
  library(formula.tools)
  library(ggpubr)
  library(zoo)
  library(scales)
  library(grid)
  library(car)
}

### load data ####
# documentation here: https://www2.census.gov/programs-surveys/popest/technical-documentation/file-layouts/2020-2021/CO-EST2021-ALLDATA.pdf
recent_pop_data <- read_csv("data/input/co-est2021-alldata.csv") %>% # 2022 has CT updates we don't want
  dplyr::select(STATE, COUNTY, STNAME, CTYNAME, POPESTIMATE2020) %>% 
  filter(COUNTY != "000") %>% 
  mutate(fips = as.integer(paste0(STATE, COUNTY)),
         STATE = as.integer(as.numeric(STATE))) 

# use nyt state data in case any discrepancies
# from: https://github.com/nytimes/covid-19-data/blob/master/rolling-averages/us-states.csv
state_pop_data <- recent_pop_data %>% 
  group_by(STATE) %>% 
  summarise(state_pop = sum(POPESTIMATE2020))

nyt_state_case_data <- read_csv("data/input/nyt-us-states-rolling-avg.csv",
                                col_types = "Dcciddidd") %>%
  filter(date > ymd("2020-05-15"), date < ymd("2021-05-01")) %>% # less restrictive so boundary roll
  separate(geoid, into = c(NA, "state_fips")) %>%
  mutate(state_fips = as.integer(as.numeric(state_fips))) %>%
  filter(state_fips <= 56) %>% 
  # we need to complete this data because no row if no cases yet
  left_join(state_pop_data, by = c("state_fips" = "STATE")) %>% 
  mutate(week = round_date(date, unit = "week")) %>% 
  group_by(state_fips, week, state_pop) %>%
  summarise(state_cases = sum(cases), # no NAs
            state_cases = ifelse(state_cases < 0, 0, state_cases)) %>% # not sure why there are negs
  ungroup() %>% 
  tidyr::complete(week, state_fips, fill = list(state_cases = 0)) %>% 
  group_by(state_fips) %>% 
  mutate(state_cases_roll4 = round(zoo::rollmean(state_cases, k = 4, fill = NA, align = "right"), 2)) %>% 
  ungroup()


# use nyt data that already has national cases, in case there are some discrepancies
# I believe this is: https://github.com/nytimes/covid-19-data/blob/master/rolling-averages/us.csv
nyt_national_roll <- read_csv("data/input/nyt-us-national-rolling-avg.csv",
                              col_types = "Dciddidd") %>% 
  mutate(week = round_date(date, unit = "week")) %>% 
  group_by(week) %>%
  summarise(national_cases = sum(cases)) %>% 
  #national_cases = ifelse(national_cases < 0, 0, national_cases)) %>% # three negs?? 
  ungroup() %>% 
  mutate(national_cases_roll4 = zoo::rollmean(national_cases, k = 4, fill = NA, align = "right")) %>% 
  ungroup()


all_cases <- nyt_state_case_data %>% 
  left_join(nyt_national_roll) %>% 
  filter(week > ymd("2020-05-31"),
         week < ymd("2021-05-01")) # have to redo bc round to week

# read in accompanying data
urb_rur_codes <- read_excel("data/input/NCHSURCodes2013.xlsx") %>% 
  rename(fips = `FIPS code`) %>% 
  mutate(`CBSA 2012 pop` = as.integer(`CBSA 2012 pop`),
         `County 2012 pop` = as.integer(`County 2012 pop`)) %>% 
  dplyr::select(fips, `State Abr.`, `County name`, `CBSA title`, `2013 code`, `County 2012 pop`) %>% 
  rename(state = `State Abr.`, county = `County name`, area = `CBSA title`, ur_code = `2013 code`,
         population_2012 = `County 2012 pop`) %>%  # can ignore warnings
  mutate(ur_code = as.factor(ur_code),
         fips = as.integer(fips)) %>% ungroup() %>% 
  dplyr::select(fips, ur_code)

hhs_regions <- read_csv("data/input/HHS_regions.csv", col_types = "if") %>% 
  rename(fips = node, hhs_region = modularity_class)  %>% ungroup()

df.fips <- read_csv('data/input/state_and_county_fips_master.csv') %>% 
  mutate(fips = ifelse(fips == 46113, 46102, 
                       ifelse(fips == 2270, 2158, fips))) %>% 
  mutate(state_fips = as.integer(ifelse(nchar(as.character(fips)) == 5, substr(as.character(fips), 1, 2),
                                        substr(as.character(fips), 1, 1)))) %>% 
  filter(fips != 66) # guam

df.state <- df.fips %>% dplyr::select(state, state_fips) %>% distinct() %>% filter(!is.na(state))

fips_I_want <- data.frame(fips = c(6037, 12086, 4013, 22071, 53033, 48141,
                                   34013, 21111, 35001, 42003, 37183,
                                   48453, 18105, 36087, 1015, 23007, 33011,
                                   27003, 31019, 30029)) %>% 
  left_join(df.fips) %>% 
  mutate(full_name = paste0(gsub(" County", "", name), ", ", state)) %>% 
  arrange(fips)

fips_I_want_short <- fips_I_want %>% 
  filter(fips %in% c(30029, 31019, 1015, 33011, 27003,
                     4013, 6037, 48453, 12086, 21111, 53033, 34013))

policy_data <- read_csv("data/input/covs_updated_for_inla_6_10.csv", 
                        col_types = cols_only(fips = col_integer(),
                                              week_date = col_date(),
                                              state_SAH = col_double(),
                                              county_SAH = col_double(),
                                              state_gathering_ban = col_double(),
                                              county_gathering_ban = col_double(),
                                              state_rest_closure = col_double(),
                                              county_rest_closure = col_double(), 
                                              state_bar_closure = col_double(),
                                              county_bar_closure = col_double(),
                                              county_mask_mandate = col_double(),
                                              state_mask_mandate = col_double())) %>% 
  rename(week = week_date) %>% 
  mutate(across(c(state_SAH:county_mask_mandate),
                ~ifelse(is.na(.x), 0, .x))) %>% 
  mutate(sum_county_measures = county_SAH + county_gathering_ban + county_rest_closure +
           county_bar_closure + county_mask_mandate,
         sum_state_measures = state_SAH + state_gathering_ban + state_rest_closure +
           state_bar_closure + state_mask_mandate) %>% 
  filter(week > ymd("2020-10-01")) %>% 
  group_by(fips) %>% 
  mutate(min_sum_county_measures = min(sum_county_measures)) %>% 
  ungroup() %>% 
  mutate(sum_county_measures_shift = sum_county_measures - min_sum_county_measures)

# try oxford data
full_oxdata <- read_csv("data/stringency/OxCGRT_compact_subnational_v1.csv") %>% 
  filter(CountryCode == "USA", !is.na(RegionName)) %>% 
  dplyr::select(-contains("Flag"), # remove these columns
                -c(CountryCode, CountryName, RegionName, CityName, CityCode, Jurisdiction)) %>% 
  mutate(date = ymd(Date),
         week = round_date(date, unit = "week")) %>% 
  separate(RegionCode, into = c(NA, "state"), sep = "_") # get state abbreviation

oxdata_week <- full_oxdata %>% 
  dplyr::select(-Date) %>% 
  group_by(state, week) %>% # three combined indices of interest at week-state level
  summarise(StringencyIndex_Average = mean(StringencyIndex_Average),
            ContainmentHealthIndex_Average = mean(ContainmentHealthIndex_Average),
            GovernmentResponseIndex_Average = mean(GovernmentResponseIndex_Average)) %>% 
  ungroup() %>% 
  group_by(state) %>% 
  mutate(StringencyIndex_Average_roll3 = zoo::rollmean(StringencyIndex_Average, k = 3, align = "center", fill = NA)) %>% 
  ungroup()

state_min_strin <- oxdata_week %>%
  filter(week > ymd("2022-06-01")) %>% 
  group_by(state) %>%
  summarise(min_strin_2022 = min(StringencyIndex_Average_roll3, na.rm = T))

state_mean_strin_study_pd <- oxdata_week %>%
  filter(week > ymd("2020-10-01"), week < ymd("2021-05-01")) %>%
  group_by(state) %>%
  summarise(tail_strin = tail(StringencyIndex_Average_roll3, 1),
            min_strin = min(StringencyIndex_Average_roll3, na.rm = T))

oxdata <- oxdata_week %>% 
  filter(week >= ymd("2020-10-01"), week <= ymd("2021-05-01")) %>% 
  left_join(state_mean_strin_study_pd) %>% 
  left_join(state_min_strin) %>% 
  mutate(StringencyIndex_Average_roll3_shifttail = StringencyIndex_Average_roll3 - tail_strin,
         StringencyIndex_Average_roll3_shiftmin = StringencyIndex_Average_roll3 - min_strin,
         StringencyIndex_Average_roll3_shift2022 = StringencyIndex_Average_roll3 - min_strin_2022) %>% 
  group_by(state) %>% 
  mutate(StringencyIndex_Average_z = c(scale(StringencyIndex_Average))) %>% 
  ungroup() %>% 
  group_by(state) %>% 
  mutate(StringencyIndex_Average_z_mincentered = c(scale(StringencyIndex_Average_roll3_shiftmin, center = F)))


dataverse_vax_data <- read_csv("data/input/COVID_county_vacc_data_dataverse.csv") %>% 
  mutate(fips = as.integer(COUNTY),
         STATE = as.integer(STATE),
         percent_vaxxed = CASES * 100,
         week = floor_date(converted_date, unit = "week")) %>% 
  mutate(people_vaxxed = percent_vaxxed/100 * POPN) %>% 
  group_by(STATE, week) %>% 
  summarise(people_vaxxed = sum(people_vaxxed)) %>% 
  ungroup() %>% 
  left_join(state_pop_data) %>% 
  mutate(percent_vaxxed = people_vaxxed / state_pop * 100) %>% 
  left_join(df.state, by = c("STATE"= "state_fips")) %>% 
  dplyr::select(state, week, percent_vaxxed)

mobility_scale <- read_csv("data/output/mobility_19_20_fall_ratio_new_norm_state.csv", col_types = "fdddfic")


# need to do same counties across all datasets

state_week_by_race <- read_csv("data/group_means_rake/contact_by_race_week_state_trunc72.csv",
                               col_types = "Dffddi") %>% # check these
  mutate(nonhh_contacts_rd = round(non_hh_contacts)) %>%
  filter(week > ymd("2020-04-30"), week < ymd("2021-05-01"))

# use same 10 per unit cutoff as with gender and age to determine which states to run model for
for(race_level in c("asian", "black", "hispanic", "other", "white")){
  
  filtered_states <- state_week_by_race %>% 
    filter(race_cat_col == race_level) %>% 
    ungroup() %>%
    filter(week >= ymd("2020-10-01"), 
           week <= ymd("2021-04-25")) %>%
    mutate(enough = ifelse(samp_size >= 10, 1, 0)) %>% 
    group_by(state) %>% 
    summarise(sum_samp = sum(enough, na.rm = T)) %>% 
    filter(sum_samp == 30) %>% # 30 weeks 
    pull(state)
  
  if(race_level == "asian"){
    asian_states <- filtered_states
  }else if (race_level == "black"){
    black_states <- filtered_states
  }else if(race_level == "hispanic"){
    hispanic_states <- filtered_states
  }else if(race_level == "other"){
    other_states <- filtered_states
  }else if (race_level == "white"){
    white_states <- filtered_states
  }
}

large_enough_race_states <- 
  intersect(asian_states,
            intersect(black_states,
                      intersect(hispanic_states,
                                intersect(other_states, white_states))))
### fitted contact data
#big_vif_df <- data.frame() # I guess doesn't depend on race
all_estimates <- data.frame()
for(race_level in c("asian", "black", "hispanic", "other", "white")){
  
  folder_name <- "ethrace_72trunc_region4_m1"
  folder_sublevel = paste0(race_level)
  spatiotemporal_fits <- read_csv(paste0("data/output/", folder_name, "/fitted_predictions_", folder_sublevel, ".csv"),
                                  col_types = "ddciDfddiicfcD")
  
  combine_data <- 
    spatiotemporal_fits %>% dplyr::select(fit, state, week, month, samp_size, non_hh_contacts, race_cat_col) %>%
    left_join((all_cases %>% left_join(df.state, by = "state_fips")), by = c("state", "week")) %>% 
    left_join(oxdata, by = c("week", "state")) %>% 
    filter(state != "AK") %>% 
    filter(state %in% large_enough_race_states) %>% 
    rename(contact_fit = fit) %>% 
    mutate(state = as.factor(state)) %>% 
    filter(week >= ymd("2020-10-01"), 
           week < ymd("2021-05-01")) 
  
  # need to filter some things
  #   AK: missing election data
  #   NYC boroughs are lumped into one for cases so not useful
  #   some other fips missing rolling averages due to NYT data (around 169)
  #   need to have contact estimates for each week, I think we completed so this should be ok?
  
  #---------------#
  ##### MODEL #####
  #---------------#
  
  state_nat_model <- lmList(
    contact_fit ~ 1 + national_cases_roll4 * percent_vaxxed + StringencyIndex_Average_roll3_shiftmin | state, combine_data
  )
  
  # vif_df <- data.frame()
  # for(i in 1:length(large_enough_race_states)){
  #   vif_df <- vif_df %>% bind_rows(data.frame(state = large_enough_race_states[[i]],
  #                                             vif = car::vif(state_nat_model[[i]])) %>% 
  #                                    rownames_to_column("metric") %>% 
  #                                    pivot_wider(names_from = "metric", values_from = "vif"))
  # }
  #big_vif_df <- big_vif_df %>% bind_rows(vif_df %>% mutate(race_cat_col = race_level))
  
  estimates <- combine_data %>% 
    left_join(coef(state_nat_model) %>% 
                rownames_to_column("state") %>% # convert state rownames into a column
                as_tibble() %>% 
                mutate(state = as.factor(state)) %>% 
                rename(intercept = `(Intercept)`) %>% 
                rename_with(~paste0(., "_slope"), -c(state, intercept)), 
              by = "state") %>% 
    cbind(residual = resid(state_nat_model)) %>% 
    cbind(pred_lm = predict(state_nat_model)) %>% 
    mutate(baseline = intercept + residual) %>% 
    ungroup() %>% 
    left_join(mobility_scale) %>% 
    mutate(scale_baseline = baseline * phi)
  
  #write_csv(estimates, paste0("data/output/baseline_contact_by_state_week_", folder_sublevel, ".csv"))
  
  all_estimates <- all_estimates %>% bind_rows(estimates)
  
}

write_csv(all_estimates, "data/output/baseline_contact_by_state_week_by_race.csv")



