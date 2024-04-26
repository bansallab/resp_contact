### run a regression to estimate what would have happened in the absence of the pandemic

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
}

### load data ####
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
                       ifelse(fips == 2270, 2158, fips)))

spatiotemporal_fits <- read_csv("data/output/normal_gamma2_72trunc/fitted_predictions.csv",
                                col_types = "ddiiddddddddiiDccDccf") %>% ungroup()


# documentation here: https://www2.census.gov/programs-surveys/popest/technical-documentation/file-layouts/2020-2021/CO-EST2021-ALLDATA.pdf
recent_pop_data <- read_csv("data/input/co-est2021-alldata.csv") %>% # 2022 has CT updates we don't want
  dplyr::select(STATE, COUNTY, STNAME, CTYNAME, POPESTIMATE2020) %>% 
  filter(COUNTY != "000") %>% 
  mutate(fips = as.integer(paste0(STATE, COUNTY)),
         STATE = as.integer(as.numeric(STATE))) 

# from: https://github.com/nytimes/covid-19-data/tree/master/rolling-averages, downloaded on 03/08/2023
# I believe this is: https://github.com/nytimes/covid-19-data/blob/master/rolling-averages/us-counties-2020.csv and 2021
# try my own rolling average from NYT data, county level
nyt_county_case_data <- read_csv("data/input/nyt-us-counties-rolling-avg-2020.csv",
                                 col_types = "Dccciddidd") %>%
  bind_rows(read_csv("data/input/nyt-us-counties-rolling-avg-2021.csv",
                     col_types = "Dccciddidd")) %>% 
  filter(date > ymd("2020-05-15"), date < ymd("2021-05-01")) %>% # less restrictive so boundary roll
  separate(geoid, into = c(NA, "fips")) %>%
  mutate(fips = as.integer(as.numeric(fips))) %>%
  filter(fips < 60000) %>% 
  # we need to complete this data because no row if no cases yet
  left_join(recent_pop_data %>% dplyr::select(fips, POPESTIMATE2020), by = "fips") %>% 
  mutate(week = round_date(date, unit = "week")) %>% 
  group_by(fips, week, POPESTIMATE2020) %>%
  summarise(county_cases = sum(cases), # no NAs
            county_cases = ifelse(county_cases < 0, 0, county_cases)) %>% # not sure why there are negs
  ungroup() %>% 
  tidyr::complete(week, fips, fill = list(county_cases = 0)) %>% 
  group_by(fips) %>% 
  mutate(county_cases_roll3 = round(zoo::rollmean(county_cases, k = 3, fill = NA, align = "right"), 2),
         county_cases_roll4 = round(zoo::rollmean(county_cases, k = 4, fill = NA, align = "right"), 2),
         county_cases_roll3_per100k = round(county_cases_roll3/(POPESTIMATE2020/1e5), 2),
         county_cases_roll4_per100k = round(county_cases_roll4/(POPESTIMATE2020/1e5), 2)) %>% 
  ungroup() %>% 
  mutate(state_fips = as.integer(as.numeric(ifelse(nchar(as.character(fips)) == 4, 
                                                   substr(as.character(fips), 1, 1),
                                                   substr(as.character(fips), 1, 2)))))

# per 100k deals with some of the scaling issues, similar to how zscores would
# may need it for national though so that national and county are on the same scale

# use nyt state data in case any discrepancies
# from: https://github.com/nytimes/covid-19-data/blob/master/rolling-averages/us-states.csv
state_pop_data <- recent_pop_data %>% 
  group_by(STATE, ) %>% 
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
  mutate(state_cases_roll3 = round(zoo::rollmean(state_cases, k = 3, fill = NA, align = "right"), 2),
         state_cases_roll4 = round(zoo::rollmean(state_cases, k = 4, fill = NA, align = "right"), 2),
         state_cases_roll3_per100k = round(state_cases_roll3/(state_pop/1e5), 2),
         state_cases_roll4_per100k = round(state_cases_roll4/(state_pop/1e5), 2)) %>% 
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
  mutate(national_cases_roll3 = zoo::rollmean(national_cases, k = 3, fill = NA, align = "right"),
         national_cases_roll4 = zoo::rollmean(national_cases, k = 4, fill = NA, align = "right"),
         national_cases_roll3_per100k = round(national_cases_roll3/(331900000/1e5), 2), # total us pop
         national_cases_roll4_per100k = round(national_cases_roll4/(331900000/1e5), 2)) %>% 
  ungroup()


all_cases <- nyt_county_case_data %>% 
  left_join(nyt_state_case_data) %>% 
  left_join(nyt_national_roll) %>% 
  filter(week > ymd("2020-05-31"),
         week < ymd("2021-05-01")) # have to redo bc round to week

# not doing state cases, but could download from NYT and follow same steps if necessary

# from here: https://dataverse.harvard.edu/dataset.xhtml?persistentId=doi:10.7910/DVN/VOQCHQ
election_data_2020 <- read_csv("data/input/mit_election_data/countypres_2000-2020.csv",
                               col_types = "iccciccciiic") %>% 
  filter(year == 2020, party %in% c("DEMOCRAT", "REPUBLICAN")) %>% 
  dplyr::select(state_po, county_fips, totalvotes, party, candidatevotes, mode) %>% 
  mutate(county_fips = ifelse(state_po == "DC", 11001, county_fips)) %>%  # DC fips is NA for some reason
  # Alaska districts are weird so we may just need to exclude
  group_by(state_po, county_fips, party, totalvotes) %>% 
  summarise(candidatevotes = sum(candidatevotes)) %>% 
  pivot_wider(names_from = party, values_from = candidatevotes) %>% 
  mutate(prop_dem = DEMOCRAT/totalvotes,
         prop_gop = REPUBLICAN/totalvotes) %>% 
  ungroup()

# try oxford data
oxdata <- read_csv("data/stringency/OxCGRT_compact_subnational_v1.csv") %>% 
  filter(CountryCode == "USA", !is.na(RegionName)) %>% 
  dplyr::select(-contains("Flag"), # remove these columns
                -c(CountryCode, CountryName, RegionName, CityName, CityCode, Jurisdiction)) %>% 
  mutate(date = ymd(Date),
         week = round_date(date, unit = "week")) %>% 
  separate(RegionCode, into = c(NA, "state"), sep = "_") %>% # get state abbreviation
  filter(week >= ymd("2020-06-01"), week <= ymd("2021-05-01")) %>% 
  dplyr::select(-Date) %>% 
  group_by(state, week) %>% # three combined indices of interest at week-state level
  summarise(StringencyIndex_Average = mean(StringencyIndex_Average),
            ContainmentHealthIndex_Average = mean(ContainmentHealthIndex_Average),
            GovernmentResponseIndex_Average = mean(GovernmentResponseIndex_Average))

combine_data <- 
  spatiotemporal_fits %>% dplyr::select(fit, se.fit, fips, week, month, samp_size, non_hh_contacts) %>% 
  left_join(all_cases %>% dplyr::select(-POPESTIMATE2020), by = c("fips", "week")) %>% 
  left_join(urb_rur_codes, by = "fips") %>% 
  left_join(df.fips, by = "fips") %>% 
  left_join(hhs_regions, by = "fips") %>% 
  left_join(election_data_2020 %>%
              dplyr::select(county_fips, prop_gop) %>% 
              rename(fips = county_fips, prop_gop_2020 = prop_gop),
            by = "fips") %>%
  left_join(oxdata, by = c("week", "state")) %>% 
  left_join(recent_pop_data, by = "fips") %>% 
  filter(! fips %in% c(36005, 36047, 36061, 36081, 36085)) %>% 
  filter(state != "AK") %>% 
  #filter(! fips%in% fips_missing_data) %>% # need to fix 9/4/23 
  rename(contact_fit = fit) %>% 
  mutate(fips = as.factor(fips),
         state = as.factor(state)) %>% 
  filter(week <= ymd("2021-05-01"),
         week > ymd("2020-06-01"))

combine_data$hhs_region <- factor(combine_data$hhs_region,
                                  levels = c("1", "2", "3", "4", "5", "6", "7", "8", "9", "10"))
# need to filter some things
#   AK: missing election data
#   NYC boroughs are lumped into one for cases so not useful
#   some other fips missing rolling averages due to NYT data (around 169)
#   need to have contact estimates for each week, I think we completed so this should be ok?


# # fips_to_plot <- sample(unique(lagged_input$fips), 25)
fips_to_plot_ur <- data.frame(fips = c(29095, 26163, 6067, 36055, 34039, # 1
                                       48055, 24017, 51650, 34031, 41071, # 2
                                       28121, 42049, 55045, 42099, 23031, # 3
                                       47019, 53015, 35013, 51069, 12005, # 4
                                       8045, 13031, 29105, 17099, 5005, # 5
                                       29029, 47123, 8029, 19125, 17135)) %>%  # 6
  left_join(df.fips) %>% 
  mutate(full_name = paste0(gsub(" County", "", name), ", ", state))

fips_I_want <- data.frame(fips = c(6037, 12086, 4013, 22071, 53033, 48141,
                                   34013, 21111, 35001, 42003, 37183,
                                   48453, 18105, 36087, 1015, 23007, 33011,
                                   27003, 31019, 30073)) %>% 
  left_join(df.fips) %>% 
  mutate(full_name = paste0(gsub(" County", "", name), ", ", state)) %>% 
  arrange(fips)

fips_I_want_short <- fips_I_want %>% 
  filter(fips %in% c(30073, 31019, 1015, 33011, 27003,
                     4013, 6037, 48453, 12086, 21111, 53033, 34013))
fips_I_want_shorter <- fips_I_want %>% 
  filter(fips %in% c(30073, 31019, 1015, 33011, 27003,
                     4013, 48453, 12086, 53033, 34013))


#-------------------------------#
##### PARTIAL POOLING MODEL #####
#-------------------------------#

combine_data %>% ggplot(aes(x = national_cases_roll4, y = contact_fit)) +
  geom_point(alpha = 0.05) +
  geom_smooth(method = "lm") +
  scale_x_log10() +
  theme_bw() +
  labs(y = "Contact", x = "National incidence\n(4 week rolling average)")
ggsave("figures/supp/disease-contact-correlation-nat.pdf", height = 5, width = 8)


model_data <- combine_data %>% 
  filter(week >= ymd("2020-10-01"),
         week <= ymd("2021-05-01")) %>%
  ungroup() %>% 
  mutate(log_contact = log(contact_fit)) %>% 
  # need to zscore cases now due to scale issues
  group_by(fips) %>% 
  # scale function takes zscore, use c bc scale works on matrix
  mutate(county_cases_roll3z = c(scale(county_cases_roll3)),
         national_cases_roll3z = c(scale(national_cases_roll3)),
         county_cases_roll4z = c(scale(county_cases_roll4)),
         national_cases_roll4z = c(scale(national_cases_roll4)),
         oxz = c(scale(StringencyIndex_Average))) %>% 
  ungroup() %>% 
  dplyr::select(-contains("state_cases"))

no_pooling <- lmList(
  contact_fit ~ 1 + 
    oxz +
    county_cases_roll4 + 
    national_cases_roll4 | fips, model_data
)

suff_samp_fips <- spatiotemporal_fits %>% 
  filter(week >= ymd("2020-10-01"),
         week <= ymd("2021-04-25")) %>%
  mutate(enough = ifelse(samp_size >= 10, 1, 0)) %>% 
  group_by(fips) %>%
  summarise(sum_samp = sum(enough, na.rm = T)) %>%
  filter(sum_samp == 30) %>%  # 30 weeks
  dplyr::select(fips) %>% 
  mutate(suff_samp = TRUE)

library(car)
vifs <- lapply(no_pooling, vif) %>%
  as.data.frame() %>%
  t() %>%
  as.data.frame() %>%
  rownames_to_column(var = "fips") %>%
  separate(fips, into = c(NA, "fips"), sep = "X") %>%
  mutate(fips = as.integer(as.numeric(fips))) %>%
  rename(policy_vif = oxz,
         county_vif = county_cases_roll4,
         national_vif = national_cases_roll4) %>%
  mutate(high_vif = #county_vif >= 5 | 
           national_vif >= 5 | policy_vif >= 5) %>%
  left_join(recent_pop_data) %>% 
  left_join(suff_samp_fips)

baseline_data <- model_data %>% 
  mutate(fips = as.integer(as.numeric(levels(fips))[as.integer(fips)])) %>% 
  left_join(coef(no_pooling) %>% 
              rownames_to_column("fips") %>% 
              as_tibble() %>% 
              mutate(fips = as.integer(fips)) %>% 
              rename(intercept = `(Intercept)`, 
                     slope_county = county_cases_roll4, 
                     slope_national = national_cases_roll4, #)) %>% 
                     slope_policyox = oxz)) %>% 
  # extract standard error on each coefficient 
  left_join(coef(summary(no_pooling))[, "Std. Error", ] %>% 
              # [row (fips), column (estimate, error, t), coef (national, county, policy)]
              as.data.frame() %>% 
              rownames_to_column("fips") %>% 
              as_tibble() %>% 
              mutate(fips = as.integer(fips)) %>% 
              rename(intercept_se = `(Intercept)`, 
                     county_se = county_cases_roll4, 
                     national_se = national_cases_roll4, #)) %>% 
                     policyox_se = oxz)) %>% 
  cbind(residual = resid(no_pooling)) %>% 
  mutate(pred_lm = intercept + slope_county * county_cases_roll4 + 
           slope_national * national_cases_roll4 +
           slope_policyox * oxz,
         baseline = intercept + residual) %>% 
  ungroup()


#### scale baseline ####
mobility_scale <- read_csv("data/output/mobility_19_20_fall_ratio_new_norm.csv", col_types = "iddd")

mob_scaled_baseline_data <- baseline_data %>% 
  left_join(mobility_scale) %>% 
  mutate(scale_baseline = baseline * phi)

write_csv(mob_scaled_baseline_data, "data/output/baseline_contact_by_county_week.csv")

