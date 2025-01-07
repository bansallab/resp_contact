# for general contact analysis, just need weekly contact means
# but for group-specific gams, we need means by group by week

{
  library(tidyverse)
  library(lubridate)
  library(tidylog)
  library(spatstat)
}


raked_data <- read_csv("data/raking/contact_raking_weights_age_sex_corrected.csv", 
                       col_types = cols_only(date = col_date(),
                                             week = col_date(),
                                             month = col_date(),
                                             avoid_contact = readr::col_factor(as.character(seq(1, 4, 1))),
                                             worry = readr::col_factor(as.character(seq(1, 4, 1))),
                                             contacts_work = col_double(),
                                             contacts_shop = col_double(),
                                             contacts_hh_gathering = col_double(),
                                             contacts_other = col_double(),
                                             gender = readr::col_factor(as.character(seq(1, 2, 1))), # now just 2
                                             age = readr::col_factor(as.character(seq(1, 7, 1))),
                                             fips = col_integer(),
                                             given_weight = col_double(),
                                             positive_test = readr::col_factor(as.character(seq(1, 3, 1))),
                                             positive_test_14d = readr::col_factor(as.character(seq(1, 3, 1))), 
                                             education = readr::col_factor(as.character(seq(1, 7, 1))),
                                             response_id = col_integer(),
                                             num_hh = col_double(),
                                             num_hh_contact = col_integer(),
                                             outside_contacts = col_integer(),
                                             caseid = col_integer(),
                                             weight = col_double(),
                                             updated_weight = col_double())) %>% 
  filter(!is.na(updated_weight)) %>% # remove observations from county-weeks with < 3 responses, <1% of responses
  filter(week < ymd("2021-05-01") & week > ymd("2020-05-31")) 

raked_data <- raked_data %>% 
  filter(outside_contacts <= 72) %>% # TODO: vary this cutoff for truncation sensitivity analyses
  mutate(age4 = as.factor(case_when(age == 1 ~ 1,
                                    age == 2 ~ 1,
                                    age == 3 ~ 1,
                                    age == 4 ~ 1,
                                    age == 5 ~ 2,
                                    age == 6 ~ 3,
                                    age == 7 ~ 4)))
# nonconvergent responses are included
# 13,372,301 responses

# # distribution of raking weights
# raked_data %>% ggplot(aes(x = updated_weight)) +
#   geom_histogram(col = NA)

#### interquartile mean ####
# iqr_obs <- raked_data %>% 
#   group_by(fips, week) %>% 
#   mutate(perc_25 = quantile(outside_contacts, prob = 0.25),
#          perc_75 = quantile(outside_contacts, prob = 0.75),
#          total_samp_size = n()) %>% 
#   ungroup() %>% 
#   filter(outside_contacts >= perc_25, outside_contacts <= perc_75) # removed 35% of observations
# 
# iqr_means <- iqr_obs %>% 
#   group_by(fips, week, total_samp_size) %>% 
#   summarise(non_hh_contacts = weighted.mean(outside_contacts, updated_weight),
#             samp_size = n(),
#             perc_25 = mean(perc_25),
#             perc_75 = mean(perc_75))

# remember to use UPDATED_WEIGHT which is filled in for non-convergence

# this is in place of resampling
#### COUNTY-WEEK MEANS ####
# also extract by context

county_week <- raked_data %>% 
  group_by(fips, week) %>% 
  summarise(hh_contacts = weighted.mean(num_hh_contact, updated_weight),
            non_hh_contacts = weighted.mean(outside_contacts, updated_weight),
            hh_contacts_med = weighted.median(num_hh_contact, updated_weight),
            non_hh_contacts_med = weighted.median(outside_contacts, updated_weight),
            # need na.rm = T for these context contacts because didn't require them in raking
            work_contacts = weighted.mean(contacts_work, updated_weight, na.rm = T),
            shop_contacts = weighted.mean(contacts_shop, updated_weight, na.rm = T),
            social_contacts = weighted.mean(contacts_hh_gathering, updated_weight, na.rm = T),
            other_contacts = weighted.mean(contacts_other, updated_weight, na.rm = T),
            samp_size = n())

write_csv(county_week, "data/group_means_rake/contact_by_county_week_trunc72.csv")

# calculate unweighted contact rates for comparison
county_week_unwtd <- raked_data %>% 
  group_by(fips, week) %>% 
  summarise(non_hh_contacts = mean(outside_contacts),
            samp_size = n())
write_csv(county_week_unwtd, "data/group_means_rake/contact_by_county_week_trunc72_unwtd.csv")


#### AGE ####

### county means by age group ###
county_by_age <- raked_data %>% 
  group_by(fips, age) %>% 
  summarize(hh_contacts = weighted.mean(num_hh_contact, updated_weight),
            non_hh_contacts = weighted.mean(outside_contacts, updated_weight),
            samp_size = n())

county_by_age %>%
  mutate(age_fct = case_when(age == 1 ~ "18-24",
                             age == 2 ~ "25-34",
                             age == 3 ~ "35-44",
                             age == 4 ~ "45-54",
                             age == 5 ~ "55-64",
                             age == 6 ~"65-74",
                             age == 7 ~ "75+")) %>% 
  ggplot(aes(x = age_fct, y = non_hh_contacts)) + 
  geom_jitter(alpha = 0.1) + 
  geom_boxplot(fill = NA, outlier.shape = NA, col = "pink") + 
  theme_bw() +
  labs(x = "Age", y = "Non-household contacts")
ggsave("figures/supp/age-aggregation.pdf", height = 4, width = 6)

write_csv(county_by_age, "data/group_means_rake/contact_by_age_county_trunc72.csv")

### weekly means by age group ###
week_by_age <- raked_data %>% 
  #mutate(outside_contacts = ifelse(outside_contacts > 29, 29, outside_contacts)) %>% 
  group_by(week, age) %>% 
  summarize(hh_contacts = weighted.mean(num_hh_contact, updated_weight),
            non_hh_contacts = weighted.mean(outside_contacts, updated_weight),
            samp_size = n())

write_csv(week_by_age, "data/group_means_rake/contact_by_age_week_trunc72.csv")

### county week by age so weights used correctly ###
county_week_by_age <- raked_data %>% 
  #mutate(outside_contacts = ifelse(outside_contacts > 29, 29, outside_contacts)) %>% 
  group_by(week, fips, age) %>% 
  summarize(hh_contacts = weighted.mean(num_hh_contact, updated_weight),
            non_hh_contacts = weighted.mean(outside_contacts, updated_weight),
            samp_size = n())
write_csv(county_week_by_age, "data/group_means_rake/contact_by_age_week_county_trunc72.csv")

county_week_by_age4 <- raked_data %>% 
  group_by(week, fips, age4) %>% 
  summarize(hh_contacts = weighted.mean(num_hh_contact, updated_weight),
            non_hh_contacts = weighted.mean(outside_contacts, updated_weight),
            samp_size = n())
write_csv(county_week_by_age4, "data/group_means_rake/contact_by_age4_week_county_trunc72.csv")

#### WORRY ####

### county means by worry group ###
county_by_worry <- raked_data %>% 
  filter(! is.na(worry)) %>% 
  group_by(fips, worry) %>% 
  summarize(hh_contacts = weighted.mean(num_hh_contact, updated_weight),
            non_hh_contacts = weighted.mean(outside_contacts, updated_weight),
            samp_size = n())

write_csv(county_by_worry, "data/group_means_rake/contact_by_worry_county_trunc72.csv")

### weekly means by worry group ###
week_by_worry <- raked_data %>% 
  filter(! is.na(worry)) %>% 
  group_by(week, worry) %>% 
  summarize(hh_contacts = weighted.mean(num_hh_contact, updated_weight),
            non_hh_contacts = weighted.mean(outside_contacts, updated_weight),
            samp_size = n())

write_csv(week_by_worry, "data/group_means_rake/contact_by_worry_week_trunc72.csv")

county_week_by_worry <- raked_data %>% 
  filter(! is.na(worry)) %>% 
  group_by(week, fips, worry) %>% 
  summarize(hh_contacts = weighted.mean(num_hh_contact, updated_weight),
            non_hh_contacts = weighted.mean(outside_contacts, updated_weight),
            samp_size = n())
write_csv(county_week_by_worry, "data/group_means_rake/contact_by_worry_week_county_trunc72.csv")

county_week_by_worry <- raked_data %>% 
  filter(! is.na(worry)) %>% 
  mutate(worry_bin = ifelse(worry %in% c(1, 2), 1, 0)) %>% 
  group_by(week, fips, worry_bin) %>% 
  summarize(hh_contacts = weighted.mean(num_hh_contact, updated_weight),
            non_hh_contacts = weighted.mean(outside_contacts, updated_weight),
            samp_size = n())
write_csv(county_week_by_worry, "data/group_means_rake/contact_by_worry_bin_week_county_trunc72.csv")

#### GENDER ####

### county means by gender group ###
county_by_gender <- raked_data %>% 
  group_by(fips, gender) %>% 
  summarize(hh_contacts = weighted.mean(num_hh_contact, updated_weight),
            non_hh_contacts = weighted.mean(outside_contacts, updated_weight),
            samp_size = n())

write_csv(county_by_gender, "data/group_means_rake/contact_by_gender_county_trunc72.csv")

### weekly means by gender group ###
week_by_gender <- raked_data %>% 
  mutate(outside_contacts = ifelse(outside_contacts > 29, 29, outside_contacts)) %>% 
  group_by(week, gender) %>% 
  summarize(hh_contacts = weighted.mean(num_hh_contact, updated_weight, na.rm = T),
            non_hh_contacts = weighted.mean(outside_contacts, updated_weight, na.rm = T),
            samp_size = n())

write_csv(week_by_gender, "data/group_means_rake/contact_by_gender_week_trunc72.csv")

## county week gender ##
county_week_by_gender <- raked_data %>% 
  group_by(week, gender, fips) %>% 
  summarize(hh_contacts = weighted.mean(num_hh_contact, updated_weight, na.rm = T),
            non_hh_contacts = weighted.mean(outside_contacts, updated_weight, na.rm = T),
            samp_size = n())
write_csv(county_week_by_gender, "data/group_means_rake/contact_by_gender_week_county_trunc72.csv")


#### SETTING ####
## county week setting ##
county_week_by_setting <- raked_data %>% # pivoting so that same format as age/gender dataframes
  dplyr::select(week, fips, contacts_work, contacts_shop, contacts_hh_gathering, contacts_other, updated_weight) %>% 
  pivot_longer(names_to = "setting", values_to = "contacts", 
               cols = c(contacts_work, contacts_shop, contacts_hh_gathering, contacts_other)) %>% 
  group_by(week, fips, setting) %>% 
  summarize(non_hh_contacts = weighted.mean(contacts, updated_weight, na.rm = T),
            samp_size = n()) %>% 
  mutate(setting = case_when(setting == "contacts_work" ~ "work",
                             setting == "contacts_hh_gathering" ~ "social",
                             setting == "contacts_shop" ~ "shopping",
                             setting == "contacts_other" ~ "other"))
write_csv(county_week_by_setting, "data/group_means_rake/contact_by_setting_week_county_trunc72.csv")

# ---------- #
#### RACE ####
# ---------- #
raked_data_w_race <- read_csv("data/raking/contact_raking_weights_age_sex_race5_statemonth_corrected.csv",
                              col_types = cols_only(week = col_date(),
                                                    month = col_date(),
                                                    gender = readr::col_factor(as.character(seq(1, 2, 1))), # now just 2
                                                    age = readr::col_factor(as.character(seq(1, 7, 1))),
                                                    fips = col_integer(),
                                                    num_hh = col_double(),
                                                    num_hh_contact = col_integer(),
                                                    outside_contacts = col_integer(),
                                                    weight = col_double(),
                                                    raceethnicity = readr::col_factor(),
                                                    race_cat_col = readr::col_factor(),
                                                    state = col_character(),
                                                    state_fips = col_integer())) %>% 
  filter(!is.na(weight)) %>% 
  filter(week < ymd("2021-05-01") & week > ymd("2020-05-31")) %>% 
  filter(outside_contacts <= 72)

### county means by race group ###
county_by_race <- raked_data_w_race %>% 
  group_by(fips, race_cat_col) %>% 
  summarize(hh_contacts = weighted.mean(num_hh_contact, weight),
            non_hh_contacts = weighted.mean(outside_contacts, weight),
            samp_size = n())

write_csv(county_by_race, "data/group_means_rake/contact_by_race_county_trunc72.csv")

### weekly means by race group ###
week_by_race <- raked_data_w_race %>% 
  group_by(week, race_cat_col) %>% 
  summarize(hh_contacts = weighted.mean(num_hh_contact, weight, na.rm = T),
            non_hh_contacts = weighted.mean(outside_contacts, weight, na.rm = T),
            samp_size = n())

write_csv(week_by_race, "data/group_means_rake/contact_by_race_week_trunc72.csv")

## county week race ##
county_week_by_race <- raked_data_w_race %>% 
  group_by(week, race_cat_col, fips) %>% 
  summarize(hh_contacts = weighted.mean(num_hh_contact, weight, na.rm = T),
            non_hh_contacts = weighted.mean(outside_contacts, weight, na.rm = T),
            samp_size = n())
write_csv(county_week_by_race, "data/group_means_rake/contact_by_race_week_county_trunc72.csv")

## state week race ##
state_week_by_race <- raked_data_w_race %>% 
  group_by(week, race_cat_col, state) %>% 
  summarize(hh_contacts = weighted.mean(num_hh_contact, weight, na.rm = T),
            non_hh_contacts = weighted.mean(outside_contacts, weight, na.rm = T),
            samp_size = n())
write_csv(state_week_by_race, "data/group_means_rake/contact_by_race_week_state_trunc72.csv")
