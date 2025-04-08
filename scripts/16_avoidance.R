# some copied over from explore_g1 worry

{
  library(tidyverse)
  library(lubridate)
  library(vroom)
  library(tidylog)
}


df.fips <- read_csv('data/input/state_and_county_fips_master.csv')

# compare time series of avoid contact with contact rates
# will have to convert avoid contact to proportion
# first I want to see how behavior varies by avoidant group like I did for worry
# FYI I technically already have this raked from before
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


#### determine how to group avoidance levels ####
avoid_levels <- raked_data %>% 
  filter(outside_contacts <= 72) %>% 
  filter(! is.na(avoid_contact)) %>% 
  group_by(avoid_contact, week) %>% 
  summarise(mean_contact = weighted.mean(outside_contacts, updated_weight),
            samp_size = n())

avoid_levels %>%
  filter(week < ymd("2020-09-09")) %>% 
  ggplot(aes(x = week, y = mean_contact, col = avoid_contact, group = avoid_contact)) +
  geom_line() -> p1

avoid_levels %>%
  filter(week > ymd("2020-11-23")) %>% 
  ggplot(aes(x = week, y = mean_contact, col = avoid_contact, group = avoid_contact)) +
  geom_line() -> p2
library(ggpubr)
ggarrange(p1, p2, nrow = 1, common.legend = T, legend = "bottom")

# ok looks like we can group levels 1 and 2 together
# we can look at them over same time period too

# how does new avoid question compare to old in terms of proportion avoiding, 4 --> 5 pt likert scale

# run the first data read in stuff for this
# load datasets 5/2021 - 6/2022
path <- # TODO: ADD YOUR PATH TO RAW DATA [9:22]

cols <- cols_only(
  fips = col_integer(),
  StartDatetime = col_datetime(),  # Time survey begun
  weight = col_double(),
  C9 = col_factor(as.character(seq(1, 4, 1))), # How worried do you feel that you or someone in your immediate family might become seriously ill from COVID-19
  C10_1_1 = col_double(), # direct contact outside household at work
  C10_2_1 = col_double(), # direct contact outside household shopping
  C10_3_1 = col_double(), # direct contact outside household social gatherings
  C10_4_1 = col_double(), # direct contact outside household other
  C7 = col_factor(as.character(seq(1, 4, 1))), # To what extent are you intentionally avoiding contact with other people?
  C7a = col_factor(as.character(seq(1, 5, 1))), # In the past 7 days, how often did you intentionally avoid contact with other people?
  G1 = col_factor(as.character(seq(1, 4, 1))), # How much do you worry about catching COVID-19?
  D2 = col_factor(as.character(seq(1, 7, 1))), # What is your age (1-9)?
  D1 = col_factor(as.character(seq(1, 5, 1))), # gender
  D8 = col_factor(as.character(seq(1, 8, 1))), # education
  raceethnicity = col_factor(levels = c("Hispanic", "NonHispanicAmericanIndianAlaskaNative",
                                        "NonHispanicAsian", "NonHispanicBlackAfricanAmerican",
                                        "NonHispanicMultipleOther", "NonHispanicNativeHawaiianPacificIslander",
                                        "NonHispanicWhite")),
  module = col_factor(levels = c("A", "B")),
  wave = col_integer())

# load FB data
data <- # TODO: ADD PATH TO FOLDER WITH RAW DATA, path) %>%
  map(vroom, # maps function to a vector, vroom is faster version of read_csv
      delim = ',',
      col_types = cols) %>% # map passes back a list of each of the separate paths
  bind_rows() 

df.clean <- data %>%
  filter(!is.na(fips), fips < 60000) # 4%

wave10_avoid <- df.clean %>% 
  filter(wave == 10) %>% 
  filter(! is.na(C7)) %>% # old avoid
  rename(old_avoid_contact = C7) %>%
  mutate(date = date(StartDatetime),
         week = floor_date(date, unit = "week"),
         month = floor_date(date, unit = "month"))

wave11_avoid <- df.clean %>% 
  filter(wave == 11) %>% 
  #filter(module == "A") %>% 
  filter(! is.na(C7a)) %>% # new avoid
  rename(new_avoid_contact = C7a) %>%
  mutate(date = date(StartDatetime),
         week = floor_date(date, unit = "week"),
         month = floor_date(date, unit = "month")) 

# get county week avoid estimates for wave 10 vs 11
wave10_avoidprop <- wave10_avoid %>% 
  mutate(old_avoid_bin = ifelse(old_avoid_contact %in% c(1, 2), 1, 0),
         old_avoid_all = ifelse(old_avoid_contact == 1, 1, 0)) %>%
  group_by(week, fips) %>%
  summarise(old_prop_avoid = weighted.mean(old_avoid_bin, weight),
            old_prop_avoidall = weighted.mean(old_avoid_all, weight),
            old_samp_size = n()) %>%
  ungroup()

wave11_avoidprop <- wave11_avoid %>% 
  mutate(new_avoid_bin = ifelse(new_avoid_contact %in% c(1, 2), 1, 0),
         new_avoid_all = ifelse(new_avoid_contact == 1, 1, 0)) %>% 
  group_by(week, fips) %>%
  summarise(new_prop_avoid = weighted.mean(new_avoid_bin, weight),
            new_prop_avoidall = weighted.mean(new_avoid_all, weight),
            new_samp_size = n()) %>%
  ungroup()

compare_avoid <- wave10_avoidprop %>% 
  inner_join(wave11_avoidprop) %>% 
  filter(old_samp_size >= 10,
         new_samp_size >= 10) %>% 
  mutate(avoid_diff = old_prop_avoid - new_prop_avoid,
         avoidall_diff = old_prop_avoidall - new_prop_avoidall)

hist(compare_avoid$avoid_diff)
summary(compare_avoid$avoid_diff)
t.test(compare_avoid$avoid_diff)
compare_avoid %>% ggplot(aes(x = week, y = avoid_diff)) + 
  geom_line(aes(group = fips), alpha = 0.1) + 
  theme_bw() + 
  geom_smooth()

hist(compare_avoid$avoidall_diff)
summary(compare_avoid$avoidall_diff)
t.test(compare_avoid$avoidall_diff)
compare_avoid %>% ggplot(aes(x = week, y = avoidall_diff)) + 
  geom_line(aes(group = fips), alpha = 0.1) + 
  theme_bw() + 
  geom_smooth()

#### how do contact patterns fluctuate with avoiding contact proportion ####

# now i'd like to see how proportion avoiding contact varies with contact
# need to use the old data for this
avoid_prop <- raked_data %>% 
  filter(outside_contacts <= 72) %>% 
  filter(! is.na(avoid_contact)) %>% 
  mutate(avoid_bin = ifelse(avoid_contact %in% c(1, 2), 1, 0)) %>% 
  group_by(fips, week) %>% 
  summarise(mean_contact = weighted.mean(outside_contacts, updated_weight),
            prop_avoid = weighted.mean(avoid_bin, updated_weight),
            samp_size = n()) %>% 
  group_by(fips) %>% 
  filter(all(samp_size >= 10)) %>% # all must be true in group!
  mutate(z_contact = scale(mean_contact),
         z_propavoid = scale(prop_avoid)) %>% 
  ungroup() %>% 
  pivot_longer(cols = c(z_contact, z_propavoid), 
               names_to = "metric", values_to = "zscore") %>% 
  mutate(metric = ifelse(metric == "z_contact", "Mean contact", "Prop. avoiding contact"))

#sample_fips <- sample(unique(avoid_prop$fips), 9)
sample_fips <- c(4013, 17031, 48201, 53033, 6073, 26125, 26163, 4019, 12103)

(avoid_prop %>% 
    #filter(week > ymd("2020-11-23")) %>% 
    filter(fips %in% sample_fips) %>% 
    ggplot(aes(x = week, y = zscore, col = metric, group = metric)) + 
    geom_point(size = 1) + 
    geom_line() +
    facet_wrap(~fips) -> p4)

avoid_prop_roll <- raked_data %>% 
  filter(outside_contacts <= 72) %>% 
  filter(! is.na(avoid_contact)) %>% 
  mutate(avoid_bin = ifelse(avoid_contact %in% c(1, 2), 1, 0)) %>% 
  group_by(fips, week) %>% 
  summarise(mean_contact = weighted.mean(outside_contacts, updated_weight),
            prop_avoid = weighted.mean(avoid_bin, updated_weight),
            samp_size = n()) %>% 
  group_by(fips) %>% 
  filter(all(samp_size >= 10)) %>% # all must be true in group!
  mutate(roll_contact = zoo::rollmean(mean_contact, k = 3, fill = NA, align = "center"),
         roll_propavoid = zoo::rollmean(prop_avoid, k = 3, fill = NA, align = "center")) %>% 
  mutate(z_contact = scale(roll_contact),
         z_propavoid = scale(roll_propavoid)) %>% 
  ungroup() %>% 
  pivot_longer(cols = c(z_contact, z_propavoid), 
               names_to = "metric", values_to = "zscore") %>% 
  mutate(metric = ifelse(metric == "z_contact", "Mean contact", "Prop. avoiding contact")) %>% 
  mutate(grp = ifelse(week < ymd("2020-09-08"), 1, 2))

avoid_prop_roll %>% 
  filter(fips %in% sample_fips) %>% 
  ggplot(aes(x = week, y = zscore, col = metric)) + 
  geom_point(size = 1) + 
  geom_line(aes(group = interaction(metric, grp))) +
  facet_wrap(~fips) +
  theme_bw()

ggsave("figures/supp/avoid-vs-contact.pdf", height = 5, width = 8)

spatiotemporal_fits <- read_csv("data/output/normal_gamma2_72trunc_m1/fitted_predictions.csv",
                                col_types = "ddiiddddddddiiDccDccf") %>% ungroup()

avoid_prop_roll_gam <- raked_data %>% 
  filter(outside_contacts <= 72) %>% 
  filter(! is.na(avoid_contact)) %>% 
  mutate(avoid_bin = ifelse(avoid_contact %in% c(1, 2), 1, 0)) %>% 
  group_by(fips, week) %>% 
  summarise(prop_avoid = weighted.mean(avoid_bin, updated_weight),
            samp_size = n()) %>% 
  group_by(fips) %>% 
  filter(all(samp_size >= 10)) %>% # all must be true in group!
  mutate(roll_propavoid = zoo::rollmean(prop_avoid, k = 3, fill = NA, align = "center")) %>% 
  left_join(spatiotemporal_fits %>% select(fips, week, fit)) %>% 
  mutate(z_contact = scale(fit),
         z_propavoid = scale(roll_propavoid)) %>% 
  ungroup() %>% 
  pivot_longer(cols = c(z_contact, z_propavoid), 
               names_to = "metric", values_to = "zscore") %>% 
  mutate(metric = ifelse(metric == "z_contact", "Mean contact", "Prop. avoiding contact")) %>% 
  mutate(grp = ifelse(week < ymd("2020-09-08"), 1, 2))

sample_fips <- c(4003, 6081, 8101, 10003, 12061, 24047, 34003, 35049, 40051,
                 45063, 51047, 53035)
avoid_prop_roll_gam %>% 
  left_join(df.fips) %>% 
  mutate(name_short = gsub(" County", "", name),
         county_name = paste0(name_short, ", ", state)) %>% 
  filter(fips %in% sample_fips) %>% 
  filter(fips != 27013) %>% 
  ggplot(aes(x = week, y = zscore, col = metric)) + 
  geom_point(size = 1) + 
  geom_line(aes(group = interaction(metric, grp))) +
  facet_wrap(~county_name) +
  scale_x_date(breaks = seq(as.Date("2020-07-01"), as.Date("2021-04-30"),
                            by = "3 month"),
               labels = c("Jul 20", "Oct", "Jan 21", "Apr"),
               minor_breaks = "1 month") +
  theme_bw() -> pa
ggsave("figures/supp/avoid-vs-contact-gam.pdf", height = 5, width = 8)


#### prep post contact period avoidance data ####

new_avoid <- df.clean %>% 
  filter(! is.na(C7a)) %>% 
  mutate(avoid_bin = ifelse(C7a %in% c(1, 2), 1, 0)) %>% 
  mutate(date = date(StartDatetime),
         week = floor_date(date, unit = "week"),
         month = floor_date(date, unit = "month")) %>% 
  group_by(fips, week) %>% 
  summarise(prop_avoid = weighted.mean(avoid_bin, weight),
            samp_size = n()) %>% ungroup()

write_csv(new_avoid, "data/output/raw_new_avoid_2021-22.csv")

new_avoid %>% 
  filter(samp_size >= 20) %>%  
  ggplot(aes(x = week, y = prop_avoid)) +
  geom_line(aes(group = fips, col = fips), alpha = 0.05) +
  labs(caption = "New C7a avoid question,
       prop. avoid weighted by provided weights (i.e. I didn't rake),
       filtered to weekly sample size >= 20") +
  theme_bw()

#### extend predictions ####
# we want to predict contact using the same coefficients as from our model
# then see how these compare to avoid contact data at that time

new_avoid <- read_csv("data/output/raw_new_avoid_2021-22.csv") %>% 
  group_by(fips) %>% 
  mutate(propavoid_roll3 = zoo::rollmean(prop_avoid, k = 3, fill = NA, align = "center")) %>% 
  ungroup()

# load coefs
# this is for model: 1 + national_cases_roll4 * percent_vaxxed + StringencyIndex_Average_roll3
# statepol_coefs <- read_csv("data/output/coefs_for_prediction.csv")
natonly_coefs <- read_csv("data/output/coefs_for_prediction_natonly.csv")

#load("data/output/statepol_model_for_prediction.RData")

library(readxl)
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

# documentation here: https://www2.census.gov/programs-surveys/popest/technical-documentation/file-layouts/2020-2021/CO-EST2021-ALLDATA.pdf
recent_pop_data <- read_csv("data/input/co-est2021-alldata.csv") %>% # 2022 has CT updates we don't want
  dplyr::select(STATE, COUNTY, STNAME, CTYNAME, POPESTIMATE2020) %>% 
  filter(COUNTY != "000") %>% 
  mutate(fips = as.integer(paste0(STATE, COUNTY)),
         STATE = as.integer(as.numeric(STATE))) 

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
# could have used predict with the model, but now that I've extracted these that feels hard

new_covars <- new_avoid %>% 
  left_join(df.fips, by = "fips") %>% 
  left_join(nyt_national_roll, by = "week") #%>% 
  #left_join(oxdata_week, by = c("state", "week")) %>%
  #left_join(vax_data, by = c("fips", "week")) %>% 
  #filter(! is.na(percent_vaxxed)) # missing for 17 fips

# not working to use predict so use coefficients
prediction_df <- new_covars %>% 
  # joining this one is trickier since it doesn't have fips --> went back and added fips!
  #left_join(statepol_coefs, by = "fips") %>% 
  left_join(natonly_coefs, by = "fips") %>% 
  rowwise() %>% 
  mutate(contact_predicted = intercept + national_cases_roll4 * national_cases_roll4_slope)


prediction_df_cor <- prediction_df %>% 
  filter(! is.na(contact_predicted)) %>% # removes NAs across columns
  group_by(fips) %>% 
  mutate(across(c(contact_predicted, propavoid_roll3),
                ~scale(.x)[,1],
                .names = "z_{.col}")) %>% 
  pivot_longer(cols = c(z_contact_predicted:z_propavoid_roll3), 
               names_to = "metric", values_to = "zscore") %>% 
  separate(metric, into = c(NA, "metric", NA))


sample_fips <- c(4013, 17031, 48201, 53033, 6073, 26125, 26163, 4019, 12103)
non6_fips <- prediction_df_cor %>% left_join(urb_rur_codes) %>% filter(ur_code != 6, ur_code != 5) %>% pull(fips)
sample_fips <- sample(unique(non6_fips), 16)

prediction_df_cor %>% 
  left_join(urb_rur_codes) %>% 
  mutate(name_short = gsub(" County", "", name),
         county_name = paste0(name_short, ", ", state)) %>% 
  filter(fips %in% sample_fips) %>% 
  mutate(metric = ifelse(metric == "contact", "Mean contact", "Prop. avoiding contact")) %>% 
  ggplot(aes(x = week, y = zscore, group = interaction(fips, metric), col = metric)) +
  geom_line() + 
  facet_wrap(~county_name) +
  scale_x_date(breaks = seq(as.Date("2021-05-01"), as.Date("2022-06-19"),
                            by = "3 month"),
               labels = c("May 21", "Aug", "Nov", "Feb 22", "May"),
               minor_breaks = "1 month") +
  geom_text(aes(x = ymd("2021-07-01"), label = ur_code), y = -3, col = "black", check_overlap = T) +
  theme_bw() -> pb
library(ggpubr)
ggarrange(pa, pb, nrow = 1, common.legend = T, labels = "AUTO", widths = c(3, 4))
ggsave("figures/supp/avoid-combined.pdf", height = 5, width = 14)

# get correlation for all counties
df_cor <- prediction_df_cor %>% 
  filter(! is.na(propavoid_roll3), ! is.na(contact_predicted)) %>% 
  group_by(fips) %>% 
  summarise(corr = cor(contact_predicted, propavoid_roll3))

library(choroplethr)
library(MetBrewer)
library(ggpubr)
mid <- 0
max <- 1
min <- -1
grad_mid <- (mid - min)/(max - min) # midpoint will be 0 or 1 depending

cor_map <- CountyChoropleth$new(df_cor %>% 
                                  rename(region = fips, value = corr))
cor_map$ggplot_polygon <- geom_polygon(aes(fill = value), color = NA, linewidth = 0.01)
cor_map$set_num_colors(0)
cor_map$ggplot_scale <- scale_fill_gradientn(colors = met.brewer("Hiroshige"), 
                                             limits = c(-1, 1),
                                             values = c(0, grad_mid, 1),
                                             name = "Correlation\ncoefficient", 
                                             guide = guide_colourbar(direction = "vertical", title.position = "left"))
cor_map$render() + theme(legend.position = c(0.9, 0.35),
                         plot.margin = unit(c(0,0,0,0), 'lines')) 
ggsave("figures/supp/avoid-cor-predicted.pdf", height = 5, width = 8)
