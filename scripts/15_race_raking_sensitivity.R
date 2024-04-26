# test sensitivity of results to exclusion of race from raking algorithm

{
  library(tidyverse)
  library(lubridate)
  library(tidylog)
  library(MetBrewer)
}

df.fips <- read_csv('data/input/state_and_county_fips_master.csv',
                    col_types = "icc") %>% 
  mutate(fips = ifelse(fips == 46113, 46102, 
                       ifelse(fips == 2270, 2158, fips)))

raked_data_wo_race <- read_csv("data/raking/contact_raking_weights_age_sex_corrected.csv", 
                               col_types = cols_only(week = col_date(),
                                                     month = col_date(),
                                                     gender = readr::col_factor(as.character(seq(1, 2, 1))), # now just 2
                                                     age = readr::col_factor(as.character(seq(1, 7, 1))),
                                                     fips = col_integer(),
                                                     num_hh = col_double(),
                                                     num_hh_contact = col_integer(),
                                                     outside_contacts = col_integer(),
                                                     updated_weight = col_double())) %>% 
  filter(!is.na(updated_weight)) %>% # remove observations from county-weeks with < 3 responses, <1% of responses
  filter(week > ymd("2020-09-05") & month < ymd("2021-05-01")) %>% 
  left_join(df.fips)

raked_data_wo_race_state <- read_csv("data/raking/contact_raking_weights_age_sex_statemonth_corrected.csv", 
                                     col_types = cols_only(week = col_date(),
                                                           month = col_date(),
                                                           gender = readr::col_factor(as.character(seq(1, 2, 1))), # now just 2
                                                           age = readr::col_factor(as.character(seq(1, 7, 1))),
                                                           fips = col_integer(),
                                                           state = col_character(),
                                                           state_fips = col_integer(),
                                                           num_hh = col_double(),
                                                           num_hh_contact = col_integer(),
                                                           outside_contacts = col_integer(),
                                                           weight = col_double())) %>% 
  filter(!is.na(weight)) %>% # remove observations from county-weeks with < 3 responses, <1% of responses
  filter(week > ymd("2020-09-05") & month < ymd("2021-05-01")) 

# need only this one for figure 3
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
  filter(month < ymd("2021-05-01")) 

norace_means <- raked_data_wo_race %>% 
  group_by(state, month) %>% 
  summarise(mean_contact_age_sex = weighted.mean(outside_contacts, updated_weight),
            samp_size_age_sex = n())

norace_means_state <- raked_data_wo_race_state %>% 
  group_by(state, month) %>% 
  summarise(mean_contact_age_sex_st = weighted.mean(outside_contacts, weight),
            samp_size_age_sex_st = n())

race_means <- raked_data_w_race %>% 
  group_by(state, month) %>% 
  summarise(mean_contact_age_sex_race = weighted.mean(outside_contacts, weight),
            samp_size_age_sex_race = n())  

hhs_regions <- read_csv("data/input/HHS_regions.csv", col_types = "if") %>% 
  rename(fips = node, hhs_region = modularity_class) %>% 
  left_join(df.fips) %>% 
  select(state, hhs_region) %>% 
  distinct()

all_data <- norace_means %>% 
  left_join(norace_means_state) %>% 
  left_join(race_means) %>% 
  left_join(hhs_regions) %>% 
  mutate(diff = mean_contact_age_sex - mean_contact_age_sex_race,
         ratio = mean_contact_age_sex/mean_contact_age_sex_race) %>% 
  filter(month != ymd("2021-05-01"))

all_data$hhs_region <- factor(all_data$hhs_region, 
                              levels = c("1", "2", "3", "4", "5", "6", "7", "8", "9", "10"))

all_data %>% ggplot(aes(x = mean_contact_age_sex, y = mean_contact_age_sex_st)) +
  geom_point(aes(col = hhs_region), alpha = 0.75) + 
  geom_abline(alpha = 0.5) +
  scale_color_manual(values = met.brewer("Egypt", 10))

all_data %>% ggplot(aes(x = mean_contact_age_sex, y = mean_contact_age_sex_race)) +
  geom_point(alpha = 0.5) + 
  geom_abline() +
  scale_color_manual(values = met.brewer("Egypt", 10)) +
  labs(x = "Mean contact\ncounty-level raking on age, sex",
       y = "Mean contact\nstate-level raking on age, sex, race") +
  theme_bw()
ggsave("figures/supp/race-raking-sensitivity.pdf", height = 5, width = 8)

all_data %>% ggplot(aes(x = samp_size_age_sex, y = diff)) +
  geom_point(aes(col = hhs_region)) + 
  scale_x_log10() +
  geom_hline(yintercept = 0) +
  scale_color_manual(values = met.brewer("Egypt", 10))

# now let's map the differences
library(choroplethr)
library(choroplethrMaps)
data(state.regions)

all_data <- all_data %>% left_join(state.regions, by = c("state" = "abb")) 

months <- unique(all_data$month)
for(i in 1:length(months)){
  map <- StateChoropleth$new(all_data %>% 
                               rename(value = diff) %>% 
                               filter(month == months[[i]]))
  map$ggplot_polygon <- geom_polygon(aes(fill = value), color = NA, size = 0.01)
  map$set_num_colors(1)
  map$ggplot_scale <-scale_fill_gradientn(colors = met.brewer("Hiroshige"), 
                                          limits = c(-2, 2),
                                          name = "Difference\nof means")
  map$show_labels = FALSE # remove state labels
  map <- map$render()
  map + ggtitle(paste0("Month of ", months[[i]])) +
    theme(plot.title = element_text(hjust = 0.5)) +
    labs(caption = "age-sex mean - age-sex-race mean")
  
  map2 <- StateChoropleth$new(all_data %>% 
                                rename(value = ratio) %>% 
                                filter(month == months[[i]]))
  map2$ggplot_polygon <- geom_polygon(aes(fill = value), color = NA, size = 0.01)
  map2$set_num_colors(1)
  map2$ggplot_scale <-scale_fill_gradientn(colors = met.brewer("Hiroshige"), 
                                           limits = c(0.8, 1.25),
                                           name = "Ratio\nof means")
  map2$show_labels = FALSE # remove state labels
  map2 <- map2$render()
  map2 + ggtitle(paste0("Month of ", months[[i]])) +
    theme(plot.title = element_text(hjust = 0.5)) +
    labs(caption = "age-sex mean / age-sex-race mean")
}
