{
  library(tidyverse)
  library(MetBrewer)
  library(tidylog)
  library(lubridate)
  library(vroom)
  library(choroplethr)
  library(choroplethrMaps)
  library(viridis)
}

#### load my estimates ####
age_estimates <- read_csv("data/output/baseline_contact_by_county_week_by_age.csv",
                          col_types = "diDDidfidiiididiccidddddddddddddddddddddddddddiddddddddddddddddddd") 

# to get pandemic estimates past feb 2021 need to grab directly from spatiotemporal data 
pandemic_age <- data.frame()
for(age_level in c(1, 2, 3, 4)){
  folder_name <- "age4_72trunc_m1"
  folder_sublevel = paste0("age", age_level)
  spatiotemporal_fits <- read_csv(paste0("spatiotemporal/", folder_name, "/fitted_predictions_", folder_sublevel, ".csv"),
                                  col_types = "ddiifddiiDccDccfd") %>% ungroup() %>%
    mutate(age = age_level)
  pandemic_age <- pandemic_age %>% bind_rows(spatiotemporal_fits)
}


gender_estimates <- read_csv("data/output/baseline_contact_by_county_week_by_gender.csv",
                             col_types = "diDDidfidiiididiccidddddddddddddddddddddddddddiddddddddddddddddddd")

pandemic_gender <- data.frame()
for(gender_level in c(1, 2)){
  folder_name <- "gender_72trunc_m1"
  folder_sublevel = paste0("gender", gender_level)
  spatiotemporal_fits <- read_csv(paste0("spatiotemporal/", folder_name, "/fitted_predictions_", folder_sublevel, ".csv"),
                                  col_types = "ddiifddiiDccDccfd") %>% ungroup() %>%
    mutate(gender = gender_level)
  pandemic_gender <- pandemic_gender %>% bind_rows(spatiotemporal_fits)
}

race_estimates <- read_csv("data/output/baseline_contact_by_state_week_by_race.csv",
                           col_types = "dcDDidfiiididddddddddddddddddddddddddcicd")

pandemic_race <- data.frame()
for(race_level in c("asian", "black", "hispanic", "other", "white")){
  folder_name <- "ethrace_72trunc_region4_m1"
  folder_sublevel <- race_level
  spatiotemporal_fits <- read_csv(paste0("spatiotemporal/", folder_name, "/fitted_predictions_", folder_sublevel, ".csv"),
                                  col_types = "ddciDfddiicfcD") %>%
    mutate(race_cat_col = race_level) # impute for imputed estimates
  pandemic_race <- pandemic_race %>% bind_rows(spatiotemporal_fits)
}

spatiotemporal_fits <- read_csv("spatiotemporal/normal_gamma2_72trunc_m1/fitted_predictions.csv",
                                col_types = "ddiiddddddddiiDccDccf") %>% ungroup()

spatiotemporal_fits$ur_code <- factor(spatiotemporal_fits$ur_code, levels = c("1", "2", "3", "4", "5", "6"))

#### POLYMOD COMPARISON ####

polymod_full_data <- read_csv("data/input/POLYMOD_DATA/contacts_final_v2_with_weights.txt",
                              col_types = "idfiiiiiifiiiiiiiii") 

polymod_clean_data <- polymod_full_data %>% 
  select(participant_age, cnt_count, country, diary_weight, global_id) %>% 
  distinct()  # 7254 rows now which is a bit low (should be 7290), do this to adjust for multiple rows per participant

polymod_data_ageadj <- polymod_clean_data %>% 
  mutate(age_fct = as.factor(case_when(participant_age %in% c(seq(18, 54, by = 1)) ~ "18-54",
                                       participant_age %in% c(seq(55, 64, by = 1)) ~ "55-64",
                                       participant_age %in% c(seq(65, 74, by = 1)) ~ "65-74",
                                       participant_age %in% c(seq(75, 85, by = 1)) ~ "75+",
                                       participant_age %in% c(seq(0, 17, by = 1)) ~ "under18",
                                       TRUE ~ "undefined"))) %>% 
  filter(age_fct != "under18",
         age_fct != "undefined") %>% 
  mutate(cnt_count_cens = ifelse(cnt_count > 29, 29, cnt_count),
         cnt_count_trunc = ifelse(cnt_count > 72, NA, cnt_count),
         rel_contact = cnt_count/mean(cnt_count))

library(boot)
N <- 1000
ages <- sort(unique(polymod_data_ageadj$age_fct))
age_only_polymod <- data.frame()
for(a in 1:length(ages)){
  doi <- polymod_data_ageadj %>% 
    filter(age_fct == ages[[a]]) %>% 
    filter(! is.na(cnt_count_trunc))
  for(i in 1:N){
    # this is not correct way to do diary weights since they are by country
    boot_samp <- mean(sample(doi$cnt_count_trunc, size = nrow(doi), replace = TRUE)) # can do rel_contact
    age_only_polymod <- age_only_polymod %>% rbind(data.frame(age_fct = ages[[a]], boot_mean = boot_samp))
  }
}

ctis <- age_estimates %>% select(fips, week, age, scale_baseline) %>% 
  group_by(fips, age) %>% 
  summarise(mean_contact = mean(scale_baseline)) %>% 
  mutate(age_fct = case_when(age == 1 ~ "18-54",
                             age == 2 ~ "55-64",
                             age == 3 ~"65-74",
                             age == 4 ~ "75+"))

together <- age_only_polymod %>% 
  mutate(survey = "polymod") %>%
  rename(mean_contact = boot_mean) %>% 
  bind_rows(ctis %>% mutate(survey = "ctis"))

together %>%   
  ggplot(aes(x = age_fct, y = mean_contact, fill = survey)) + 
  geom_violin(alpha = 0.75, col = NA, position = "identity") + 
  scale_fill_met_d(name = "Isfahan2", direction = -1) +
  labs(x = "Age", y = "Mean contact") +
  theme_bw()

ggsave("figures/supp/validation_polymod_age.pdf", height = 4, width = 6)



#### DeStefano comparison (age and gender) ####
destefano_age <- data.frame(compat_age_fct = c("18-24", "25-34", "35-44", "45-54", "55-64", "65+"),
                            mean_speaking_contact = c(14.0, 13.9, 16.0, 12.8, 9.4, 6.1),
                            sd_speaking_contact = c(43.5, 24.1, 28.8, 17.1, 10.2, 6.1),
                            mean_close_contact = c(15.6, 14.4, 17.1, 12.4, 9.5, 7.2),
                            sd_close_contact = c(54.1, 30.7, 34.7, 17.4, 11.5, 8.2)) %>% 
  mutate(max_speaking_contact = mean_speaking_contact + sd_speaking_contact,
         min_speaking_contact = max(0, mean_speaking_contact - sd_speaking_contact),
         max_close_contact = mean_close_contact + sd_close_contact,
         min_close_contact = max(0, mean_close_contact - sd_close_contact))

ctis_destefano_age <- age_estimates %>% 
  filter(state_fips == 37) %>% # North Carolina
  # whole time period is fine to match since we are looking at baseline
  select(fips, week, age, scale_baseline) %>% 
  mutate(age_fct = case_when(age == 1 ~ "18-54",
                             age == 2 ~ "55-64",
                             age == 3 ~ "65-74",
                             age == 4 ~ "75+"),
         compat_age_fct = ifelse(age_fct == "75+", "65+",
                                 ifelse(age_fct == "65-74", "65+",
                                        age_fct))) %>% 
  group_by(fips, compat_age_fct) %>% 
  summarise(mean_contact = mean(scale_baseline))
# only 7 counties with sufficient sample size i guess
ctis_destefano_age %>% ggplot(aes(x = compat_age_fct)) +
  geom_violin(aes(y = mean_contact), fill = "dodgerblue", alpha = 0.75, col = NA) +
  geom_pointrange(data = destefano_age, 
                  aes(y = mean_close_contact, 
                      ymax = max_close_contact,
                      ymin = min_close_contact),
                  col = "firebrick" , lwd = 1.5, size = 1) +
  geom_pointrange(data = destefano_age, 
                  aes(y = mean_speaking_contact, 
                      ymax = max_speaking_contact,
                      ymin = min_speaking_contact),
                  col = "orange", alpha = 0.65, lwd = 1.5, size = 1) +
  labs(y = "Mean contact", x = "Age group") +
  theme_bw() #+
#ylim(0, 50)

ggsave("figures/supp/validation_destefano_age.pdf", height = 4, width = 6)


destefano_gender <- data.frame(gender_cat = c("men", "women"),
                               mean_speaking_contact = c(11.7, 12.5),
                               sd_speaking_contact = c(19.5, 14.6),
                               mean_close_contact = c(13.2, 12.2),
                               sd_close_contact = c(25.4, 15.6)) %>% 
  mutate(max_speaking_contact = mean_speaking_contact + sd_speaking_contact,
         min_speaking_contact = max(0, mean_speaking_contact - sd_speaking_contact),
         max_close_contact = mean_close_contact + sd_close_contact,
         min_close_contact = max(0, mean_close_contact - sd_close_contact))

ctis_destefano_gender <- gender_estimates %>% 
  filter(state_fips == 37) %>% 
  mutate(gender_cat = ifelse(gender == 1, "men", "women")) %>% 
  group_by(fips, gender_cat) %>% 
  summarise(mean_contact = mean(scale_baseline))


ctis_destefano_gender %>% ggplot(aes(x = gender_cat)) +
  geom_violin(aes(y = mean_contact), fill = "dodgerblue", alpha = 0.5, col = NA) +
  geom_pointrange(data = destefano_gender, 
                  aes(y = mean_close_contact, 
                      ymax = max_close_contact,
                      ymin = min_close_contact),
                  col = "firebrick", lwd = 1.5, size = 1) +
  geom_pointrange(data = destefano_gender, 
                  aes(y = mean_speaking_contact, 
                      ymax = max_speaking_contact,
                      ymin = min_speaking_contact),
                  col = "orange", alpha = 0.65, lwd = 1.5, size = 1) +
  labs(y = "Mean contact", x = "Gender")  +
  theme_bw()

ggsave("figures/supp/validation_destefano_gender.pdf", height = 4, width = 6)

#### Feehan (prepandemic age) ####
# read in fb data downloaded from harvard dataverse from pandemic Feehan & Mahmud ms
fb_ego <- read_csv("data/validation/bics-paper-dataverse/bics-paper-release/bics-paper-code/data/fb-2015-svy/fb_ego.csv") %>% 
  rename(compat_age_fct = ego_agecat_fb)
# these results don't match the plot in the supplement of Feehan & Mahmud pandemic paper,
#   perhaps because they use the mixing data to create the plot
# no raw gender data available, all I can tell is that males seem skewed higher but can't really use that
ctis_feehan_age <- age_estimates %>% select(fips, week, age, scale_baseline) %>% 
  mutate(age_fct = case_when(age == 1 ~ "18-54",
                             age == 2 ~ "55-64",
                             age == 3 ~ "65-74",
                             age == 4 ~ "75+"),
         compat_age_fct = case_when(age_fct == "18-54" ~ "[18,54)",
                                    age_fct == "55-64" ~ "[55,65)",
                                    age_fct == "65-74" ~ "[65,100]",
                                    age_fct == "75+" ~ "[65,100]")) %>% 
  group_by(fips, compat_age_fct) %>% 
  summarise(mean_contact = mean(scale_baseline)) 

ctis_feehan_age %>% ggplot(aes(x = compat_age_fct)) +
  geom_violin(aes(y = mean_contact), fill = "dodgerblue", alpha = 0.5, col = NA) +
  geom_boxplot(data = fb_ego, aes(y = num_cc), fill = NA, width = 0.5) +
  labs(y = "Mean contact", x = "Age group")+
  theme_bw()

ggsave("figures/supp/validation_feehan_age.pdf", height = 4, width = 6)


#### Nelson (age, gender, race) pandemic ####
# THIS IS PANDEMIC SO NEED TO LOAD PANDEMIC AGE AND GENDER DATA # 
nelson_age_data <- data.frame(compat_age_fct = c("18-24", "25-34", "35-44", "45-54", "55-64", "65+"),
                              mean_contact_2020 = c(20.9, 16, 18.3, 17.7, 10.7, 5.5),
                              median_contact_2020 = c(5, 6, 5, 5, 3, 2),
                              mean_contact_2021 = c(17.7, 23.3, 14.5, 16.8, 12.7, 6.0),
                              median_contact_2021 = c(7, 7, 6, 5, 4, 2),
                              contact_2020_25th = c(3, 3, 3, 2, 1, 1),
                              contact_2020_75th = c(20, 13, 13, 11, 7, 5),
                              contact_2020_90th = c(60, 35, 35, 32, 26, 10),
                              contact_2021_25th = c(3, 3, 3, 2, 1, 1),
                              contact_2021_75th = c(16, 14, 14, 12, 10, 6),
                              contact_2021_90th = c(62, 40, 23, 29, 20, 12))

nelson_gender_data <- data.frame(gender_cat = c("women", "men"),
                                 mean_contact_2020 = c(12.5, 15),
                                 mean_contact_2021 = c(11.3, 17.2),
                                 contact_2020_25th = c(2, 2),
                                 contact_2020_75th = c(9, 10),
                                 contact_2021_25th = c(2, 2),
                                 contact_2021_75th = c(10, 12))


nelson_race_data <- data.frame(race_cat_col = c("Hispanic", "White", "Black", "Asian", "Other"),
                               mean_contact_2020 = c(11.8, 14.3, 16.1, 6.8, 25.5),
                               mean_contact_2021 = c(14.8, 14.0, 15.5, 11.3, 28.2),
                               contact_2020_25th = c(2, 2, 1, 2, 1),
                               contact_2020_75th = c(12, 9, 10, 7, 13),
                               contact_2021_25th = c(2, 2, 1, 2, 2),
                               contact_2021_75th = c(13, 11, 9, 9, 21))

# I think this should still use pandemic data since Nelson is Aug-Dec 2020?
ctis_nelson_age_2020 <- pandemic_age %>% select(fips, week, age, fit) %>% 
  mutate(age_fct = case_when(age == 1 ~ "18-54",
                             age == 2 ~ "55-64",
                             age == 3 ~ "65-74",
                             age == 4 ~ "75+"),
         compat_age_fct = ifelse(age_fct == "65-74", "65+",
                                 ifelse(age_fct == "75+", "65+", age_fct))) %>% 
  filter(week %within% interval(ymd("2020-08-01"), ymd("2020-12-31"))) %>% 
  mutate(year = year(floor_date(week, unit = "year"))) %>% 
  group_by(fips, compat_age_fct, year) %>% 
  summarise(mean_contact = mean(fit)) 

ctis_nelson_age_2021 <- pandemic_age %>% select(fips, week, age, fit) %>% 
  mutate(age_fct = case_when(age == 1 ~ "18-54",
                             age == 2 ~ "55-64",
                             age == 3 ~ "65-74",
                             age == 4 ~ "75+"),
         compat_age_fct = ifelse(age_fct == "65-74", "65+",
                                 ifelse(age_fct == "75+", "65+", age_fct))) %>% 
  filter(week %within% interval(ymd("2021-03-01"), ymd("2021-04-30"))) %>% 
  mutate(year = year(floor_date(week, unit = "year"))) %>% 
  group_by(fips, compat_age_fct, year) %>% 
  summarise(mean_contact = mean(fit)) 

ctis_nelson_age <- ctis_nelson_age_2020 %>% 
  rename(mean_contact_2020 = mean_contact) %>% 
  select(-year) %>% 
  left_join(ctis_nelson_age_2021 %>% 
              select(-year) %>% 
              rename(mean_contact_2021 = mean_contact), 
            by = c("fips", "compat_age_fct"))

ctis_nelson_age %>% ggplot(aes(x = compat_age_fct)) +
  geom_violin(aes(y = mean_contact_2020), fill = "navyblue", alpha = 0.5, col = NA) +
  geom_violin(aes(y = mean_contact_2021), fill = "dodgerblue", alpha = 0.5, col = NA) +
  geom_pointrange(data = nelson_age_data, 
                  aes(y = mean_contact_2020, 
                      ymax = contact_2020_75th,
                      ymin = contact_2020_25th),
                  col = "firebrick", lwd = 1.5, size = 1) +
  geom_pointrange(data = nelson_age_data, 
                  aes(y = mean_contact_2021, 
                      ymax = contact_2021_75th,
                      ymin = contact_2021_25th),
                  col = "orange", alpha = 0.65, lwd = 1.5, size = 1) +
  labs(y = "Mean contact", x = "Age group")+
  theme_bw()

ggsave("figures/supp/validation_nelson_age.pdf", height = 4, width = 6)

ctis_nelson_gender_2020 <- pandemic_gender %>% select(fips, week, gender, fit) %>% 
  mutate(gender_cat = ifelse(gender == 1, "men", "women")) %>% 
  filter(week %within% interval(ymd("2020-08-01"), ymd("2020-12-31"))) %>% 
  mutate(year = year(floor_date(week, unit = "year"))) %>% 
  group_by(fips, gender_cat, year) %>% 
  summarise(mean_contact = mean(fit)) 

ctis_nelson_gender_2021 <- pandemic_gender %>% select(fips, week, gender, fit) %>% 
  mutate(gender_cat = ifelse(gender == 1, "men", "women")) %>% 
  filter(week %within% interval(ymd("2021-03-01"), ymd("2021-04-30"))) %>% 
  mutate(year = year(floor_date(week, unit = "year"))) %>% 
  group_by(fips, gender_cat, year) %>% 
  summarise(mean_contact = mean(fit)) 

ctis_nelson_gender <- ctis_nelson_gender_2020 %>% 
  rename(mean_contact_2020 = mean_contact) %>% 
  select(-year) %>% 
  left_join(ctis_nelson_gender_2021 %>% 
              select(-year) %>% 
              rename(mean_contact_2021 = mean_contact), 
            by = c("fips", "gender_cat"))

ctis_nelson_gender %>% ggplot(aes(x = gender_cat)) +
  geom_violin(aes(y = mean_contact_2020), fill = "navyblue", alpha = 0.5, col = NA) +
  geom_violin(aes(y = mean_contact_2021), fill = "dodgerblue", alpha = 0.5, col = NA) +
  geom_pointrange(data = nelson_gender_data, 
                  aes(y = mean_contact_2020, 
                      ymax = contact_2020_75th,
                      ymin = contact_2020_25th),
                  col = "firebrick", lwd = 1.5, size = 1) +
  geom_pointrange(data = nelson_gender_data, 
                  aes(y = mean_contact_2021, 
                      ymax = contact_2021_75th,
                      ymin = contact_2021_25th),
                  col = "orange", alpha = 0.65, lwd = 1.5, size = 1) +
  labs(y = "Mean contact", x = "Gender")+
  theme_bw()

ggsave("figures/supp/validation_nelson_gender.pdf", height = 4, width = 6)

ctis_nelson_race_2020 <- pandemic_race %>% select(state, week, race_cat_col, fit) %>% 
  filter(week %within% interval(ymd("2020-09-01"), ymd("2020-12-31"))) %>% 
  mutate(year = year(floor_date(week, unit = "year"))) %>% 
  group_by(state, race_cat_col, year) %>% 
  summarise(mean_contact = mean(fit)) 

ctis_nelson_race_2021 <- pandemic_race %>% select(state, week, race_cat_col, fit) %>% 
  filter(week %within% interval(ymd("2021-03-01"), ymd("2021-04-30"))) %>% 
  mutate(year = year(floor_date(week, unit = "year"))) %>% 
  group_by(state, race_cat_col, year) %>% 
  summarise(mean_contact = mean(fit)) 

ctis_nelson_race <- ctis_nelson_race_2020 %>% 
  rename(mean_contact_2020 = mean_contact) %>% 
  select(-year) %>% 
  left_join(ctis_nelson_race_2021 %>% 
              select(-year) %>% 
              rename(mean_contact_2021 = mean_contact), 
            by = c("state", "race_cat_col"))

ctis_nelson_race %>% 
  mutate(race_cat_col = str_to_title(race_cat_col)) %>% 
  ggplot(aes(x = race_cat_col)) +
  geom_violin(aes(y = mean_contact_2020), fill = "navyblue", alpha = 0.5, col = NA) +
  geom_violin(aes(y = mean_contact_2021), fill = "dodgerblue", alpha = 0.5, col = NA) +
  geom_pointrange(data = nelson_race_data, 
                  aes(y = mean_contact_2020, 
                      ymax = contact_2020_75th,
                      ymin = contact_2020_25th),
                  col = "firebrick", lwd = 1.5, size = 1) +
  geom_pointrange(data = nelson_race_data, 
                  aes(y = mean_contact_2021, 
                      ymax = contact_2021_75th,
                      ymin = contact_2021_25th),
                  col = "orange", alpha = 0.65, lwd = 1.5, size = 1) +
  labs(y = "Mean contact", x = "Race/Ethnicity")+
  theme_bw()

ggsave("figures/supp/validation_nelson_race.pdf", height = 4, width = 6)

#### Dorelien (pandemic race, age) ####
dorelien_race_data <- data.frame(race_cat_col = c("Black", "Asian", "White", "Hispanic", "Other"),
                                 mean_contact = c(4.4, 5.86, 6.01, 2.87, 9.79),
                                 contact_sd = c(5.35, 8.59, 11.28, 2.91, 15.69),
                                 contact_se = c(0.84, 1.16, 0.26, 0.46, 4.96))

dorelien_age_data <- data.frame(age_fct = c("20-24", "25-29", "30-34", "35-39", "40-44", 
                                            "45-49", "50-55", "55-59", "60-64", "65-69", "70-74", "75+"),
                                mean_contact = c(4.1, 4.1, 6.65, 4.60, 12.53, 8.0, 6.19, 9.26, 4.85, 2.84, 2.41, 3.00),
                                contact_se = c(0.76, 0.75, 1.45, 0.38, 2.11, 1.26, 0.57, 0.87, 0.61, 0.50, 0.23, 0.36))

dorelien_gender_data <- data.frame(gender_cat = c("men", "women"),
                                   mean_contact = c(4.79, 6.58),
                                   contact_se = c(0.26, 0.37))

ctis_dorelien_age <- pandemic_age %>% 
  filter(state == "MN") %>% 
  filter(week < ymd("2020-05-17"), week > ymd("2020-04-30")) %>% # DATE ISSUE!!!
  select(fips, week, age, fit) %>% 
  mutate(age_fct = case_when(age == 1 ~ "18-54",
                             age == 2 ~ "55-64",
                             age == 3 ~ "65-74",
                             age == 4 ~ "75+")) %>% 
  group_by(fips, age_fct) %>% 
  summarise(mean_contact = mean(fit)) 

ctis_dorelien_age %>% ggplot(aes(x = age_fct)) +
  geom_violin(aes(y = mean_contact), fill = "dodgerblue", alpha = 0.5, col = NA) +
  geom_pointrange(data = dorelien_age_data, 
                  aes(y = mean_contact, 
                      ymax = mean_contact + contact_se,
                      ymin = mean_contact - contact_se),
                  col = "firebrick", lwd = 1.5, size = 1, alpha = 0.75) +
  labs(y = "Mean contact", x = "Age group")+
  theme_bw()
ggsave("figures/supp/validation_dorelien2023_age.pdf", height = 4, width = 8)

ctis_dorelien_gender <- pandemic_gender %>% 
  filter(state == "MN") %>% 
  filter(week < ymd("2020-05-17"), week > ymd("2020-04-30")) %>% # DATE ISSUE!!!
  select(fips, week, gender, fit) %>% 
  mutate(gender_cat = ifelse(gender == 1, "men", "women")) %>% 
  group_by(fips, gender_cat) %>% 
  summarise(mean_contact = mean(fit)) 

ctis_dorelien_gender %>% ggplot(aes(x = gender_cat)) +
  geom_violin(aes(y = mean_contact), fill = "dodgerblue", alpha = 0.5, col = NA) +
  geom_pointrange(data = dorelien_gender_data, 
                  aes(y = mean_contact, 
                      ymax = mean_contact + contact_se,
                      ymin = mean_contact - contact_se),
                  col = "firebrick", lwd = 1.5, size = 1, alpha = 0.75) +
  labs(y = "Mean contact", x = "Gender")+
  theme_bw()
ggsave("figures/supp/validation_dorelien2023_gender.pdf", height = 4, width = 6)

ctis_dorelien_race <- pandemic_race %>% 
  filter(state == "MN") %>% 
  mutate(race_cat_col = str_to_title(race_cat_col))#%>% 
# #earlier than our entire time period 
# group_by(state, race_cat_col) %>% 
# summarise(mean_contact = mean(fit),
#           mean_error = mean(se.fit))

ctis_dorelien_race %>% 
  mutate(race_cat_col = str_to_title(race_cat_col)) %>% 
  ggplot(aes(x = race_cat_col)) +
  geom_violin(aes(y = fit),
              fill = "dodgerblue", alpha = 0.5, col = NA) +
  # geom_pointrange(aes(y = mean_contact, 
  #                 ymax = mean_contact + mean_error,
  #                 ymin = mean_contact - mean_error),
  #             col = "dodgerblue", lwd = 1.5, size = 1, alpha = 0.75) +
  geom_pointrange(data = dorelien_race_data, 
                  aes(y = mean_contact, 
                      ymax = mean_contact + contact_se,
                      ymin = mean_contact - contact_se),
                  col = "firebrick", lwd = 1.5, size = 1, alpha = 0.75) +
  labs(y = "Mean contact", x = "Race/Ethnicity")+
  theme_bw()
ggsave("figures/supp/validation_dorelien2023_race.pdf", height = 4, width = 6)


#### Feehan & Mahmud (pandemic age, gender, urban/rural) ####
# only waves 2-3 overlap with our data collection
mahmud_data <- readRDS("data/validation/bics-paper-dataverse 2/bics-paper-release/bics-paper-code/data/df_all_waves.rds") %>% 
  filter(city == "National", wave %in% c(2, 3)) %>% 
  mutate(race_cat_col = ifelse(hispanic == 1, "Hispanic", 
                               ifelse(ethnicity == "Black", "Black", 
                                      ifelse(ethnicity == "White", "White", "Other"))),
         ur_cat = ifelse(urbanrural == "Suburban", 3,
                         ifelse(urbanrural == "Urban", 1,
                                ifelse(urbanrural == "Rural", 5, NA))),
         gender_cat = ifelse(gender == "Male", "men", ifelse(gender == "Female", "women", NA))) %>% 
  # need to deal with outliers...
  # I think probably use same approach as polymod
  mutate(num_cc_nonhh_trunc = ifelse(num_cc_nonhh > 72, NA, num_cc_nonhh))

library(boot)
N <- 1000
ages <- sort(unique(mahmud_data$agecat_w0))
urs <- sort(unique(mahmud_data$ur_cat))
races <- sort(unique(mahmud_data$race_cat_col))
genders <- sort(unique(mahmud_data$gender_cat))
mahmud_ages <- data.frame()
mahmud_urs <- data.frame()
mahmud_races <- data.frame()
mahmud_genders <- data.frame()
for(w in c(2, 3)){
  for(a in 1:length(ages)){
    doi <- mahmud_data %>% 
      filter(agecat_w0 == ages[[a]], wave == w) %>% 
      filter(! is.na(num_cc_nonhh_trunc))
    for(i in 1:N){
      # SHOULD I USE WEIGHTS?
      boot_samp <- mean(sample(doi$num_cc_nonhh_trunc, size = nrow(doi), replace = TRUE, prob = doi$weight_pooled)) 
      mahmud_ages <- mahmud_ages %>% rbind(data.frame(agecat_w0 = ages[[a]], boot_mean = boot_samp, wave = w))
    }
  }
  for(u in 1:length(urs)){
    doi <- mahmud_data %>% 
      filter(ur_cat == urs[[u]], wave == w) %>% 
      filter(! is.na(num_cc_nonhh_trunc))
    for(i in 1:N){
      # SHOULD I USE WEIGHTS?
      boot_samp <- mean(sample(doi$num_cc_nonhh_trunc, size = nrow(doi), replace = TRUE, prob = doi$weight_pooled)) 
      mahmud_urs <- mahmud_urs %>% rbind(data.frame(ur_cat = urs[[u]], boot_mean = boot_samp, wave = w))
    }
  }
  for(r in 1:length(races)){
    doi <- mahmud_data %>% 
      filter(race_cat_col == races[[r]], wave == w) %>% 
      filter(! is.na(num_cc_nonhh_trunc))
    for(i in 1:N){
      # SHOULD I USE WEIGHTS?
      boot_samp <- mean(sample(doi$num_cc_nonhh_trunc, size = nrow(doi), replace = TRUE, prob = doi$weight_pooled)) 
      mahmud_races <- mahmud_races %>% rbind(data.frame(race_cat_col = races[[r]], boot_mean = boot_samp, wave = w))
    }
  }
  for(g in 1:length(genders)){
    doi <- mahmud_data %>% 
      filter(gender_cat == genders[[g]], wave == w) %>% 
      filter(! is.na(num_cc_nonhh_trunc))
    for(i in 1:N){
      # SHOULD I USE WEIGHTS?
      boot_samp <- mean(sample(doi$num_cc_nonhh_trunc, size = nrow(doi), replace = TRUE, prob = doi$weight_pooled)) 
      mahmud_genders <- mahmud_genders %>% rbind(data.frame(gender_cat = genders[[g]], boot_mean = boot_samp, wave = w))
    }
  }
}


all_ur <- mahmud_urs %>% 
  mutate(survey = "Feehan & Mahmud") %>%
  rename(mean_contact = boot_mean) %>% 
  bind_rows(spatiotemporal_fits %>% 
              mutate(wave = ifelse(week %within% interval(start = ymd("2020-06-17"), end = ymd("2020-06-23")), 2,
                                   ifelse(week %within% interval(start = ymd("2020-09-11"), end = ymd("2020-09-26")), 3, NA))) %>% 
              group_by(fips, ur_code, wave) %>% 
              summarise(mean_contact = mean(fit)) %>% 
              mutate(survey = "ctis") %>% 
              mutate(ur_cat = as.integer(ur_code))) %>% 
  mutate(ur_name = as.factor(ifelse(ur_cat %in% c(1, 2), "urban",
                                    ifelse(ur_cat %in% c(3, 4), "suburban",
                                           ifelse(ur_cat %in% c(5, 6), "rural",
                                                  NA)))))
all_ur$ur_name <- factor(all_ur$ur_name, levels = c("urban", "suburban", "rural"))

all_ur %>%   
  filter(! is.na(wave)) %>% 
  ggplot(aes(x = ur_name, y = mean_contact, fill = interaction(survey, wave))) + 
  geom_violin(alpha = 0.75, col = NA, position = "identity") + 
  scale_fill_manual(values = c("#0E286D","#AE0000", "#0088AC","#E04200")) +
  labs(x = "Urbanicity", y = "Mean contact", fill = "Survey.Wave") +
  theme_bw() 

ggsave("figures/supp/validation_mahmud_ur.pdf", height = 4, width = 6)

all_ages <- mahmud_ages %>% 
  mutate(survey = "Feehan & Mahmud") %>%
  rename(mean_contact = boot_mean) %>% 
  bind_rows(pandemic_age %>% select(fips, week, age, fit) %>% 
              mutate(wave = ifelse(week %within% interval(start = ymd("2020-06-17"), end = ymd("2020-06-23")), 2,
                                   ifelse(week %within% interval(start = ymd("2020-09-11"), end = ymd("2020-09-26")), 3, NA))) %>% 
              group_by(fips, age, wave) %>% 
              summarise(mean_contact = mean(fit)) %>% 
              mutate(agecat_w0 = case_when(age == 1 ~ "[18-55)",
                                           age == 2 ~ "[55-65)",
                                           age == 3 ~"[65-75)",
                                           age == 4 ~ "[75, 100)"),
                     survey = "CTIS"))

all_ages %>%   
  filter(! is.na(wave)) %>% 
  ggplot(aes(x = agecat_w0, y = mean_contact, fill = interaction(survey, wave))) + 
  geom_violin(alpha = 0.75, col = NA, position = "identity") + 
  scale_fill_manual(values = c("#0E286D","#AE0000", "#0088AC","#E04200")) +
  labs(x = "Age", y = "Mean contact", fill = "Survey.Wave") +
  theme_bw() 
ggsave("figures/supp/validation_mahmud_age.pdf", height = 4, width = 8)

all_genders <- mahmud_genders %>% 
  mutate(survey = "Feehan & Mahmud") %>%
  rename(mean_contact = boot_mean) %>% 
  bind_rows(pandemic_gender %>% 
              mutate(gender_cat = ifelse(gender == 1, "men", "women")) %>% 
              select(fips, week, gender_cat, fit) %>% 
              mutate(wave = ifelse(week %within% interval(start = ymd("2020-06-17"), end = ymd("2020-06-23")), 2,
                                   ifelse(week %within% interval(start = ymd("2020-09-11"), end = ymd("2020-09-26")), 3, NA))) %>% 
              group_by(fips, gender_cat, wave) %>% 
              summarise(mean_contact = mean(fit)) %>% 
              mutate(survey = "ctis"))

all_genders %>%   
  filter(! is.na(wave)) %>% 
  ggplot(aes(x = gender_cat, y = mean_contact, fill = interaction(survey, wave))) + 
  geom_violin(alpha = 0.75, col = NA, position = "identity") + 
  scale_fill_manual(values = c("#0E286D","#AE0000", "#0088AC","#E04200")) +
  labs(x = "Gender", y = "Mean contact", fill = "Survey.Wave") +
  theme_bw() 
ggsave("figures/supp/validation_mahmud_gender.pdf", height = 4, width = 6)

all_races <- mahmud_races %>% 
  mutate(survey = "Feehan & Mahmud") %>%
  rename(mean_contact = boot_mean) %>% 
  bind_rows(pandemic_race %>% 
              select(state, week, race_cat_col, fit) %>% 
              mutate(wave = ifelse(week %within% interval(start = ymd("2020-06-17"), end = ymd("2020-06-23")), 2,
                                   ifelse(week %within% interval(start = ymd("2020-09-11"), end = ymd("2020-09-26")), 3, NA))) %>% 
              group_by(state, race_cat_col, wave) %>% 
              summarise(mean_contact = mean(fit)) %>% 
              mutate(survey = "ctis"))

all_races %>%   
  filter(wave == 3) %>% 
  mutate(race_cat_col = str_to_title(race_cat_col)) %>% 
  ggplot(aes(x = race_cat_col, y = mean_contact, fill = interaction(survey, wave))) + 
  geom_violin(alpha = 0.75, col = NA, position = "identity") + 
  scale_fill_manual(values = c("#0E286D","#AE0000", "#0088AC","#E04200")) +
  labs(x = "Race/Ethnicity", y = "Mean contact", fill = "Survey.Wave") +
  theme_bw() 
ggsave("figures/supp/validation_mahmud_race.pdf", height = 4, width = 6)

# num_cc_nonhh is what we want
# ethnicity and hispanic hold race/ethnicity
# LEFT OFF HERE WHAT DO AGE GROUPS MEAN, agecat is missing alot so use the other?
# urbanrural cats are suburban, rural, and urban

#### Kiti work data (gender, age, setting, race) ####
# over two days
kiti_age_data <- data.frame(age_fct = c("20-29", "30-39", "40-49", "50-59", "60+"),
                            contact_rd1 = c(2, 2, 3, 2, 2)/2,
                            contact_rd1_liqr = c(1, 1, 2, 1, 1)/2,
                            contact_rd1_uiqr = c(3, 4, 5, 4, 4)/2,
                            contact_rd2 = c(6, 6, 9, 6, 8)/2,
                            contact_rd2_liqr = c(3, 4, 5, 4, 4)/2,
                            contact_rd2_uiqr = c(9, 9, 12, 9, 12)/2)
kiti_race_data <- data.frame(race_cat_col = c("Hispanic", "Asian", "Black", "White", "Mixed", "Other"),
                             contact_rd1 = c(3, 2, 2, 3, 2, 3)/2,
                             contact_rd1_liqr = c(1, 1, 1, 1, 1, 1)/2,
                             contact_rd1_uiqr = c(4, 3, 4, 4, 4, 4)/2,
                             contact_rd2 = c(6, 8, 7, 7, 4, 4)/2,
                             contact_rd2_liqr = c(4, 3, 5, 4, 3, 3)/2,
                             contact_rd2_uiqr = c(11, 9, 10, 10, 9, 5)/2)
kiti_gender_data <- data.frame(gender_cat = c("women", "men"),
                               contact_rd1 = c(2, 3)/2,
                               contact_rd1_liqr = c(1, 1)/2,
                               contact_rd1_uiqr = c(4, 4)/2,
                               contact_rd2 = c(6, 7)/2,
                               contact_rd2_liqr = c(4, 4)/2,
                               contact_rd2_uiqr = c(10, 10)/2)

# Rd 1 is Apr-June 2020
# Rd 2 is Nov 20 - Jan 2021
ctis_kiti_age_2020 <- pandemic_age %>% 
  filter(state == "GA") %>% 
  select(fips, week, age, fit) %>% 
  mutate(age_fct = case_when(age == 1 ~ "18-54",
                             age == 2 ~ "55-64",
                             age == 3 ~ "65-74",
                             age == 4 ~ "75+")) %>% 
  filter(week %within% interval(ymd("2020-04-01"), ymd("2020-06-30"))) %>% # DATE ISSUE!!!
  #mutate(year = year(floor_date(week, unit = "year"))) %>% 
  group_by(fips, age_fct) %>% 
  summarise(mean_contact = mean(fit)) 

ctis_kiti_age_2021 <- pandemic_age %>%
  filter(state == "GA") %>% 
  select(fips, week, age, fit) %>% 
  mutate(age_fct = case_when(age == 1 ~ "18-54",
                             age == 2 ~ "55-64",
                             age == 3 ~ "65-74",
                             age == 4 ~ "75+")) %>% 
  filter(week %within% interval(ymd("2020-11-01"), ymd("2021-01-31"))) %>% 
  #mutate(year = year(floor_date(week, unit = "year"))) %>% 
  group_by(fips, age_fct) %>% 
  summarise(mean_contact = mean(fit)) 
# SOMETHING IS MESSED UP HERE!!!
ctis_kiti_age <- ctis_kiti_age_2020 %>% 
  rename(contact_rd1 = mean_contact) %>% 
  #select(-year) %>% 
  left_join(ctis_kiti_age_2021 %>% 
              #select(-year) %>% 
              rename(contact_rd2 = mean_contact), 
            by = c("fips", "age_fct"))

ctis_kiti_age %>% ggplot(aes(x = age_fct)) +
  geom_violin(aes(y = contact_rd1), fill = alpha("navyblue", 1), col = alpha("navyblue", 0.5)) +
  geom_violin(aes(y = contact_rd2), fill = alpha("dodgerblue", 1), col = alpha("dodgerblue", 0.5)) +
  geom_pointrange(data = kiti_age_data, 
                  aes(y = contact_rd1, 
                      ymax = contact_rd1_uiqr,
                      ymin = contact_rd1_liqr),
                  col = "firebrick", lwd = 1.5, size = 1) +
  geom_pointrange(data = kiti_age_data, 
                  aes(y = contact_rd2, 
                      ymax = contact_rd2_uiqr,
                      ymin = contact_rd2_liqr),
                  col = "orange", alpha = 0.65, lwd = 1.5, size = 1) +
  labs(y = "Mean contact", x = "Age group")+
  theme_bw()

ggsave("figures/supp/validation_kiti_age.pdf", height = 4, width = 6)

ctis_kiti_gender_2020 <- pandemic_gender %>% 
  filter(state == "GA") %>% 
  select(fips, week, gender, fit) %>% 
  mutate(gender_cat = ifelse(gender == 1, "men", "women")) %>% 
  filter(week %within% interval(ymd("2020-04-01"), ymd("2020-06-30"))) %>% 
  #mutate(year = year(floor_date(week, unit = "year"))) %>% 
  group_by(fips, gender_cat) %>% 
  summarise(mean_contact = mean(fit)) 

ctis_kiti_gender_2021 <- pandemic_gender %>% 
  filter(state == "GA") %>% select(fips, week, gender, fit) %>% 
  mutate(gender_cat = ifelse(gender == 1, "men", "women")) %>% 
  filter(week %within% interval(ymd("2020-11-01"), ymd("2021-01-31"))) %>% 
  #mutate(year = year(floor_date(week, unit = "year"))) %>% 
  group_by(fips, gender_cat) %>% 
  summarise(mean_contact = mean(fit)) 

ctis_kiti_gender <- ctis_kiti_gender_2020 %>% 
  rename(contact_rd1 = mean_contact) %>% 
  left_join(ctis_kiti_gender_2021 %>% 
              rename(contact_rd2 = mean_contact), 
            by = c("fips", "gender_cat"))

ctis_kiti_gender %>% ggplot(aes(x = gender_cat)) +
  geom_violin(aes(y = contact_rd1), fill = "navyblue", alpha = 0.5, col = NA) +
  geom_violin(aes(y = contact_rd2), fill = "dodgerblue", alpha = 0.5, col = NA) +
  geom_pointrange(data = kiti_gender_data, 
                  aes(y = contact_rd1, 
                      ymax = contact_rd1_uiqr,
                      ymin = contact_rd1_liqr),
                  col = "firebrick", lwd = 1.5, size = 1) +
  geom_pointrange(data = kiti_gender_data, 
                  aes(y = contact_rd2, 
                      ymax = contact_rd2_uiqr,
                      ymin = contact_rd2_liqr),
                  col = "orange", alpha = 0.65, lwd = 1.5, size = 1) +
  labs(y = "Mean contact", x = "Gender")+
  theme_bw()

ggsave("figures/supp/validation_kiti_gender.pdf", height = 4, width = 6)

ctis_kiti_race_2021 <- pandemic_race %>% 
  filter(state == "GA") %>% select(state, week, race_cat_col, fit) %>% 
  filter(week %within% interval(ymd("2020-11-01"), ymd("2021-01-31"))) #%>% 
#mutate(year = year(floor_date(week, unit = "year"))) %>% 
# group_by(state, race_cat_col) %>% 
# summarise(mean_contact = mean(fit)) 

ctis_kiti_race_2021 %>% 
  mutate(race_cat_col = str_to_title(race_cat_col)) %>% 
  ggplot(aes(x = race_cat_col)) +
  geom_violin(aes(y = fit), fill = "dodgerblue", alpha = 0.75, col = NA) +
  geom_pointrange(data = kiti_race_data, 
                  aes(y = contact_rd2, 
                      ymax = contact_rd2_uiqr,
                      ymin = contact_rd2_liqr),
                  col = "orange", alpha = 0.65, lwd = 1.5, size = 1) +
  labs(y = "Mean contact", x = "Race/Ethnicity")+
  theme_bw()

ggsave("figures/supp/validation_kiti_race.pdf", height = 4, width = 6)


#### BREEN ####

# more complicated to exclude household contacts here, would need to rerun models and 
# change functions sooo is it worth comparing without those?

# read in breen predictions (if I understand code correctly)
all_data <- data.frame()
for(i in 1:411){
  preds <- readRDS(paste0('data/validation/breen/data/predictions/prediction_day', i, ".rds"))[[2]]
  print(i)
  all_data <- all_data %>% bind_rows(preds)
}


all_data_agg <- all_data %>% 
  group_by(.ego_age, state, day_std) %>% 
  summarise(total_contact = sum(contacts)) %>% 
  mutate(age_grp = case_when(.ego_age == "[0,18)" ~ 0,
                             .ego_age == "[18,25)" ~ 1,
                             .ego_age == "[25,35)" ~ 2,
                             .ego_age == "[35,45)" ~ 3,
                             .ego_age == "[45,55)" ~ 4,
                             .ego_age == "[55,65)" ~ 5,
                             .ego_age == "[65,100]" ~ 6))

df.fips <- read_csv('data/input/state_and_county_fips_master.csv') %>% 
  mutate(fips = ifelse(fips == 46113, 46102, 
                       ifelse(fips == 2270, 2158, fips))) %>% 
  mutate(state_fips = as.integer(ifelse(nchar(as.character(fips)) == 5, substr(as.character(fips), 1, 2),
                                        substr(as.character(fips), 1, 1)))) %>% 
  filter(fips != 66) # guam

# I think to compare this we would have to reweight by ACS
all_acs <- read_csv("data/output/acs_target_data_for_contact_raking.csv") %>% 
  select(fips:prop_75up) %>% 
  left_join(df.fips) 

agg_to_state <- all_acs %>% 
  mutate(across(prop_18to24:prop_75up, ~ .x * all_18up)) %>% 
  mutate(prop_65up = prop_65to74 + prop_75up) %>% 
  select(-prop_65to74, -prop_75up) %>% 
  relocate(prop_65up, .after = prop_55to64) %>% 
  group_by(state) %>% 
  summarise(across(all_18up:prop_65up, ~ sum(.x))) %>% 
  mutate(across(prop_18to24:prop_65up, ~ .x/all_18up)) %>% 
  pivot_longer(prop_18to24:prop_65up, names_to = "age_range", values_to = "prop") %>% 
  mutate(age_grp = case_when(age_range == "prop_18to24" ~ 1,
                             age_range == "prop_25to34" ~ 2,
                             age_range == "prop_35to44" ~ 3,
                             age_range == "prop_45to54" ~ 4,
                             age_range == "prop_55to64" ~ 5,
                             age_range == "prop_65up" ~ 6))

data(state.regions)
wtd_st_mus <- all_data_agg %>% 
  filter(age_grp > 0) %>% 
  left_join(agg_to_state) %>% 
  group_by(state) %>% 
  summarise(mean_contact = weighted.mean(total_contact, prop)) %>% 
  left_join(state.regions, by = c("state" = "abb")) %>% 
  mutate(rel_mean_contact = mean_contact/mean(mean_contact))

map <- StateChoropleth$new(wtd_st_mus %>% rename(value = rel_mean_contact))
map$ggplot_polygon <- geom_polygon(aes(fill = value), color = NA, linewidth = 0.01)
map$set_num_colors(1)
map$ggplot_scale <-scale_fill_met_c(name = "Hokusai3")
map$show_labels = FALSE # remove state labels
map <- map$render()
map


# now try *excluding* household contacts

all_data <- data.frame()
for(i in 1:412){
  preds <- readRDS(paste0('data/validation/breen/data/predictions/nonhh_prediction_day', i, ".rds"))
  print(i)
  all_data <- all_data %>% bind_rows(preds)
}


all_data_agg <- all_data %>% 
  group_by(.ego_age, state, day_std) %>% 
  summarise(total_contact = sum(contacts)) %>% 
  mutate(age_grp = case_when(.ego_age == "[0,18)" ~ 0,
                             .ego_age == "[18,25)" ~ 1,
                             .ego_age == "[25,35)" ~ 2,
                             .ego_age == "[35,45)" ~ 3,
                             .ego_age == "[45,55)" ~ 4,
                             .ego_age == "[55,65)" ~ 5,
                             .ego_age == "[65,100]" ~ 6))

# I think to compare this we would have to reweight by ACS
all_acs <- read_csv("data/output/acs_target_data_for_contact_raking.csv") %>% 
  select(fips:prop_75up) %>% 
  left_join(df.fips) 

agg_to_state <- all_acs %>% 
  mutate(across(prop_18to24:prop_75up, ~ .x * all_18up)) %>% 
  mutate(prop_65up = prop_65to74 + prop_75up) %>% 
  select(-prop_65to74, -prop_75up) %>% 
  relocate(prop_65up, .after = prop_55to64) %>% 
  group_by(state) %>% 
  summarise(across(all_18up:prop_65up, ~ sum(.x))) %>% 
  mutate(across(prop_18to24:prop_65up, ~ .x/all_18up)) %>% 
  pivot_longer(prop_18to24:prop_65up, names_to = "age_range", values_to = "prop") %>% 
  mutate(age_grp = case_when(age_range == "prop_18to24" ~ 1,
                             age_range == "prop_25to34" ~ 2,
                             age_range == "prop_35to44" ~ 3,
                             age_range == "prop_45to54" ~ 4,
                             age_range == "prop_55to64" ~ 5,
                             age_range == "prop_65up" ~ 6))

data(state.regions)
wtd_st_mus <- all_data_agg %>% 
  filter(age_grp > 0) %>% 
  left_join(agg_to_state) %>% 
  group_by(state) %>% 
  summarise(mean_contact = weighted.mean(total_contact, prop)) %>% 
  ungroup() %>% 
  left_join(state.regions, by = c("state" = "abb")) %>% 
  mutate(rel_mean_contact = mean_contact/mean(mean_contact))

grad_mid <- (1-0.45)/(1.5-0.45)
map <- StateChoropleth$new(wtd_st_mus %>% rename(value = rel_mean_contact))
map$ggplot_polygon <- geom_polygon(aes(fill = value), color = NA, linewidth = 0.01)
map$set_num_colors(1)
map$ggplot_scale <- scale_fill_gradientn(colors = met.brewer("OKeeffe1"),
                                         limits = c(0.3, 1.7),
                                         name = "Mean relative\ncontact",
                                         values = c(1, grad_mid, 0),
                                         breaks = c(0.5, 1, 1.5),
                                         labels = c("below", "mean", "above"))
map$show_labels = FALSE # remove state labels
map <- map$render()
map -> breen_map

# load my spatiotemporal and agg to state
map_rel_means <- spatiotemporal_fits %>%
  filter(week > ymd("2020-05-31"), week < ymd("2021-05-01")) %>% 
  mutate(nat_mean = mean(fit)) %>% 
  group_by(state, nat_mean) %>% 
  summarise(mu = mean(fit)) %>% 
  ungroup() %>% 
  mutate(rel_mu = mu/nat_mean) %>% 
  left_join(state.regions, by = c("state" = "abb")) 
# weighting problematic because no weight for imputed counties either

map <- StateChoropleth$new(map_rel_means %>% rename(value = rel_mu))
map$ggplot_polygon <- geom_polygon(aes(fill = value), color = NA, linewidth = 0.01)
map$set_num_colors(1)
map$ggplot_scale <- scale_fill_gradientn(colors = met.brewer("OKeeffe1"),
                                         limits = c(0.3, 1.7),
                                         name = "Mean relative\ncontact",
                                         values = c(1, grad_mid, 0),
                                         breaks = c(0.5, 1, 1.5),
                                         labels = c("below", "mean", "above"))
map$show_labels = FALSE # remove state labels
map <- map$render()
map -> my_map

library(ggpubr)
ggarrange(my_map, breen_map, ncol = 2, labels = "AUTO")
ggsave("figures/supp/validation-w-breen.pdf", height = 5, width = 16)

#### Crawford et al 2022 ####
crawford <- read_csv("data/validation/contact_data_dryad.csv")
summary(crawford)
# from: https://data.ct.gov/Local-Government/Connecticut-Towns-Crosswalk-with-Tax-Codes-and-FIP/5hqs-h5c3/data
ct_cross <- read_csv("data/validation/connecticut_towns_crosswalk.csv") %>% 
  mutate(county_fips = as.integer(substr(`FIPS Code`, 1, 5)))
head(ct_cross)

crawford_county <- crawford %>% left_join(ct_cross, by = c("town" = "Town Name")) %>% 
  # categorizing norwich as new london
  mutate(county_fips = ifelse(town == "Norwich", 9011, county_fips)) %>% 
  mutate(week = floor_date(date, unit = "week")) %>% 
  group_by(county_fips, week) %>% 
  summarise(mean_contact = mean(contact_rate)) %>% 
  ungroup() %>% 
  left_join(spatiotemporal_fits %>% select(fips, week, fit), by = c("county_fips" = "fips", "week")) %>% 
  filter(! is.na(fit)) %>% 
  filter(week >= ymd("2020-06-01"))

crawford_county %>% ggplot(aes(x = mean_contact, y = fit)) + 
  geom_point() + 
  geom_abline()

crawford_county %>% 
  rename(`Crawford et al. 2022` = mean_contact,
         CTIS = fit) %>% 
  pivot_longer(cols = c(`Crawford et al. 2022`, CTIS),
               names_to = "study", values_to = "contact") %>% 
  ggplot(aes(x = week, y = contact, col = study)) + 
  geom_point() + 
  geom_line(aes(group = interaction(study, county_fips))) +
  facet_wrap(~county_fips, nrow = 2) +
  scale_color_met_d(name = "Isfahan2", direction = 1) +
  scale_x_date(breaks = seq(as.Date("2020-06-01"), as.Date("2021-01-31"),
                            by = "3 month"),
               labels = c("Jun 2020", "Oct", "Jan 2021"),
               minor_breaks = "1 month") +
  theme_bw() +
  theme(legend.position = "inside",
        legend.position.inside = c(0.87, 0.35),
        legend.background = element_rect(fill = "transparent", color = NA),
        legend.text = element_text(size = 14),
        legend.title = element_text(size = 16)) +
  labs(y = "Mean contact")
ggsave("figures/supp/validation-crawford.pdf", height = 5, width = 10)
