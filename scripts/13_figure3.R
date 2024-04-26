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
urb_rur_codes$ur_code <- factor(urb_rur_codes$ur_code, levels = c("1", "2", "3", "4", "5", "6"))


hhs_regions <- read_csv("data/input/HHS_regions.csv", col_types = "if") %>% 
  rename(fips = node, hhs_region = modularity_class)  %>% ungroup()

df.fips <- read_csv('data/input/state_and_county_fips_master.csv') %>% 
  mutate(fips = ifelse(fips == 46113, 46102, 
                       ifelse(fips == 2270, 2158, fips)))

#### age estimates ####

age_estimates <- read_csv("data/output/baseline_contact_by_county_week_by_age.csv",
                          col_types = "diDDidfiddiiiddfccfddddidddddddddddd")

# map of counties we used
amap <- CountyChoropleth$new(age_estimates %>% group_by(fips) %>% 
                               summarise(mean_contact_allage = mean(contact_fit)) %>% 
                               rename(value = mean_contact_allage,
                                      region = fips))
amap$ggplot_polygon <- geom_polygon(aes(fill = value), color = NA, linewidth = 0.01)
amap$set_num_colors(1)
amap$ggplot_scale <- scale_fill_gradientn(colors = met.brewer("Tam"))
amap <- amap$render()
print(amap)

foo <- age_estimates %>% left_join(urb_rur_codes) %>% 
  group_by(fips, ur_code) %>% 
  summarise()

# check age specific coefficient differences
age_spec <- age_estimates %>% dplyr::select(fips, age, intercept, slope_county, 
                                            slope_national, slope_policyox) %>% 
  distinct()

(age_spec %>%  mutate(age_fct = case_when(age == 1 ~ "18-54",
                                          age == 2 ~ "55-64",
                                          age == 3 ~"65-74",
                                          age == 4 ~ "75+")) %>% 
    ggplot(aes(x = age_fct, y = slope_national)) + 
    geom_jitter(aes(col = age_fct), alpha = 0.5) +
    geom_boxplot(fill = NA, outlier.shape = NA) + 
    labs(y = "National incidence slope", x = "Age") +
    theme_bw() +
    geom_hline(yintercept = 0, col = "black", lty = "dashed") +
    ylim(-6E-6, 4.1E-6) +
    #annotate(geom = "text", label = "More responsive", x = "55-64", y = -4.5E-06, col = "royalblue") +
    scale_color_natparks_d(name = "Arches") +
    theme(legend.position = "none",
          axis.text = element_text(size = 14),
          axis.title = element_text(size = 16)) -> age_slope)


agg_age_comp <- age_estimates %>% 
  pivot_longer(cols = c(contact_fit, baseline, scale_baseline),
               values_to = "num_contacts",
               names_to = "setting") %>% 
  group_by(fips, state, hhs_region, age, setting) %>% 
  summarise(mean_contact = mean(num_contacts))

agg_age_comp$setting <- factor(agg_age_comp$setting, levels = c("contact_fit", "baseline", "scale_baseline"))
levels(agg_age_comp$setting) <- c("pandemic", "disease = 0", "baseline") #CHECK THAT LEVELS ARE CORRECT
(agg_age_comp %>% 
    filter(setting != "disease = 0") %>% 
    mutate(age_fct = case_when(age == 1 ~ "18-54",
                               age == 2 ~ "55-64",
                               age == 3 ~"65-74",
                               age == 4 ~ "75+")) %>% 
    ggplot(aes(x = age_fct, y = mean_contact)) + 
    geom_jitter(aes(col = age_fct), alpha = 0.5) +
    geom_boxplot(fill = NA, outlier.shape = NA) +
    facet_wrap(~setting) +
    theme_bw() + 
    labs(y = "Mean contact", x = "Age") + 
    scale_color_met_d(name = "Tam") +
    #ylim(0, 20) +
    theme(legend.position = "none",
          axis.text = element_text(size = 14),
          axis.title = element_text(size = 16),
          strip.text = element_text(size = 16)) -> age_pandemic)

#### gender results ####
gender_estimates <- read_csv("data/output/baseline_contact_by_county_week_by_gender.csv",
                             col_types = "diDDidfiddiiiddfccfddddidddddddddddd")

# map of counties we used
gmap <- CountyChoropleth$new(gender_estimates %>% group_by(fips) %>% 
                               summarise(mean_contact_allg = mean(contact_fit)) %>% 
                               rename(value = mean_contact_allg,
                                      region = fips))
gmap$ggplot_polygon <- geom_polygon(aes(fill = value), color = NA, linewidth = 0.01)
gmap$set_num_colors(1)
gmap$ggplot_scale <- scale_fill_gradientn(colors = met.brewer("Tam"))
gmap <- gmap$render()
print(gmap)

foo <- gender_estimates %>% left_join(urb_rur_codes) %>% 
  group_by(fips, ur_code) %>% 
  summarise()

agg_gender_comp <- gender_estimates %>% 
  pivot_longer(cols = c(contact_fit, baseline, scale_baseline),
               values_to = "num_contacts",
               names_to = "setting") %>% 
  group_by(fips, state, hhs_region, gender, setting) %>% 
  summarise(mean_contact = mean(num_contacts))
agg_gender_comp$setting <- factor(agg_gender_comp$setting, levels = c("contact_fit", "baseline", "scale_baseline"))
levels(agg_gender_comp$setting) <- c("pandemic", "disease = 0", "baseline")
(agg_gender_comp %>% 
    #filter(setting == "pandemic") %>% 
    filter(setting != "disease = 0") %>% 
    mutate(gen_fct = case_when(gender == 1 ~ "men",
                               gender == 2 ~ "women")) %>% 
    ggplot(aes(x = gen_fct, y = mean_contact)) + 
    geom_jitter(aes(col = gen_fct), alpha = 0.5) +
    geom_boxplot(fill = NA, outlier.shape = NA) +
    facet_wrap(~setting) +
    theme_bw() + 
    #ylim(0, 20) +
    labs(y = "Mean contact", x = "Gender") + 
    scale_color_met_d(name = "Archambault", direction = 1) +
    theme(legend.position = "none",
          axis.text = element_text(size = 14),
          axis.title = element_text(size = 16),
          strip.text = element_text(size = 16)) -> gender_pandemic)

# check gender specific coefficient differences
gender_spec <- gender_estimates %>% 
  dplyr::select(fips, gender, intercept, slope_county, 
                slope_national, slope_policyox) %>% 
  distinct()

(gender_spec %>% mutate(gender_cat = ifelse(gender == 1, "men", "women")) %>% 
    ggplot(aes(x = gender_cat, y = slope_national)) + 
    geom_jitter(aes(col = gender_cat), alpha = 0.5) +
    geom_boxplot(fill = NA, outlier.shape = NA) + 
    labs(y = "National incidence slope", x = "Gender") +
    theme_bw() +
    geom_hline(yintercept = 0, col = "black", lty = "dashed") +
    #annotate(geom = "text", label = "More responsive", x = "female", y = -5.5E-06, col = "royalblue")
    scale_color_natparks_d(name = "Arches2", n = 5) +
    ylim(-6E-6, 4.1E-6) +
    theme(legend.position = "none",
          axis.text = element_text(size = 14),
          axis.title = element_text(size = 16)) -> gender_slope)


#### setting estimates ####
setting_estimates <- read_csv("data/output/baseline_contact_by_county_week_by_setting.csv",
                              col_types = "diDDidfiddiiiddfccfddddidddddddddddd")

coef_checks <- setting_estimates %>% 
  mutate(nat_effect = national_cases_roll4 * slope_national,
         county_effect = county_cases_roll4 * slope_county,
         pol_effect = oxz * slope_policyox) %>% 
  group_by(setting, state) %>% 
  summarise(mean_nat = mean(nat_effect),
            mean_county = mean(county_effect),
            mean_pol = mean(pol_effect))

# map coefficients
for(soi in c("other", "work", "social", "shopping")){
  doi <- setting_estimates %>% filter(setting == soi) %>% 
    dplyr::select(fips, week, contact_fit) %>% 
    #distinct() %>% 
    group_by(fips) %>% 
    summarise(mean_contact = mean(contact_fit)) %>% 
    rename(value = mean_contact,
           region = fips) 
  
  smap <- CountyChoropleth$new(doi)
  smap$ggplot_polygon <- geom_polygon(aes(fill = value), color = NA, linewidth = 0.01)
  smap$set_num_colors(1)
  smap$ggplot_scale <- scale_fill_gradientn(colors = met.brewer("VanGogh3"),
                                            #values = c(1, grad_mid, 0),
                                            name = paste0("Fit, ", soi))
  smap <- smap$render()
  print(smap)
}

time_data <- setting_estimates %>% 
  dplyr::select(fips, week, contact_fit, setting, state, hhs_region, ur_code) %>% 
  group_by(fips, setting, hhs_region, ur_code, state) %>% 
  mutate(z_contact = c(scale(contact_fit)),
         mean_contact = mean(contact_fit)) %>% 
  ungroup() %>% 
  mutate(rel_mean_contact = mean_contact/mean(contact_fit))

time_data %>% 
  ggplot(aes(x = week, y = z_contact, group = fips, col = hhs_region)) + # this intercept is incorporating the random effect
  geom_line(alpha = 0.3) +
  theme_bw() +
  theme(axis.text = element_text(size = 16),# angle = 10, hjust = 1),
        plot.subtitle=element_text(size=16, hjust=0.5),
        strip.text = element_text(size = 16),
        axis.title = element_text(size = 18),
        legend.text = element_text(size = 14),
        legend.title = element_text(size = 14, vjust = 0.5), # + 
        legend.position = "right") + #c(0.8, 0.3)) +  #"right") +
  scale_x_date(breaks = seq(as.Date("2020-07-01"), as.Date("2021-04-30"),
                            by = "3 month"),
               labels = c("Jul 2020", "Oct", "Jan 2021",
                          "Apr"),
               minor_breaks = "1 month") +
  scale_y_continuous(limits = c(-3, 3), breaks = c(-2, -1, 0, 1, 2)) + # can change this
  labs(y = "Mean contacts\n(z-score)") +
  facet_wrap(~setting)

setting_estimates %>% group_by(setting, week) %>% 
  summarise(mean_contact = mean(contact_fit),
            mean_baseline = mean(scale_baseline)) %>% 
  ggplot(aes(x = week, y = mean_contact)) +
  geom_line(aes(group = setting, col = setting))

agg_setting_comp <- setting_estimates %>% 
  pivot_longer(cols = c(contact_fit, baseline, scale_baseline),
               values_to = "num_contacts",
               names_to = "scenario") %>% 
  group_by(fips, state, hhs_region, scenario, setting) %>% 
  summarise(mean_contact = mean(num_contacts))
agg_setting_comp$scenario <- factor(agg_setting_comp$scenario, levels = c("contact_fit", "baseline", "scale_baseline"))
levels(agg_setting_comp$scenario) <- c("pandemic", "disease = 0", "baseline")
agg_setting_comp$setting <- factor(agg_setting_comp$setting, levels = c("other", "social", "shopping", "work"))
(agg_setting_comp %>% 
    #filter(scenario == "pandemic") %>% 
    filter(scenario != "disease = 0") %>% 
    ggplot(aes(x = setting, y = mean_contact)) + 
    geom_jitter(aes(col = setting), alpha = 0.5) +
    geom_boxplot(fill = NA, outlier.shape = NA) +
    facet_wrap(~scenario) +
    theme_bw() + 
    #ylim(0, 9) +
    labs(y = "Mean contact", x = "Setting") +
    scale_color_met_d(name = "Isfahan2") +
    scale_y_continuous(breaks = c(2, 4, 6, 8), labels =  c("2", "4", "6", "8")) +
    theme(legend.position = "none",
          axis.text = element_text(size = 14),
          axis.title = element_text(size = 16),
          strip.text = element_text(size = 16),
          axis.text.x = element_text(angle = 20, hjust = 1)) -> setting_pandemic)

setting_spec <- setting_estimates %>% 
  dplyr::select(fips, setting, intercept, #slope_county, 
                slope_national, slope_policyox) %>% 
  distinct()
setting_spec$setting <- factor(setting_spec$setting, levels = c("other", "social", "shopping", "work"))

(setting_spec %>% 
    ggplot(aes(x = setting, y = slope_national)) + 
    geom_jitter(aes(col = setting), alpha = 0.5) +
    geom_boxplot(fill = NA, outlier.shape = NA) + 
    geom_hline(yintercept = 0, col = "black", lty = "dashed") +
    theme_bw() + 
    labs(y = "National incidence slope", x = "Setting") +
    scale_color_natparks_d(name = "Volcanoes") +
    ylim(-6E-6, 4.1E-6) +
    theme(legend.position = "none",
          axis.text = element_text(size = 14),
          axis.title = element_text(size = 16),
          strip.text = element_text(size = 16),
          axis.text.x = element_text(angle = 20, hjust = 1)) -> setting_slope)


#### add google mobility data #### 
# should I just do average 2020 difference since they don't give us 2019 and there's no reason to use time?
google_mob_data_read <- read_csv("data/input/google_mobility_data/2020_US_Region_Mobility_Report.csv") %>% 
  rename(fips = census_fips_code) %>% 
  dplyr::select(-c(country_region_code, country_region, sub_region_1, sub_region_2, 
                   iso_3166_2_code, place_id, metro_area)) %>% 
  filter(!is.na(fips)) %>% 
  mutate(fips = as.integer(fips)) 

google_mob_data <- google_mob_data_read %>% 
  mutate(retail_roll = round(zoo::rollmean(retail_and_recreation_percent_change_from_baseline, k = 7, fill = NA, align = "center"), 2),
         grocery_roll = round(zoo::rollmean(grocery_and_pharmacy_percent_change_from_baseline, k = 7, fill = NA, align = "center"), 2),
         parks_roll = round(zoo::rollmean(parks_percent_change_from_baseline, k = 7, fill = NA, align = "center"), 2),
         transit_roll = round(zoo::rollmean(transit_stations_percent_change_from_baseline, k = 7, fill = NA, align = "center"), 2),
         work_roll = round(zoo::rollmean(workplaces_percent_change_from_baseline, k = 7, fill = NA, align = "center"), 2),
         residential_roll = round(zoo::rollmean(residential_percent_change_from_baseline, k = 7, fill = NA, align = "center"), 2))

google_mob_scale <- google_mob_data %>% 
  pivot_longer(cols = c(retail_and_recreation_percent_change_from_baseline, 
                        grocery_and_pharmacy_percent_change_from_baseline, 
                        parks_percent_change_from_baseline, 
                        transit_stations_percent_change_from_baseline, 
                        workplaces_percent_change_from_baseline, 
                        residential_percent_change_from_baseline), 
               names_to = "setting", values_to = "pct_change_from_baseline") %>% 
  filter(!is.na(pct_change_from_baseline)) %>% 
  filter(date > ymd("2020-09-30"), 
         date < ymd("2021-01-01")) %>% # over whole fall so I don't think we need to make weekly
  group_by(fips, setting) %>% 
  summarise(mean_change = mean(pct_change_from_baseline)) 

wmap <- CountyChoropleth$new(google_mob_scale %>% filter(setting == "workplaces_percent_change_from_baseline") %>% 
                               rename(value = mean_change,
                                      region = fips))
wmap$ggplot_polygon <- geom_polygon(aes(fill = value), color = NA, linewidth = 0.01)
wmap$set_num_colors(1)
wmap$ggplot_scale <- scale_fill_gradientn(colors = met.brewer("Hokusai3"))
wmap <- wmap$render()
print(wmap)

work_scaled <- setting_estimates %>% 
  filter(setting == "work") %>% 
  left_join(google_mob_scale %>% filter(setting == "workplaces_percent_change_from_baseline"), by = "fips") %>% 
  mutate(google_to_ratio = 100/(mean_change + 100),
         baseline_google = baseline * google_to_ratio)

work_scaled_plot <- work_scaled %>% 
  pivot_longer(cols = c(contact_fit, baseline, scale_baseline, baseline_google),
               values_to = "num_contacts",
               names_to = "scenario") %>% 
  group_by(fips, state, hhs_region, scenario) %>% 
  summarise(mean_contact = mean(num_contacts))
work_scaled_plot$scenario <- factor(work_scaled_plot$scenario, 
                                    levels = c("contact_fit", "baseline", "scale_baseline", "baseline_google"))
levels(work_scaled_plot$scenario) <- c("pandemic", "intercept + error", "Safegraph", "Google")
work_scaled_plot %>% 
  ggplot(aes(x = scenario, y = mean_contact)) + 
  geom_jitter(alpha = 0.5, col = "dimgrey") +
  geom_boxplot(fill = NA, outlier.shape = NA) +
  theme_bw() + 
  labs(y = "Mean work contacts", x = "") 
ggsave("figures/supp/google-vs-safegraph-work-comparison.pdf", height = 4, width = 6)


# ------------------------------ #
#### make all together figure ####
# ------------------------------ #
# read in race and urban/rural data here
#### race data ####
race_estimates <- read_csv("data/output/baseline_contact_by_state_week_by_race.csv",
                           col_types = "dcDDidfiiiddiddddddddddddddcicd")

agg_race_comp <- race_estimates %>% 
  pivot_longer(cols = c(contact_fit, baseline, scale_baseline),
               values_to = "num_contacts",
               names_to = "setting") %>% 
  group_by(state, race_cat_col, setting) %>% 
  summarise(mean_contact = mean(num_contacts))
agg_race_comp$setting <- factor(agg_race_comp$setting, levels = c("contact_fit", "baseline", "scale_baseline"))
levels(agg_race_comp$setting) <- c("pandemic", "disease = 0", "baseline")
levels(agg_race_comp$race_cat_col) <- c("Asian", "Black", "Hispanic", "Other", "White")
#race_fig <- 
(agg_race_comp %>% 
    #filter(setting == "pandemic") %>% 
    filter(setting != "disease = 0") %>% 
    ggplot(aes(x = race_cat_col, y = mean_contact)) + 
    geom_jitter(aes(col = race_cat_col), alpha = 0.5) +
    geom_boxplot(fill = NA, outlier.shape = NA) +
    facet_wrap(~setting) +
    theme_bw() + 
    labs(y = "Mean contact", x = "Race/Ethnicity") + #, caption = "Each point represents mean contact Sept-Feb for a given state,\nsee slack 01/09/2024 for more info")
    scale_color_met_d(name = "Java") +
    #ylim(0, 20) +
    theme(legend.position = "none",
          axis.text = element_text(size = 14),
          axis.title = element_text(size = 16),
          strip.text = element_text(size = 16),
          axis.text.x = element_text(angle = 20, hjust = 1)) -> race_pandemic)

race_spec <- race_estimates %>% dplyr::select(state, race_cat_col, intercept, #slope_state, slope_policyox,
                                              slope_national) %>% 
  distinct()
levels(race_spec$race_cat_col) <- c("Asian", "Black", "Hispanic", "Other", "White")

(race_spec %>% ggplot(aes(x = race_cat_col, y = slope_national)) + 
    geom_jitter(aes(col = race_cat_col), alpha = 0.5) +
    geom_boxplot(fill = NA, outlier.shape = NA) + 
    labs(y = "National incidence slope", x = "Race/Ethnicity") +
    theme_bw() +
    geom_hline(yintercept = 0, col = "black", lty = "dashed") +
    theme_bw() + 
    ylim(-6E-6, 4.1E-6) +
    labs(y = "National incidence slope", x = "Race/Ethnicity") +
    scale_color_natparks_d(name = "Triglav") +
    theme(legend.position = "none",
          axis.text = element_text(size = 14),
          axis.title = element_text(size = 16),
          strip.text = element_text(size = 16),
          axis.text.x = element_text(angle = 20, hjust = 1)) -> race_slope)

#### urban v rural estimates ####
mob_scaled_baseline_data <- read_csv("data/output/baseline_contact_by_county_week_nopolicy.csv",
                                     col_types = "diDDididdddiiiddddfcffddddccccddddddddddddddddd")
spatiotemporal_fits <- read_csv("data/output/normal_gamma2_72trunc/fitted_predictions.csv",
                                col_types = "ddiiddddddddiiDccDccf") %>% ungroup()
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
mob_scaled_baseline_data$ur_code <- factor(mob_scaled_baseline_data$ur_code, levels = c("1", "2", "3", "4", "5", "6"))
spatiotemporal_fits$ur_code <- factor(spatiotemporal_fits$ur_code, levels = c("1", "2", "3", "4", "5", "6"))

### trying with higher sampled counties - UR code ###
suff_samp_fips <- spatiotemporal_fits %>% 
  filter(week >= ymd("2020-10-01"),
         week <= ymd("2021-04-25")) %>%
  mutate(enough = ifelse(samp_size >= 10, 1, 0)) %>% 
  group_by(fips) %>%
  summarise(sum_samp = sum(enough, na.rm = T)) %>%
  filter(sum_samp == 30) %>% # 30 weeks
  pull(fips)

ur_plot_data3 <- mob_scaled_baseline_data %>%
  group_by(fips) %>% 
  summarise(mean_pandemic = mean(contact_fit),
            mean_baseline_nomob = mean(baseline),
            mean_baseline_mob = mean(scale_baseline)) %>% 
  filter(fips %in% suff_samp_fips) %>% 
  filter(fips < 15000 | fips >= 16000) %>%  # removing hawaii, alaska already removed
  pivot_longer(cols = c(mean_baseline_nomob, mean_baseline_mob, mean_pandemic), 
               names_to = "setting", values_to = "mean_contact")
ur_plot_data3$setting <- factor(ur_plot_data3$setting, levels = c("mean_pandemic", "mean_baseline_nomob", "mean_baseline_mob"))
levels(ur_plot_data3$setting) <- c("Pandemic", "Pre-pandemic no mob", "Pre-pandemic")

ur_plot_data3 %>% 
  left_join(urb_rur_codes) %>% 
  filter(setting != "Pre-pandemic no mob") %>% 
  ggplot(aes(x = ur_code, y = mean_contact)) + 
  geom_jitter(aes(col = ur_code), alpha = 0.5) +
  geom_boxplot(fill = NA, outlier.shape = NA) +
  facet_wrap(~setting) +
  labs(y = "Mean contact", x = "NCHS Urban-Rural Class") +
  theme_bw() + 
  scale_color_met_d(name = "Derain", direction = -1) +
  theme(legend.position = "none",
        axis.text = element_text(size = 14),
        axis.title = element_text(size = 16),
        strip.text = element_text(size = 16))

mob_scaled_baseline_data %>%
  dplyr::select(fips, ur_code, intercept, slope_national, slope_county) %>%  #, slope_policyox) %>% 
  distinct() %>% 
  filter(fips %in% suff_samp_fips) %>% 
  ggplot(aes(x = ur_code, y = slope_county)) + 
  geom_jitter(aes(col = ur_code), alpha = 0.5) +
  geom_boxplot(fill = NA, outlier.shape = NA) + 
  labs(y = "County incidence slope", x = "UR Class") +
  theme_bw() +
  geom_hline(yintercept = 0, lty = "dashed") +
  #annotate(geom = "text", label = "More responsive", x = "55-64", y = -4.5E-06, col = "royalblue") +
  scale_color_met_d(name = "Derain") +
  theme(legend.position = "none",
        axis.text = element_text(size = 14),
        axis.title = element_text(size = 16))

{ doi <- mob_scaled_baseline_data %>%
    dplyr::select(fips, ur_code, intercept, slope_national, #slope_county, 
                  slope_policyox) %>% 
    distinct() %>% 
    filter(fips %in% suff_samp_fips) %>% 
    rename(value = slope_national, region = fips)
  mid <- 0 # midpoint here is 0 not 1
  grad_mid <- (mid-min(doi$value))/(max(doi$value)-min(doi$value))
  slopemap <- CountyChoropleth$new(doi)
  slopemap$ggplot_polygon <- geom_polygon(aes(fill = value), color = NA, linewidth = 0.01)
  slopemap$set_num_colors(1)
  slopemap$ggplot_scale <- scale_fill_gradientn(colors = met.brewer("Benedictus"),
                                                values = c(1, grad_mid, 0),
                                                name = "National")
  slopemap <- slopemap$render()
  print(slopemap)
  }

ur_spec <- mob_scaled_baseline_data %>% 
  dplyr::select(fips, ur_code, intercept, #slope_county, 
                slope_national, slope_policyox) %>% 
  distinct()

ur_spec %>% 
  ggplot(aes(x = ur_code, y = slope_national)) + 
  geom_jitter(aes(col = ur_code), alpha = 0.5) +
  geom_boxplot(fill = NA, outlier.shape = NA) + 
  labs(y = "National incidence slope", x = "UR Class") +
  theme_bw() +
  geom_hline(yintercept = 0, col = "royalblue", lty = "dashed") +
  #annotate(geom = "text", label = "More responsive", x = "female", y = -5.5E-06, col = "royalblue")
  scale_color_met_d(name = "Derain") +
  theme(legend.position = "none",
        axis.text = element_text(size = 14),
        axis.title = element_text(size = 16))

contact_plot <- ggarrange(age_pandemic, gender_pandemic,
                          race_pandemic, setting_pandemic,
                          ncol = 2, nrow = 2,
                          labels = "AUTO", font.label = list(size = 20))
slope_plot <- ggarrange(age_slope, gender_slope,
                        race_slope, setting_slope,
                        ncol = 2, nrow = 2,
                        labels = "AUTO", font.label = list(size = 20))

contact_plot
ggsave("figures/fig3.pdf", height = 8, width = 16)

slope_plot
ggsave("figures/supp/nat-coefs-demog.pdf", height = 8, width = 16)
