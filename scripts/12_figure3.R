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

age_estimates <- read_csv("estimates/baseline_contact_by_county_week_by_age.csv",
                          col_types = "diDDidfidiiididiccidddddddddddddddddddddddddddiddddddddddddddddddd")

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

# check age specific coefficient differences
age_spec <- age_estimates %>% dplyr::select(fips, age, intercept, contains("slope")) %>% 
  distinct()

(age_spec %>%  mutate(age_fct = case_when(age == 1 ~ "18-54",
                                          age == 2 ~ "55-64",
                                          age == 3 ~"65-74",
                                          age == 4 ~ "75+")) %>% 
    ggplot(aes(x = age_fct, y = national_cases_roll4_slope)) + 
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

truncated_colors <- met.brewer("Tam", n = 6)[1:4]
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
    scale_color_manual(values = truncated_colors) +
    #ylim(0, 20) +
    theme(legend.position = "none",
          axis.text = element_text(size = 14),
          axis.title = element_text(size = 16),
          strip.text = element_text(size = 16)) -> age_pandemic)

#### gender results ####
gender_estimates <- read_csv("estimates/baseline_contact_by_county_week_by_gender.csv",
                             col_types = "diDDidfidiiididiccidddddddddddddddddddddddddddiddddddddddddddddddd")

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
  dplyr::select(fips, gender, intercept, contains("slope")) %>% 
  distinct()

(gender_spec %>% mutate(gender_cat = ifelse(gender == 1, "men", "women")) %>% 
    ggplot(aes(x = gender_cat, y = national_cases_roll4_slope)) + 
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
setting_estimates <- read_csv("data/output/baseline_contact_by_county_week_by_setting_full.csv",
                              # the file without full has fewer columns so it can fit on github
                              col_types = "diDDidfidiiididiccidddddddddddddddddddddddddddiddddddddddddddddddd")

setting_estimates_reduced <- setting_estimates %>% 
  dplyr::select(contact_fit, fips, week, samp_size, non_hh_contacts, setting, 
                national_cases_roll4, ur_code, state, 
                national_cases_roll4_slope, percent_vaxxed_slope, 
                sum_county_measures_shift_slope, StringencyIndex_Average_roll3_shiftmin_slope,
                `national_cases_roll4:percent_vaxxed_slope`, residual, pred_lm,
                baseline, phi, scale_baseline, hhs_region, sum_county_measures_shift,
                StringencyIndex_Average_roll3_shiftmin, percent_vaxxed, intercept)

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
  dplyr::select(fips, setting, intercept, contains("slope")) %>% 
  distinct()
setting_spec$setting <- factor(setting_spec$setting, levels = c("other", "social", "shopping", "work"))

(setting_spec %>% 
    ggplot(aes(x = setting, y = national_cases_roll4_slope)) + 
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

#### setting time series ####
# supplementary figure requires additional datasets
setting_spatiotemporal_fits <- read_csv(paste0("spatiotemporal/contactsetting_72trunc_m1/fitted_predictions_work.csv"),
                                        col_types = "ddiicdiDccDccfi") %>% ungroup() %>%
  mutate(setting = "work") %>% 
  bind_rows(read_csv(paste0("spatiotemporal/contactsetting_72trunc_m1/fitted_predictions_social.csv"),
                     col_types = "ddiicdiDccDccfi") %>% ungroup() %>%
              mutate(setting = "social")) %>% 
  bind_rows(read_csv(paste0("spatiotemporal/contactsetting_72trunc_m1/fitted_predictions_shopping.csv"),
                     col_types = "ddiicdiDccDccfi") %>% ungroup() %>%
              mutate(setting = "shopping")) %>% 
  bind_rows(read_csv(paste0("spatiotemporal/contactsetting_72trunc_m1/fitted_predictions_other.csv"),
                     col_types = "ddiicdiDccDccfi") %>% ungroup() %>%
              mutate(setting = "other"))


setting_timeseries <- setting_spatiotemporal_fits %>% 
  dplyr::select(fips, week, setting, fit) %>%
  rename(contact_fit = fit) %>% 
  group_by(fips, setting) %>% 
  mutate(z_contact = c(scale(contact_fit)),
         mean_contact = mean(contact_fit)) %>% 
  group_by(setting) %>% 
  mutate(rel_mean_contact = mean_contact/mean(contact_fit)) %>% 
  ungroup()

setting_timeseries %>% 
  ggplot(aes(x = week, y = z_contact, group = interaction(fips, setting),
             col = rel_mean_contact)) + # this intercept is incorporating the random effect
  geom_line(alpha = 0.4) +
  theme_dark() +
  theme(axis.text = element_text(size = 16),# angle = 10, hjust = 1),
        plot.subtitle=element_text(size=16, hjust=0.5),
        strip.text = element_text(size = 16),
        axis.title = element_text(size = 18),
        axis.title.y = element_text(vjust = 2),
        panel.spacing = unit(2, "lines"),
        legend.text = element_text(size = 14),
        legend.title = element_text(size = 14, vjust = 0.5), # + 
        legend.position = "right") + #c(0.8, 0.3)) +  #"right") +
  guides(color = guide_colorbar(title = "Relative\npandemic\ncontact",
                                title.position = "top", title.vjust = 5)) + 
  scale_x_date(breaks = seq(as.Date("2020-07-01"), as.Date("2021-04-30"),
                            by = "3 month"),
               labels = c("Jul 2020", "Oct", "Jan 2021",
                          "Apr"),
               minor_breaks = "1 month") +
  scale_y_continuous(limits = c(-5, 2.5), breaks = c(-2, -1, 0, 1, 2)) + # can change this
  labs(y = "Mean contacts\n(z-score)") +
  scale_color_gradientn(colors = met.brewer("OKeeffe1"), 
                        limits = c(0.45, 1.5),
                        breaks = c(0.5, 1, 1.5), 
                        labels = c("below", "mean", "above"),
                        values = c(1, (1-0.45)/(1.5-0.45), 0)) +
  facet_wrap(~setting) -> setting_fig2a

nyt_national_roll <- read_csv("data/input/nyt-us-national-rolling-avg.csv",
                              col_types = "Dciddidd") %>% 
  mutate(week = round_date(date, unit = "week")) %>% 
  group_by(week) %>%
  summarise(national_cases = sum(cases)) %>% 
  #national_cases = ifelse(national_cases < 0, 0, national_cases)) %>% # three negs?? 
  ungroup() %>% 
  mutate(#national_cases_roll3 = zoo::rollmean(national_cases, k = 3, fill = NA, align = "right"),
    national_cases_roll4 = zoo::rollmean(national_cases, k = 4, fill = NA, align = "right")) %>% 
  #national_cases_roll3_per100k = round(national_cases_roll3/(331900000/1e5), 2), # total us pop
  #national_cases_roll4_per100k = round(national_cases_roll4/(331900000/1e5), 2)) %>% 
  ungroup()

# add national incidence below temporal contact curve
(nyt_national_roll %>% 
    mutate(national_cases_roll3c = zoo::rollmean(national_cases, k = 3, fill = NA, align = "center")) %>% 
    filter(week >= ymd("2020-06-07"),
           week <= ymd("2021-04-25")) %>% 
    ggplot(aes(x = week, y = scale(national_cases_roll3c))) + # cases_avg is rolling average, smooths drops in weekend/holidays
    geom_line(linewidth = 1, col = "black") + # aes(col = "black")) +
    #scale_color_identity(guide = "legend", name = "", labels = "National incident cases") +
    theme_classic() +
    labs(y = "Natl. incid.\n(z-score)") +
    scale_x_date(expand = c(0,0),
                 limits = c(as.Date("2020-06-01"), as.Date("2021-04-25"))) +#, breaks = seq(as.Date("2020-09-15"), as.Date("2021-05-15"), by = "1 month"), DATE_LABELS ="%B") +
    theme(panel.background = element_rect(fill = "transparent"), # bg of the panel
          plot.background = element_rect(fill = "transparent", color = NA), # bg of the plot
          axis.title.x = element_blank(),
          axis.text.x = element_blank(),
          axis.text.y = element_text(color = "black"),
          axis.ticks = element_blank(), # change to just y to line up
          #axis.title.y = element_blank(),
          #axis.line.y = element_blank(),
          axis.line.x = element_blank()
    ) -> casecurve)

cc_grob <- ggplotGrob(casecurve)

pb <- ggplot_build(setting_fig2a)
xlim <- pb$layout$panel_params[[1]]$x.range
ylim <- pb$layout$panel_params[[1]]$y.range

# Use full width (xmin = full plot), and a narrow y-range at the bottom
inset_ylow  <- ylim[1]
inset_yhigh <- ylim[1] + 0.33 * diff(ylim)  # 15% of height at bottom

# Add the inset
setting_fig2a + annotation_custom(
  grob = cc_grob,
  xmin = xlim[1] - (0.0025 * xlim[1]),
  xmax = xlim[2],
  ymin = inset_ylow,
  ymax = inset_yhigh
)
ggsave("figures/supp/setting-timeseries.pdf", height = 6, width = 10)


#### add google mobility data #### 
# should I just do average 2020 difference since they don't give us 2019 and there's no reason to use time?
google_mob_data_read <- read_csv("data/input/2020_US_Region_Mobility_Report.csv") %>% 
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

#### google mobility data 2022 ####
# I want to look at google workplace signal at end of 2022 and to see if there is difference
#   from mobility in 2019/pre 2020 pandemic
google_mob_data_2020 <- read_csv("data/input/2020_US_Region_Mobility_Report.csv") %>% 
  rename(fips = census_fips_code) %>% 
  dplyr::select(-c(country_region_code, country_region, sub_region_1, sub_region_2, 
                   iso_3166_2_code, place_id, metro_area)) %>% 
  filter(!is.na(fips)) %>% 
  mutate(fips = as.integer(fips))
google_mob_data_2021 <- read_csv("data/input/2021_US_Region_Mobility_Report.csv") %>% 
  rename(fips = census_fips_code) %>% 
  dplyr::select(-c(country_region_code, country_region, sub_region_1, sub_region_2, 
                   iso_3166_2_code, place_id, metro_area)) %>% 
  filter(!is.na(fips)) %>% 
  mutate(fips = as.integer(fips))
google_mob_data_2022 <- read_csv("data/input/2022_US_Region_Mobility_Report.csv") %>% 
  rename(fips = census_fips_code) %>% 
  dplyr::select(-c(country_region_code, country_region, sub_region_1, sub_region_2, 
                   iso_3166_2_code, place_id, metro_area)) %>% 
  filter(!is.na(fips)) %>% 
  mutate(fips = as.integer(fips))

google_mob_data_2022_weekly <- google_mob_data_2020 %>% 
  bind_rows(google_mob_data_2021) %>% 
  bind_rows(google_mob_data_2022) %>% 
  # gets weekly data
  mutate(week = floor_date(date, unit = "week")) %>% 
  group_by(fips, week) %>% 
  summarise(retail_week = mean(retail_and_recreation_percent_change_from_baseline),
            grocery_week = mean(grocery_and_pharmacy_percent_change_from_baseline),
            parks_week = mean(parks_percent_change_from_baseline),
            transit_week = mean(transit_stations_percent_change_from_baseline),
            work_week = mean(workplaces_percent_change_from_baseline),
            residential_week = mean(residential_percent_change_from_baseline))

google_mob_data_2022_weekly %>% 
  ggplot(aes(x = week, y = work_week, group = fips)) + 
  geom_line(alpha = 0.03) + 
  theme_bw() + 
  geom_hline(yintercept = 0, col = "dodgerblue") +
  geom_vline(xintercept = ymd("2020-03-16"), col = "firebrick", lty = "dashed") +
  labs(y = "Mean weekly work mobility\nrelative to baseline") -> pgoogle

# look at BTS data, from: 
# https://www.bts.gov/daily-travel
# https://data.bts.gov/Research-and-Statistics/Trips-by-Distance/w96p-f2qv/about_data
# this file is too big to upload to github, but can be downloaded at link above
bts <- read_csv("data/input/Trips_by_Distance_20241202.csv") 

bts_county <- bts %>% 
  filter(Level == "County") %>% 
  mutate(fips = as.integer(`County FIPS`),
         est_pop = `Population Staying at Home` + `Population Not Staying at Home`,
         prop_home = `Population Staying at Home`/est_pop) %>% 
  left_join(recent_pop_data) 

bts_weekly <- bts_county %>% 
  mutate(week = floor_date(Date, unit = "week")) %>% 
  group_by(fips, week) %>% 
  summarise(mean_prop_home = mean(prop_home, na.rm = T)) %>% 
  ungroup %>% 
  filter(! is.na(mean_prop_home))

# estimated population is about right

prepandemic_mean <- bts_weekly %>% 
  filter(week < ymd("2020-01-01")) %>% 
  pull(mean_prop_home) %>% 
  mean()

bts_weekly %>% left_join(urb_rur_codes) %>% 
  filter(ur_code != 6) %>% 
  ggplot(aes(x = week, y = mean_prop_home, group = fips)) + 
  geom_line(alpha = 0.03) + 
  theme_bw() + 
  geom_hline(yintercept = prepandemic_mean, col = "dodgerblue") +
  geom_vline(xintercept = ymd("2020-03-16"), col = "firebrick", lty = "dashed") +
  labs(y = "Mean proportion of\npeople staying home") -> pbts

new_data <- read_csv("data/input/social_distancing_county_2019_2020_2021_visitorcount.csv",
                     col_types = "Diddddddd") %>% 
  rename(fips = countyFIPS) %>% 
  filter(fips < 60000)

new_data %>% 
  mutate(week = floor_date(date, unit = "week")) %>% 
  group_by(fips, week) %>% 
  summarise(prop_visitor_maxvisitor_mean = mean(prop_visitor_maxvisitor)) %>% 
  ungroup() %>% 
  filter(week < ymd("2021-05-01")) -> safedata

safedata %>% 
  left_join(urb_rur_codes) %>% 
  filter(ur_code != 6) %>% 
  ggplot(aes(x = week, y = prop_visitor_maxvisitor_mean, group = fips)) +
  geom_line(alpha = 0.03) + 
  theme_bw() + 
  #geom_hline(yintercept = prepandemic_mean, col = "dodgerblue") +
  geom_vline(xintercept = ymd("2020-03-16"), col = "firebrick", lty = "dashed") +
  scale_x_date(date_labels = "%Y", date_breaks = "1 year") +
  labs(y = "Safegraph mobility metric") -> psafe

ggarrange(pgoogle, pbts, psafe, nrow = 3, labels = "AUTO")
ggsave("figures/supp/google-bts-comp.pdf", height = 8, width = 10)


# ------------------------------ #
#### make all together figure ####
# ------------------------------ #
# read in race and urban/rural data here
#### race data ####
race_estimates <- read_csv("estimates/baseline_contact_by_state_week_by_race.csv",
                           col_types = "dcDDidfiiididddddddddddddddddddddddddcicd")

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

race_spec <- race_estimates %>% dplyr::select(state, race_cat_col, intercept, contains("slope")) %>% 
  distinct()
levels(race_spec$race_cat_col) <- c("Asian", "Black", "Hispanic", "Other", "White")

(race_spec %>% ggplot(aes(x = race_cat_col, y = national_cases_roll4_slope)) + 
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
library(Cairo)
ggsave("figures/fig3.eps", device = cairo_ps, height = 8, width = 16)

slope_plot
ggsave("figures/supp/nat-coefs-demog.pdf", height = 8, width = 16)
