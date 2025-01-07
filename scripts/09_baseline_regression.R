### run a regression to estimate what would have happened in the absence of the pandemic

# load libraries
{
  library(plyr)
  library(tidyverse)
  # library(tidylog)
  library(lubridate)
  library(readxl)
  library(viridis)
  library(lme4)
  library(choroplethr)
  library(choroplethrMaps)
  library(NatParksPalettes)
  library(MetBrewer)
  library(formula.tools)
  library(ggpubr)
  library(zoo)
  library(scales)
  library(grid)
  library(car)
  library(purrr)
  library(Cairo)
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

spatiotemporal_fits <- read_csv("data/output/normal_gamma2_72trunc_m1/fitted_predictions.csv",
                                col_types = "ddiiddddddddiiDccDccf") %>% ungroup()

### raw time series ###
# library("scales")
# integer_breaks <- function(n = 5, ...) {
#   breaker <- pretty_breaks(n, ...)
#   function(x) {
#     breaks <- breaker(x)
#     breaks[breaks == floor(breaks)]
#   }
# }
# 
# non6_fips <- spatiotemporal_fits %>% left_join(urb_rur_codes) %>% filter(ur_code != 6) %>% 
#   pull(fips) %>% unique() %>% sample(36)
# spatiotemporal_fits %>% 
#   filter(fips %in% non6_fips) %>% 
#   filter(week >= ymd("2020-06-01")) %>% 
#   ggplot(aes(x = week, y = non_hh_contacts)) + 
#   geom_line(aes(group = fips)) +
#   facet_wrap(~fips, scale = "free_y") +
#   labs(y = "Raw mean contact") +
#   scale_y_continuous(breaks = integer_breaks()) +
#   scale_x_date(breaks = seq(as.Date("2020-06-01"), as.Date("2021-04-30"),
#                             by = "3 month"),
#                labels = c("Jul 2020", "Oct", "Jan 2021",
#                           "Apr"),
#                minor_breaks = "1 month") +
#   theme_bw()
# ggsave("figures/supp/raw-time-series.pdf", height = 8, width = 16)

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
  mutate(county_cases_roll4 = round(zoo::rollmean(county_cases, k = 4, fill = NA, align = "right"), 2)) %>% 
  ungroup() %>% 
  mutate(state_fips = as.integer(as.numeric(ifelse(nchar(as.character(fips)) == 4, 
                                                   substr(as.character(fips), 1, 1),
                                                   substr(as.character(fips), 1, 2)))))

# per 100k deals with some of the scaling issues, similar to how zscores would
# may need it for national though so that national and county are on the same scale

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


all_cases <- nyt_county_case_data %>% 
  left_join(nyt_state_case_data) %>% 
  left_join(nyt_national_roll) %>% 
  filter(week > ymd("2020-05-31"),
         week < ymd("2021-05-01")) # have to redo bc round to week

# not doing state cases, but could download from NYT and follow same steps if necessary

# formatted by Casey Zipfel, 
# missing values could have no order or could be missing?
# county_SAH and state_SAH are all NA or zero in my regression period
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
                ~ifelse(is.na(.x), 0, .x))) %>% # assuming if missing data that no order
  mutate(sum_county_measures = county_SAH + county_gathering_ban + county_rest_closure +
           county_bar_closure + county_mask_mandate,
         sum_state_measures = state_SAH + state_gathering_ban + state_rest_closure +
           state_bar_closure + state_mask_mandate) %>% 
  filter(week > ymd("2020-10-01")) %>% 
  group_by(fips) %>% 
  mutate(min_sum_county_measures = min(sum_county_measures)) %>% 
  ungroup() %>% 
  mutate(sum_county_measures_shift = sum_county_measures - min_sum_county_measures)

# oxford stringency data
full_oxdata <- read_csv("data/input/OxCGRT_compact_subnational_v1.csv") %>% 
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

full_oxdata %>% 
  pivot_longer(cols = c(ContainmentHealthIndex_Average, GovernmentResponseIndex_Average, 
                        StringencyIndex_Average), 
               names_to = "policy", values_to = "index") %>% 
  separate(col = policy, sep = "_", into = c("policy", NA)) %>% 
  left_join(state_mean_strin_study_pd) %>% 
  filter(policy == "StringencyIndex") %>% 
  #filter(state %in% c("OH", "MA", "MD", "CA")) %>% 
  ggplot(aes(x = week)) +
  geom_line(aes(y = index, col = policy)) +
  geom_vline(xintercept = ymd("2020-10-01"), lty = "dashed") +
  geom_vline(xintercept = ymd("2021-05-01"), lty = "dashed") +
  facet_wrap(~state, nrow = 9) + #, ncol = 1) +
  labs(y = "Stringency", x = "week") +
  scale_color_manual(values = c("navy", "cornflowerblue", "firebrick")) +
  theme_bw() +
  theme(legend.position = "none") #c(0.7, 0.05))
ggsave("figures/supp/policy-over-longer-time.pdf", height = 14, width = 16)

# version from Andrew Tiu's paper
dataverse_vax_data <- read_csv("data/input/COVID_county_vacc_data_dataverse.csv") %>% 
  mutate(fips = as.integer(COUNTY),
         percent_vaxxed = CASES * 100,
         week = floor_date(converted_date, unit = "week")) %>% 
  dplyr::select(fips, week, percent_vaxxed)

combine_data <- 
  spatiotemporal_fits %>% dplyr::select(fit, se.fit, fips, week, month, samp_size, non_hh_contacts) %>% 
  left_join(all_cases %>% dplyr::select(-POPESTIMATE2020), by = c("fips", "week")) %>% 
  left_join(urb_rur_codes, by = "fips") %>% 
  left_join(df.fips, by = "fips") %>% 
  left_join(hhs_regions, by = "fips") %>% 
  left_join(policy_data, by = c("fips", "week")) %>% # do we need to fill in NAs with 0? 
  left_join(dataverse_vax_data, by = c("fips", "week")) %>% 
  left_join(oxdata, by = c("week", "state")) %>% 
  left_join(recent_pop_data, by = "fips") %>% 
  filter(! fips %in% c(36005, 36047, 36061, 36081, 36085, 8111)) %>% # CO no change in pct vax 
  filter(state != "AK") %>% 
  rename(contact_fit = fit) %>% 
  mutate(fips = as.factor(fips),
         state = as.factor(state)) %>% 
  filter(week <= ymd("2021-05-01"),
         week >= ymd("2020-06-01")) %>% 
  mutate(percent_vaxxed = ifelse(is.na(percent_vaxxed), 0, percent_vaxxed))

combine_data$hhs_region <- factor(combine_data$hhs_region,
                                  levels = c("1", "2", "3", "4", "5", "6", "7", "8", "9", "10"))
# need to filter some things
#   AK: missing election data
#   NYC boroughs are lumped into one for cases so not useful
#   some other fips missing rolling averages due to NYT data (around 169)
#   need to have contact estimates for each week, I think we completed so this should be ok?

fips_I_want <- data.frame(fips = c(6037, 12086, 4013, 22071, 53033, 48141,
                                   34013, 21111, 35001, 42003, 37183,
                                   48453, 18105, 36087, 1015, 23007, 33011,
                                   27003, 31019, 30029)) %>% 
  left_join(df.fips) %>% 
  mutate(full_name = paste0(gsub(" County", "", name), ", ", state)) %>% 
  arrange(fips)

fips_I_want_shorter <- fips_I_want %>% 
  filter(fips %in% c(30029, 31019, 1015, 33011, 27003,
                     4013, 48453, 12086, 53033, 34013))


#-------------------------------#
##### NO POOLING MODEL #####
#-------------------------------#
# Function to calculate VIFs while handling aliased coefficients
# had to figure out how to extract aliased coefs
calculate_vif <- function(model) {
  if (length(coef(model)) > 1) {  # ensure model is not intercept-only
    # check for aliased coefficients
    aliased <- rownames(alias(model)$Complete)
    non_aliased_terms <- setdiff(names(coef(model)), aliased)
    
    # refit the model without aliased terms (if necessary)
    if (length(non_aliased_terms) > 1) {  # must have at least two terms
      formula_new <- reformulate(non_aliased_terms[-1], response = "contact_fit")
      #print(formula_new)
      model_new <- lm(formula_new, data = model$model)
      vif_values <- vif(model_new, type = "predictor")
      return(vif_values)
    } else {
      return(NA)  # no VIFs for intercept-only models
    }
  } else {
    return(NA)  # no VIFs for intercept-only models
  }
}

mobility_scale <- read_csv("data/output/mobility_19_20_fall_ratio_new_norm.csv", col_types = "iddd")

#### FIXED COMPONENTS ####
fig2a_function <- function(combine_data){
  
  fig2a_data <- combine_data %>% 
    dplyr::select(fips, week, contact_fit) %>% 
    group_by(fips) %>% 
    mutate(z_contact = c(scale(contact_fit)),
           mean_contact = mean(contact_fit)) %>% 
    ungroup() %>% 
    mutate(rel_mean_contact = mean_contact/mean(contact_fit))
  
  fig2a_data %>% 
    ggplot(aes(x = week, y = z_contact, group = fips, col = rel_mean_contact)) + # this intercept is incorporating the random effect
    geom_line(alpha = 0.4) +
    theme_dark() +
    theme(axis.text = element_text(size = 16),# angle = 10, hjust = 1),
          plot.subtitle=element_text(size=16, hjust=0.5),
          strip.text = element_text(size = 16),
          axis.title = element_text(size = 18),
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
    scale_y_continuous(limits = c(-4, 2.25), breaks = c(-2, -1, 0, 1, 2)) + # can change this
    labs(y = "Mean contacts\n(z-score)") +
    scale_color_gradientn(colors = met.brewer("OKeeffe1"), 
                          limits = c(0.45, 1.5),
                          breaks = c(0.5, 1, 1.5), 
                          labels = c("below", "mean", "above"),
                          values = c(1, (1-0.45)/(1.5-0.45), 0)) -> fig2a
  
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
      ) -> case_curve)
  
  vp <- viewport(width = 0.795, height = 0.14, x = 0.452, y = 0.68)
  
  return(list(case_curve, vp, fig2a))
}

library(Cairo)
### MODEL ####
baseline_models_function <- function(model_formula, combine_data, 
                                     spatiotemporal_fits,
                                     add_filename, pdf_file = T, YN = 12, YP = 5, 
                                     URY = 5, PMIN = 0.45, PMAX = 1.5,
                                     is_interaction = T,
                                     calc_vif = F, startdate = "2020-10-01",
                                     fig1_wid = 10){
  
  model_data <- combine_data %>% 
    filter(week >= ymd(startdate),
           week <= ymd("2021-05-01")) %>%
    ungroup() %>%
    filter(! is.na(percent_vaxxed)) %>% 
    mutate(vax_pd = as.factor(ifelse(week < ymd("2021-01-01"), 1,
                                     ifelse(week < ymd("2021-03-15"), 2, 3)))) 
  
  no_pooling <- lmList(
    model_formula, model_data
  )
  
  ### MODEL DIAGNOSTICS ###
  model_sum <- summary(no_pooling)
  model_r2 <- c()
  for(j in 1:length(model_sum$adj.r.squared)){
    model_r2 <- c(model_r2, model_sum$adj.r.squared[[j]])
  }
  
  # p_values <- map(no_pooling, ~ summary(.x)$coefficients[,4])
  # p_values_df <- bind_rows(p_values, .id = "fips") %>% 
  #   rename(intercept = `(Intercept)`)
  # is_signif <- data.frame(p_values_df < 0.05)
  
  # Apply the VIF calculation to each model in the list
  if(calc_vif){
    vif_results <- lapply(no_pooling, calculate_vif)
    if(is_interaction){
      # Combine results into a data frame for inspection
      vif_results_df <- bind_rows(vif_results, .id = "fips") %>% 
        rownames_to_column() %>% 
        separate(rowname, into = c("predictor", NA), sep = "\\.\\.\\.") %>% 
        mutate(GVIF2 = `GVIF^(1/(2*Df))`^2) %>% # square this so usual interpretable scale
        select(fips, predictor, GVIF2) %>% 
        mutate(predictor = paste0(predictor, "_gvif")) %>% 
        pivot_wider(names_from = predictor, values_from = GVIF2) %>% 
        rowwise() %>% 
        mutate(mean_gvif = mean(c_across(contains("gvif")), na.rm = T))
      
      mean_vif <- mean(vif_results_df$mean_gvif)
    }else{
      # Combine results into a data frame for inspection
      vif_results_df <- sapply(vif_results, mean) %>% 
        as.data.frame() %>% 
        rownames_to_column("fips") %>% 
        rename(vif = ".") 
      
      mean_vif <- vif_results_df %>% pull(vif) %>% mean()
    }
    
    print(paste("mean model vif:", mean(mean_vif)))
  }
  print(paste("mean model r2:", mean(model_r2)))
  
  
  #### BASELINE ####
  
  baseline_data <- model_data %>%
    mutate(fips = as.integer(as.numeric(levels(fips))[as.integer(fips)])) %>%
    left_join(coef(no_pooling) %>%
                rownames_to_column("fips") %>%
                as_tibble() %>%
                mutate(fips = as.integer(fips)) %>%
                rename(intercept = `(Intercept)`) %>%
                rename_with(~paste0(., "_slope"), -c(fips, intercept)),
              by = "fips"
    ) %>%
    # extract standard error on each coefficient
    left_join(coef(summary(no_pooling))[, "Std. Error", ] %>%
                # [row (fips), column (estimate, error, t), coef (national, county, policy)]
                as.data.frame() %>%
                rownames_to_column("fips") %>%
                as_tibble() %>%
                mutate(fips = as.integer(fips)) %>%
                rename(intercept_se = `(Intercept)`) %>%
                rename_with(~paste0(., "_se"), -c(fips, intercept_se)),
              by = "fips"
    ) %>%
    cbind(residual = resid(no_pooling)) %>% # this is residual from linear prediction from model vs fit value from spatiotemp model
    #left_join(county_min_strin) %>%
    cbind(pred_lm = predict(no_pooling)) %>%
    mutate(baseline = intercept + residual) %>%
    ungroup()
  
  #### scale baseline ####
  
  mob_scaled_baseline_data <- baseline_data %>%
    left_join(mobility_scale) %>%
    mutate(scale_baseline = baseline * phi)
  
  write_csv(mob_scaled_baseline_data, paste0("data/output/baseline_contact_by_county_week", add_filename, ".csv"))
  
  plot_df_all <- mob_scaled_baseline_data %>%
    mutate(fit_seplus = contact_fit + se.fit,
           fit_seminus = contact_fit - se.fit,
           baseline_seplus = (baseline + intercept_se) * phi,
           baseline_seminus = (baseline - intercept_se) * phi) %>%
    dplyr::select(fips, week, samp_size, ur_code, contact_fit, scale_baseline,
                  fit_seplus, fit_seminus, baseline_seplus, baseline_seminus) %>%
    ungroup() %>%
    rename(fit_contact = contact_fit, baseline_contact = scale_baseline) %>%
    pivot_longer(cols = c(fit_contact, baseline_contact, fit_seplus, fit_seminus,
                          baseline_seplus, baseline_seminus),
                 names_to = c("scenario", "metric"),
                 names_sep = "_",
                 values_to = c("value")) %>%
    pivot_wider(names_from = "metric", values_from = "value",
                values_fn = list) %>% # have to do this for some duplicate values but i dont see any
    unnest(cols = everything()) %>% # have to do this for some duplicate values but i dont see any
    mutate(fips = as.factor(fips),
           scenario = as.factor(scenario))
  plot_df_all %>% group_by(scenario, fips) %>%
    summarise(mean_contact = mean(contact)) %>%
    mutate(fips = as.integer(as.numeric(levels(fips))[as.integer(fips)])) %>%
    pivot_wider(names_from = scenario, values_from = mean_contact) %>%
    rename(pandemic = fit) %>%
    mutate(diff = baseline - pandemic) -> mean_pan_diff
  
  #sample_fips <- sample(unique(plot_df_all %>% filter(ur_code != 6) %>% pull(fips)), 36)
  sample_fips <- c(5009, 12035, 13027, 13153, 13209, 17031, 18027, 18111, 22033, 22043, 24037,
                   26079, 28009, 28071, 29049, 31051, 36119, 37003, 37019, 37141, 37155, 39055,
                   41053, 42079, 47115, 48039, 48091, 48233, 51011, 53021, 53073, 54037, 54105,
                   55009, 55025, 55111)
  #I think can ignore error?
  if(startdate == "2020-06-01"){
    DATE_LABELS <- c("Jul 20", "Oct", "Jan 21", "Apr")
  }else{
    DATE_LABELS <- c("Oct", "Jan 2021","Apr")
  }
  
  TICK_START = ifelse(startdate == "2020-06-01", "2020-07-01", "2020-10-01")
  (plot_df_all %>%
      filter(fips %in% sample_fips) %>%
      ggplot() +
      geom_line(aes(x = week, y = contact,
                    group = interaction(fips, scenario), col = scenario),
                linewidth = 1.5, alpha = 0.75) +
      geom_ribbon(aes(x = week, ymin = seminus, ymax = seplus,
                      group = interaction(fips, scenario), fill = scenario), col = NA, alpha = 0.25) +
      geom_text(aes(x = ymd("2020-10-01"), y = 5, label = ur_code), check_overlap = T) +
      scale_x_date(breaks = seq(as.Date(TICK_START), as.Date("2021-04-30"),
                                by = "3 month"),
                   labels = DATE_LABELS,
                   minor_breaks = "1 month") +
      theme_bw() +
      labs(y = "Mean contacts") +
      theme(legend.title = element_blank(), #text(size = 14),
            legend.position = "none") +
      facet_wrap(~fips, nrow = 6) +
      scale_y_continuous(breaks = breaks_pretty(n = 4)) +
      scale_color_manual(values = c("#3F3E66", "#1E7D79")) +
      scale_fill_manual(values = c("#3F3E66", "#1E7D79"))  -> all_baseline)
  ggsave(paste0("figures/supp/random-trajectories", add_filename, ".pdf"), height = 10, width = 12)
  
  plot_df <- mob_scaled_baseline_data %>%
    mutate(fit_seplus = contact_fit + se.fit,
           fit_seminus = contact_fit - se.fit,
           baseline_seplus = (baseline + intercept_se) * phi,
           baseline_seminus = (baseline - intercept_se) * phi) %>%
    dplyr::select(fips, week, samp_size, ur_code, contact_fit, baseline, scale_baseline,
                  fit_seplus, fit_seminus, baseline_seplus, baseline_seminus) %>%
    ungroup() %>%
    filter(fips %in% fips_I_want_shorter$fips) %>%
    rename(fit_contact = contact_fit, baseline_contact = scale_baseline) %>%
    pivot_longer(cols = c(fit_contact, baseline_contact, fit_seplus, fit_seminus,
                          baseline_seplus, baseline_seminus),
                 names_to = c("scenario", "metric"),
                 names_sep = "_",
                 values_to = c("value")) %>%
    pivot_wider(names_from = "metric", values_from = "value") %>%
    mutate(fips = as.factor(fips),
           scenario = as.factor(scenario))
  levels(plot_df$fips) <- fips_I_want_shorter$full_name
  plot_df$fips <- factor(plot_df$fips, levels = c("Maricopa, AZ", "Miami-Dade, FL",
                                                  "King, WA", "Travis, TX", "Essex, NJ",
                                                  "Hillsborough, NH", "Anoka, MN", "Calhoun, AL",
                                                  "Buffalo, NE", "Flathead, MT"))
  levels(plot_df$scenario) <- c("non-pandemic", "pandemic")
  
  (plot_df %>%
      ggplot() +
      geom_line(aes(x = week, y = contact,
                    group = interaction(fips, scenario), col = scenario),
                linewidth = 1.5, alpha = 0.75) +
      geom_ribbon(aes(x = week, ymin = seminus, ymax = seplus,
                      group = interaction(fips, scenario), fill = scenario), col = NA, alpha = 0.25) +
      scale_x_date(breaks = seq(as.Date(TICK_START), as.Date("2021-04-30"),
                                by = "3 month"),
                   labels = DATE_LABELS,
                   minor_breaks = "1 month") +
      theme_bw() +
      labs(y = "Mean contacts") +
      theme(axis.text = element_text(size = 16),# angle = 10, hjust = 1),
            plot.subtitle=element_text(size=16, hjust=0.5),
            strip.text = element_text(size = 16),
            axis.title = element_text(size = 18),
            legend.text = element_text(size = 14),
            legend.title = element_blank(), #text(size = 14),
            legend.position = "none") +
      guides(color = guide_legend(ncol= 2)) +
      facet_wrap(~fips, nrow = 2) +
      scale_y_continuous(breaks = breaks_pretty(n = 4)) +
      scale_color_manual(values = c("#3F3E66", "#1E7D79")) +
      scale_fill_manual(values = c("#3F3E66", "#1E7D79"))  -> fig2b)
  
  dat_text <- data.frame(label = c("pandemic", "", "", "", "", "", "", "", "", ""),
                         label2 = c("non-pandemic", "", "", "", "", "", "", "", "", ""),
                         fips = c(4013, 1015, 12086, 27003, 30029, 31019, 33011, 34013, 48453, 53033)) %>%
    left_join(fips_I_want_shorter) %>%
    mutate(fips = as.factor(full_name))
  
  startX = ifelse(startdate == "2020-06-01", "2020-09-10", "2020-12-10")
  fig2b +
    geom_text(data = dat_text,
              mapping = aes(x = ymd(startX), y = YP, label = label),
              color = "#1E7D79",
              size = 6,
              fontface = "bold") +
    geom_text(data = dat_text,
              mapping = aes(x = ymd(startX), y = YN, label = label2),
              color = "#3F3E66",
              size = 6,
              hjust = 0.33,
              fontface = "bold") -> fig2b_fin
  if(pdf_file){
    fig2a_stuff <- fig2a_function(combine_data)
    pdf(paste0("figures/supp/fig1", add_filename, ".pdf"), height = 8, width = fig1_wid)
    print(ggarrange(fig2a_stuff[[3]], fig2b_fin, ncol = 1, heights = c(5, 6),
                    labels = "AUTO", font.label = list(size = 28)))
    print(fig2a_stuff[[1]], vp = fig2a_stuff[[2]])
    dev.off()
  }
  
  #### BASELINE VS PANDEMIC MAP ####
  baseline_map <- mob_scaled_baseline_data %>%
    filter(fips < 15000 | fips >= 16000) %>% # removing hawaii, alaska already removed
    ungroup() %>%
    mutate(baseline_rel = scale_baseline/mean(scale_baseline),
           pandemic_rel = contact_fit/mean(contact_fit),
           diff = scale_baseline - contact_fit) %>%
    group_by(fips, POPESTIMATE2020) %>%
    summarise(mean_baseline = mean(scale_baseline),
              mean_baseline_rel = mean(baseline_rel),
              mean_pandemic = mean(contact_fit),
              mean_pandemic_rel = mean(pandemic_rel),
              mean_diff = mean(diff))
  
  data(state.regions) # might have to run example in help
  states_i_want <- state.regions %>% filter(! abb %in% c("HI", "AK")) %>% dplyr::select(region)
  state_zoom = states_i_want$region
  
  midpoint <- 1
  # grad_mid <- (midpoint-min(baseline_map$mean_baseline_rel))/
  #   (max(baseline_map$mean_baseline_rel)-min(baseline_map$mean_baseline_rel))
  if(is.na(PMIN) | is.na(PMAX)){
    PMIN <- 0.1
    PMAX <- 2
  }
  grad_mid <- (1-PMIN)/(PMAX-PMIN)
  bmap <- CountyChoropleth$new(baseline_map %>% rename(value = mean_baseline_rel, region = fips))
  bmap$ggplot_polygon <- geom_polygon(aes(fill = value), color = NA, linewidth = 0.01)
  bmap$set_num_colors(1)
  bmap$ggplot_scale <- scale_fill_gradientn(colors = met.brewer("Hiroshige"),
                                            limits = c(PMIN, #min(baseline_map$mean_baseline_rel),
                                                       PMAX), #max(baseline_map$mean_baseline_rel)),
                                            name = "", # Mean relative\nnon-pandemic contact",
                                            guide = guide_colourbar(direction = "vertical", title.position = "left"),
                                            values = c(1, grad_mid, 0),
                                            breaks = c(0.5, 1, 1.5),
                                            labels = c("below", "mean", "above"))
  
  bmap$set_zoom(state_zoom) # no AK or HI
  bmap <- bmap$render() + theme(legend.position = c(0.9, 0.35),
                                legend.text = element_text(size = 16),
                                legend.title = element_text(hjust = 0.5, vjust = 1, size = 20),
                                legend.title.align = 0.5,
                                plot.margin=grid::unit(c(0,0,0,0), "mm"),
                                plot.caption = element_text(hjust = 0.5, size = 20, vjust = 5)) +
    labs(caption = "Mean relative non-pandemic contact" )
  #print(bmap)
  
  # grad_mid <- (midpoint-min(baseline_map$mean_pandemic_rel))/
  #   (max(baseline_map$mean_pandemic_rel)-min(baseline_map$mean_pandemic_rel))
  grad_mid <- (1-0.45)/(1.5-0.45)
  pmap <- CountyChoropleth$new(baseline_map %>% rename(value = mean_pandemic_rel, region = fips))
  pmap$ggplot_polygon <- geom_polygon(aes(fill = value), color = NA, linewidth = 0.01)
  pmap$set_num_colors(1)
  pmap$ggplot_scale <- scale_fill_gradientn(colors = met.brewer("OKeeffe1"),
                                            limits = c(0.45, 1.5),
                                            name = "", #Mean relative\npandemic contact",
                                            guide = guide_colourbar(direction = "vertical", title.position = "left"),
                                            values = c(1, grad_mid, 0),
                                            breaks = c(0.5, 1, 1.5),
                                            labels = c("below", "mean", "above"))
  
  pmap$set_zoom(state_zoom) # no AK or HI
  pmap <- pmap$render() + theme(legend.position = c(0.9, 0.35),
                                #legend.position =  c(0.2, 0.075), #c(0.92, 0.35), # horiz
                                legend.text = element_text(size = 16),
                                legend.title = element_text(hjust = 0.5, vjust = 1, size = 20),
                                #legend.key.width = unit(dev.size()[1]/15, "inches"),
                                plot.margin=grid::unit(c(0,0,0,0), "mm"),
                                plot.caption = element_text(hjust = 0.5, size = 20, vjust = 5)) +
    labs(caption = "Mean relative pandemic contact" )
  #print(pmap)
  
  
  #### URBAN RURAL BASELINE VS PANDEMIC ####

  ### trying with higher sampled counties ###
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
  levels(ur_plot_data3$setting) <- c("Pandemic", "Pre-pandemic no mob", "Non-pandemic")
  
  ann_text <- data.frame(x = c(1, 6), y = URY,
                         setting = factor("Non-pandemic",
                                          levels = c("Pandemic", "Pre-pandemic no mob", "Non-pandemic")),
                         label = c("Urban", "Rural"))
  data.segm <- data.frame(x=2,y=URY,xend=5,yend=URY,
                          setting = factor("Non-pandemic",
                                           levels = c("Pandemic", "Pre-pandemic no mob", "Non-pandemic")))
  
  (ur_fig <- ur_plot_data3 %>%
      left_join(urb_rur_codes) %>%
      
      filter(setting != "Pre-pandemic no mob") %>%
      ggplot(aes(x = ur_code, y = mean_contact)) +
      geom_jitter(aes(col = ur_code), alpha = 0.5) +
      geom_boxplot(fill = NA, outlier.shape = NA) +
      facet_wrap(~setting, nrow = 2) +
      labs(y = "Mean contact", x = "NCHS Urban-Rural Class", col = "NCHS class") +
      theme_bw() +
      guides(colour = guide_legend(override.aes = list(alpha = 1), nrow = 1, position = "inside")) +
      scale_color_met_d(name = "Derain", direction = -1) +
      theme(legend.position.inside = c(0.5, 0.96),
            legend.title.align = 0.5,
            legend.background = element_rect(fill = "transparent"),
            axis.text = element_text(size = 14),
            axis.title = element_text(size = 16),
            strip.text = element_text(size = 16)) +
      geom_text(data = ann_text,
                aes(x = x, y = y, label = label), size = 6) +
      geom_segment(data = data.segm, aes(x=x,y=y,yend=yend,xend=xend),
                   arrow = arrow(length = unit(0.03, "npc"), ends = "both")))
  
  ur_fig_half <- ur_plot_data3 %>%
    left_join(urb_rur_codes) %>%
    filter(setting == "Non-pandemic") %>%
    ggplot(aes(x = ur_code, y = mean_contact)) +
    geom_jitter(aes(col = ur_code), alpha = 0.5) +
    geom_boxplot(fill = NA, outlier.shape = NA) +
    facet_wrap(~setting, nrow = 2) +
    labs(y = "Mean contact", x = "NCHS Urban-Rural Class", col = "NCHS class") +
    theme_bw() +
    scale_color_met_d(name = "Derain", direction = -1) +
    scale_y_continuous(breaks = pretty_breaks(n = 5))
  theme(legend.position = "none",
        axis.text = element_text(size = 14),
        axis.title = element_text(size = 16),
        strip.text = element_text(size = 16))
  half_fig <- ggarrange(bmap, ur_fig_half, nrow = 1)
  
  if(pdf_file){
    maps <- ggarrange(pmap, bmap, ncol = 1, labels = "AUTO", font.label=list(size=20))
    ggarrange(maps, ur_fig, ncol = 2, widths = c(4, 2), labels = c("", "C"), font.label=list(size=20))
    
    ggsave(paste0("figures/supp/fig2", add_filename, ".pdf"), #device = cairo_ps, 
           height = 8, width = 12)
  }
  
  if(pdf_file){
    return(list(no_pooling, mob_scaled_baseline_data))
  }else{
    return(list(fig2b_fin, half_fig))
  }
  
}

#### RUN MODELS ####

model_list <- list(
  # 1
  contact_fit ~ 1 + national_cases_roll4 * percent_vaxxed + sum_county_measures_shift + StringencyIndex_Average_roll3_shiftmin | fips,
  # 2
  contact_fit ~ 1 + national_cases_roll4 * percent_vaxxed | fips,
  # 3
  contact_fit ~ 1 + county_cases_roll4 + national_cases_roll4 * percent_vaxxed + sum_county_measures_shift + StringencyIndex_Average_roll3_shiftmin | fips,
  # 4
  contact_fit ~ 1 + national_cases_roll4 + percent_vaxxed + sum_county_measures_shift + StringencyIndex_Average_roll3_shiftmin | fips,
  # 5
  contact_fit ~ 1 + national_cases_roll4 * percent_vaxxed + StringencyIndex_Average_roll3 | fips,
  # 6 
  contact_fit ~ 1 + national_cases_roll4 | fips
)


add_filename <- c("", "_nopolicy", "_countyinc", "_nointeract", "_statepol", "_natonly")
# model_formula, model_data, 
# add_filename, pdf_file = T, Y = 12,
# is_interaction = T
main_model <- baseline_models_function(model_formula = model_list[[1]],
                                       combine_data = combine_data,
                                       spatiotemporal_fits = spatiotemporal_fits,
                                       add_filename = add_filename[[1]],
                                       calc_vif = T) 

nopol_model <- baseline_models_function(model_formula = model_list[[2]],
                                        combine_data = combine_data,
                                        spatiotemporal_fits = spatiotemporal_fits,
                                        add_filename = add_filename[[2]],
                                        pdf_file = F, YN = 11,
                                        calc_vif = T)  

county_model <- baseline_models_function(model_formula = model_list[[3]],
                                         combine_data = combine_data,
                                         spatiotemporal_fits = spatiotemporal_fits,
                                         add_filename = add_filename[[3]],
                                         pdf_file = F, YN = 11,
                                         calc_vif = T)  

noint_model <- baseline_models_function(model_formula = model_list[[4]],
                                        combine_data = combine_data,
                                        spatiotemporal_fits = spatiotemporal_fits,
                                        add_filename = add_filename[[4]],
                                        pdf_file = F, YN = 11, is_interaction = F,
                                        calc_vif = T)  

ggarrange(nopol_model[[1]], county_model[[1]], noint_model[[1]], nrow = 3, labels = "AUTO")
ggsave("figures/supp/fig1-nopol-county-noint.pdf", height = 12, width = 10)

ggarrange(nopol_model[[2]], county_model[[2]], noint_model[[2]], nrow = 3, labels = "AUTO")
ggsave("figures/supp/fig2-nopol-county-noint.pdf", height = 12, width = 12)

#### state policy only for coefficients for prediction ####
# ended up not using these and using national case model only (model)
# statepol_model <- baseline_models_function(model_formula = model_list[[5]],
#                                            combine_data = combine_data,
#                                            spatiotemporal_fits = spatiotemporal_fits,
#                                            add_filename = add_filename[[5]],
#                                            calc_vif = T) 
# statepol_coefs <- statepol_model[[2]] %>% 
#   select(fips, contains("slope"), intercept) %>% 
#   distinct()
# write_csv(statepol_coefs, "data/output/coefs_for_prediction.csv")
# 
# statepol_model_obj <- statepol_model[[1]]
# save(statepol_model_obj, file = "data/output/statepol_model_for_prediction.RData")

natonly_model <- baseline_models_function(model_formula = model_list[[6]],
                                          combine_data = combine_data,
                                          spatiotemporal_fits = spatiotemporal_fits,
                                          add_filename = add_filename[[6]],
                                          calc_vif = F) 
natonly_coefs <- natonly_model[[2]] %>% 
  select(fips, contains("slope"), intercept) %>% 
  distinct()
write_csv(natonly_coefs, "data/output/coefs_for_prediction_natonly.csv")

#### now add 36, 108 sensitivity ####
spatiotemporal_fits36 <- read_csv("spatiotemporal/normal_gamma2_36trunc_m1/fitted_predictions.csv",
                                  col_types = "ddiiddddddddiiDccDccf") %>% ungroup()

spatiotemporal_fits108 <- read_csv("spatiotemporal/normal_gamma2_108trunc_m1/fitted_predictions.csv",
                                   col_types = "ddiiddddddddiiDccDccf") %>% ungroup()

combine_data36 <- 
  spatiotemporal_fits36 %>% dplyr::select(fit, se.fit, fips, week, month, samp_size, non_hh_contacts) %>% 
  left_join(all_cases %>% dplyr::select(-POPESTIMATE2020), by = c("fips", "week")) %>% 
  left_join(urb_rur_codes, by = "fips") %>% 
  left_join(df.fips, by = "fips") %>% 
  left_join(hhs_regions, by = "fips") %>% 
  left_join(policy_data, by = c("fips", "week")) %>% # do we need to fill in NAs with 0? 
  left_join(dataverse_vax_data, by = c("fips", "week")) %>% 
  left_join(mask_data, by = c("fips", "week")) %>% 
  left_join(oxdata, by = c("week", "state")) %>% 
  left_join(recent_pop_data, by = "fips") %>% 
  filter(! fips %in% c(36005, 36047, 36061, 36081, 36085, 8111)) %>% # CO no change in pct vax
  filter(state != "AK") %>% 
  #filter(! fips%in% fips_missing_data) %>% # need to fix 9/4/23 
  rename(contact_fit = fit) %>% 
  mutate(fips = as.factor(fips),
         state = as.factor(state)) %>% 
  filter(week <= ymd("2021-05-01"),
         week > ymd("2020-06-01")) %>% 
  mutate(percent_vaxxed = ifelse(is.na(percent_vaxxed), 0, percent_vaxxed))


combine_data108 <- 
  spatiotemporal_fits108 %>% dplyr::select(fit, se.fit, fips, week, month, samp_size, non_hh_contacts) %>% 
  left_join(all_cases %>% dplyr::select(-POPESTIMATE2020), by = c("fips", "week")) %>% 
  left_join(urb_rur_codes, by = "fips") %>% 
  left_join(df.fips, by = "fips") %>% 
  left_join(hhs_regions, by = "fips") %>% 
  left_join(policy_data, by = c("fips", "week")) %>% # do we need to fill in NAs with 0? 
  left_join(dataverse_vax_data, by = c("fips", "week")) %>% 
  left_join(mask_data, by = c("fips", "week")) %>% 
  left_join(oxdata, by = c("week", "state")) %>% 
  left_join(recent_pop_data, by = "fips") %>% 
  filter(! fips %in% c(36005, 36047, 36061, 36081, 36085, 8111)) %>% # CO no change in pct vax
  filter(state != "AK") %>% 
  #filter(! fips%in% fips_missing_data) %>% # need to fix 9/4/23 
  rename(contact_fit = fit) %>% 
  mutate(fips = as.factor(fips),
         state = as.factor(state)) %>% 
  filter(week <= ymd("2021-05-01"),
         week > ymd("2020-06-01")) %>% 
  mutate(percent_vaxxed = ifelse(is.na(percent_vaxxed), 0, percent_vaxxed))

main36_model <- baseline_models_function(model_formula = model_list[[1]],
                                         combine_data = combine_data36,
                                         spatiotemporal_fits = spatiotemporal_fits36,
                                         add_filename = "_36trunc",
                                         YN = 10, YP = 7, URY = 3) 

main108_model <- baseline_models_function(model_formula = model_list[[1]],
                                          combine_data = combine_data108,
                                          spatiotemporal_fits = spatiotemporal_fits108,
                                          add_filename = "_108trunc", YN = 14) 

### jun 2021 sensitivity will be harder


#### DIAGNOSTICS for main model ####
main_data <- main_model[[2]]

## diagnostics
main_data %>% ggplot(aes(x = pred_lm, y = residual)) + 
  geom_point(alpha = 0.05, col = "royalblue") + 
  geom_hline(yintercept = 0) +
  theme_bw() +
  labs(x = "fitted")
ggsave("figures/supp/regression-diags-resid-vs-fit.pdf", height = 5, width = 8)
main_data %>% 
  filter(fips %in% sample(unique(main_data$fips), 36)) %>% 
  ggplot(aes(x = pred_lm, y = residual)) + 
  geom_point(alpha = 1, col = "royalblue") + 
  geom_hline(yintercept = 0) +
  theme_bw() +
  labs(x = "fitted") +
  facet_wrap(~fips)
ggsave("figures/supp/regression-diags-resid-vs-fit-sample.pdf", height = 10, width = 16)
main_data %>% ggplot(aes(x = state_cases_roll4, y = residual)) + 
  geom_point(alpha = 0.05, col = "royalblue") + 
  geom_hline(yintercept = 0) +
  theme_bw() +
  labs(x = "state cases")
ggsave("figures/supp/regression-diags-resid-vs-state.pdf", height = 5, width = 8)

sample_fips <- sample(unique(main_data$fips), 16)
model_data_sub <- main_data %>% filter(fips %in% sample_fips)
no_pooling_sub <- lmList(model_list[[1]], model_data_sub)

pdf("figures/supp/regression-diags-qq-sample.pdf", height = 10, width = 16)
par(mfrow = c(4, 4))
invisible(lapply(no_pooling_sub,plot,which=2))
dev.off()

#### model assumptions plots ####

combine_data %>% ggplot(aes(x = national_cases_roll4, y = contact_fit)) +
  geom_point(alpha = 0.05) +
  geom_smooth(method = "lm") +
  scale_x_log10() +
  theme_bw() +
  labs(y = "Contact", x = "National incident cases\n(4 week rolling average)")
ggsave("figures/supp/disease-contact-correlation-nat.pdf", height = 5, width = 8)

combine_data %>% 
  filter(fips %in% sample(unique(combine_data$fips), 25)) %>% 
  ggplot(aes(x = county_cases_roll4, y = contact_fit)) +
  geom_point() +
  geom_smooth(method = "lm") +
  theme_bw() +
  labs(y = "Contact", x = "County incident cases\n(4 week rolling average)") +
  facet_wrap(~fips, scales = "free")
ggsave("figures/supp/disease-contact-correlation-sample-counties.pdf", height = 8, width = 14)


#### model intermediates plots ####
supp_df <- main_data %>% 
  filter(fips %in% fips_I_want$fips) %>% 
  pivot_longer(cols = c(pred_lm, contact_fit), 
               names_to = "source",
               values_to = "num_contacts") %>% 
  mutate(fips = as.factor(fips),
         source = as.factor(source))
levels(supp_df$fips) <- fips_I_want$full_name
levels(supp_df$source) <- c("smoothed data", "model prediction")

supp_df %>% 
  ggplot(aes(x = week, y = num_contacts, 
             group = interaction(fips, source), col = source)) +
  geom_line(lty = 1.25, alpha = 0.75, linewidth = 1.5) +
  scale_x_date(breaks = seq(as.Date("2020-10-01"), as.Date("2021-04-30"),
                            by = "3 month"),
               labels = c("Oct 20", "Jan 21",
                          "Apr 21"),
               minor_breaks = "1 month") +
  theme_bw() +
  labs(y = "Number of contacts") +
  theme(axis.text = element_text(size = 16),# angle = 10, hjust = 1),
        axis.text.x = element_text(vjust = 0),
        plot.subtitle=element_text(size=16, hjust=0.5),
        strip.text = element_text(size = 16),
        axis.title = element_text(size = 18),
        legend.text = element_text(size = 14),
        legend.title = element_text(size = 14),
        legend.position = "right") +
  guides(color = guide_legend(ncol= 1)) +
  facet_wrap(~fips, scales = "free") +
  scale_y_continuous(breaks = breaks_pretty(n = 4)) +
  scale_color_met_d("Isfahan2", direction = -1)
ggsave("figures/supp/fit-roll4-stringencyshift.pdf", height = 8, width = 14)


#### june 2021 attempt ####
policy_data_june <- read_csv("data/input/covs_updated_for_inla_6_10.csv", 
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
  filter(week > ymd("2020-06-01")) %>% 
  group_by(fips) %>% 
  mutate(min_sum_county_measures = min(sum_county_measures)) %>% 
  ungroup() %>% 
  mutate(sum_county_measures_shift = sum_county_measures - min_sum_county_measures)

state_min_strin_june <- oxdata_week %>%
  filter(week >= ymd("2020-06-01")) %>% 
  group_by(state) %>%
  summarise(min_strin_2022 = min(StringencyIndex_Average_roll3, na.rm = T))

state_mean_strin_study_pd_june <- oxdata_week %>%
  filter(week >= ymd("2020-06-01"), week < ymd("2021-05-01")) %>%
  group_by(state) %>%
  summarise(tail_strin = tail(StringencyIndex_Average_roll3, 1),
            min_strin = min(StringencyIndex_Average_roll3, na.rm = T))

oxdata_june <- oxdata_week %>% 
  filter(week >= ymd("2020-06-01"), week <= ymd("2021-05-01")) %>% 
  left_join(state_mean_strin_study_pd_june) %>% 
  left_join(state_min_strin_june) %>% 
  mutate(StringencyIndex_Average_roll3_shifttail = StringencyIndex_Average_roll3 - tail_strin,
         StringencyIndex_Average_roll3_shiftmin = StringencyIndex_Average_roll3 - min_strin,
         StringencyIndex_Average_roll3_shift2022 = StringencyIndex_Average_roll3 - min_strin_2022) %>% 
  group_by(state) %>% 
  mutate(StringencyIndex_Average_z = c(scale(StringencyIndex_Average))) %>% 
  ungroup() %>% 
  group_by(state) %>% 
  mutate(StringencyIndex_Average_z_mincentered = c(scale(StringencyIndex_Average_roll3_shiftmin, center = F)))

combine_data_june <- 
  spatiotemporal_fits %>% dplyr::select(fit, se.fit, fips, week, month, samp_size, non_hh_contacts) %>% 
  left_join(all_cases %>% dplyr::select(-POPESTIMATE2020), by = c("fips", "week")) %>% 
  left_join(urb_rur_codes, by = "fips") %>% 
  left_join(df.fips, by = "fips") %>% 
  left_join(hhs_regions, by = "fips") %>% 
  left_join(policy_data_june, by = c("fips", "week")) %>% # do we need to fill in NAs with 0? 
  left_join(dataverse_vax_data, by = c("fips", "week")) %>% 
  left_join(oxdata_june, by = c("week", "state")) %>% 
  left_join(recent_pop_data, by = "fips") %>% 
  filter(! fips %in% c(36005, 36047, 36061, 36081, 36085, 8111)) %>% # CO no change in pct vax
  filter(state != "AK") %>% 
  #filter(! fips%in% fips_missing_data) %>% # need to fix 9/4/23 
  rename(contact_fit = fit) %>% 
  mutate(fips = as.factor(fips),
         state = as.factor(state)) %>% 
  filter(week <= ymd("2021-05-01"),
         week > ymd("2020-06-01")) %>% 
  mutate(percent_vaxxed = ifelse(is.na(percent_vaxxed), 0, percent_vaxxed))

june_model <- baseline_models_function(model_formula = model_list[[1]],
                                       combine_data = combine_data_june,
                                       spatiotemporal_fits = spatiotemporal_fits,
                                       add_filename = "_june",
                                       calc_vif = F, startdate = "2020-06-01", 
                                       YN = 15, fig1_wid = 12,
                                       pdf_file = F) 
ggarrange(june_model[[1]], june_model[[2]], nrow = 2, labels  = "AUTO")
ggsave("figures/supp/june.pdf", height = 8, width = 12)


#### anova on urban-rural ####
aov_test <- aov(scale_baseline ~ ur_code, data = main_data)
summary(aov_test)
# given that significant differences at 0.05 level, use multiple comparisons to tease out
TukeyHSD(aov_test)

pairwise.t.test(main_data$scale_baseline, main_data$ur_code, # additional alternative
                p.adjust.method = "bonf")
# check homogeneity of variance
plot(aov_test, 1)
library(car)
leveneTest(scale_baseline ~ ur_code, data = main_data) # if significant, variance is different, therefore can't use anova
# anova with no assumption of equal variances
# stats::oneway.test(slope_national ~ age, data = age_spec) # not sure why nt working
pairwise.t.test(main_data$scale_baseline, main_data$ur_code,
                p.adjust.method = "bonf", pool.sd = FALSE)
# check normality assumption
plot(aov_test, 2)
aov_residuals <- residuals(object = aov_test)
shapiro.test(x = aov_residuals) # Shapiro-Wilk test, if significant then sample hasn't been generated from normal distribution

# use kruskal wallis since assumptions not met
kruskal.test(scale_baseline ~ ur_code, data = main_data)
pairwise.wilcox.test(main_data$scale_baseline, main_data$ur_code, p.adjust.method = "bonf")


#### try eps fig 1 ####
main_data <- main_model[[2]]
plot_df <- main_data %>%
  mutate(fit_seplus = contact_fit + se.fit,
         fit_seminus = contact_fit - se.fit,
         baseline_seplus = (baseline + intercept_se) * phi,
         baseline_seminus = (baseline - intercept_se) * phi) %>%
  dplyr::select(fips, week, samp_size, ur_code, contact_fit, baseline, scale_baseline,
                fit_seplus, fit_seminus, baseline_seplus, baseline_seminus) %>%
  ungroup() %>%
  filter(fips %in% fips_I_want_shorter$fips) %>%
  rename(fit_contact = contact_fit, baseline_contact = scale_baseline) %>%
  pivot_longer(cols = c(fit_contact, baseline_contact, fit_seplus, fit_seminus,
                        baseline_seplus, baseline_seminus),
               names_to = c("scenario", "metric"),
               names_sep = "_",
               values_to = c("value")) %>%
  pivot_wider(names_from = "metric", values_from = "value") %>%
  mutate(fips = as.factor(fips),
         scenario = as.factor(scenario))
levels(plot_df$fips) <- fips_I_want_shorter$full_name
plot_df$fips <- factor(plot_df$fips, levels = c("Maricopa, AZ", "Miami-Dade, FL",
                                                "King, WA", "Travis, TX", "Essex, NJ",
                                                "Hillsborough, NH", "Anoka, MN", "Calhoun, AL",
                                                "Buffalo, NE", "Flathead, MT"))
levels(plot_df$scenario) <- c("non-pandemic", "pandemic")

(plot_df %>%
    ggplot() +
    geom_line(aes(x = week, y = contact,
                  group = interaction(fips, scenario), col = scenario),
              linewidth = 1.5, alpha = 0.75) +
    geom_ribbon(aes(x = week, ymin = seminus, ymax = seplus,
                    group = interaction(fips, scenario), fill = scenario), col = NA, alpha = 0.25) +
    scale_x_date(breaks = seq(as.Date("2020-10-01"), as.Date("2021-04-30"),
                              by = "3 month"),
                 labels = c("Oct", "Jan 2021", "Apr"),
                 minor_breaks = "1 month") +
    theme_bw() +
    labs(y = "Mean contacts") +
    theme(axis.text = element_text(size = 16),# angle = 10, hjust = 1),
          plot.subtitle=element_text(size=16, hjust=0.5),
          strip.text = element_text(size = 16),
          axis.title = element_text(size = 18),
          legend.text = element_text(size = 14),
          legend.title = element_blank(), #text(size = 14),
          legend.position = "none") +
    guides(color = guide_legend(ncol= 2)) +
    facet_wrap(~fips, nrow = 2) +
    scale_y_continuous(breaks = breaks_pretty(n = 4)) +
    scale_color_manual(values = c("#3F3E66", "#1E7D79")) +
    scale_fill_manual(values = c("#3F3E66", "#1E7D79"))  -> fig2b)

dat_text <- data.frame(label = c("pandemic", "", "", "", "", "", "", "", "", ""),
                       label2 = c("non-pandemic", "", "", "", "", "", "", "", "", ""),
                       fips = c(4013, 1015, 12086, 27003, 30029, 31019, 33011, 34013, 48453, 53033)) %>%
  left_join(fips_I_want_shorter) %>%
  mutate(fips = as.factor(full_name))

startX = "2020-12-10"
YN = 12; YP = 5
fig2b +
  geom_text(data = dat_text,
            mapping = aes(x = ymd(startX), y = YP, label = label),
            color = "#1E7D79",
            size = 6,
            fontface = "bold") +
  geom_text(data = dat_text,
            mapping = aes(x = ymd(startX), y = YN, label = label2),
            color = "#3F3E66",
            size = 6,
            hjust = 0.33,
            fontface = "bold") -> fig2b_fin

library(patchwork)
fig2a_stuff <- fig2a_function(combine_data)
casecurve <- fig2a_stuff[[1]]
fig2_tog <- ggarrange(fig2a_stuff[[3]], fig2b_fin, ncol = 1, heights = c(5, 6),
                      labels = "AUTO", font.label = list(size = 28))
fig2_tog + inset_element(
  casecurve, 
  left = 0.053, 
  right = 0.847, 
  bottom = 0.61, 
  top = 0.75
)
#width = 0.795, height = 0.14, x = 0.452, y = 0.68
ggsave("figures/for-paper/fig1.eps", height = 8, width = 10, device = cairo_ps)
