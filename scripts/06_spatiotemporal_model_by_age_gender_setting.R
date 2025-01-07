{
  library(tidyverse)
  library(mgcv)
  library(glue)
  library(lubridate)
  library(MetBrewer)
  library(choroplethr)
  library(choroplethrMaps)
  library(readxl)
  library(cowplot)
  library(grid)
  library(gridExtra)
  library(viridis)
}

# --------------- #
#### Load data ####
# --------------- #

df.fips <- read_csv('data/input/state_and_county_fips_master.csv') #%>% 
# mutate(fips = ifelse(fips == 2270, 2158,
#                      ifelse(fips == 46113, 46102,
#                             fips)))

urb_rur_codes <- read_excel("data/input/NCHSURCodes2013.xlsx") %>% rename(fips = `FIPS code`) %>% 
  mutate(`CBSA 2012 pop` = as.integer(`CBSA 2012 pop`),
         `County 2012 pop` = as.integer(`County 2012 pop`)) %>% 
  select(fips, `State Abr.`, `County name`, `CBSA title`, `2013 code`, 
         `County 2012 pop`) %>% 
  rename(state = `State Abr.`, county = `County name`, area = `CBSA title`,
         ur_code = `2013 code`, population = `County 2012 pop`) %>%  # can ignore warnings
  mutate(ur_code = as.factor(ur_code))

hhs_regions <- read_csv("data/input/HHS_regions.csv", col_types = "if") %>% 
  rename(fips = node, hhs_region = modularity_class) %>% 
  left_join(df.fips)


# TODO: COMMENT OUT SECTIONS NOT CORRESPONDING TO ATTRIBUTE OF INTEREST

county_week_by_age4 <- read_csv("data/group_means_rake/contact_by_age4_week_county_trunc72.csv",
                               col_types = "Difddii") %>%
  mutate(nonhh_contacts_rd = round(non_hh_contacts)) %>%
  filter(week > ymd("2020-04-30"), week < ymd("2021-05-01"))

# determine which counties have 10 or more observations per week per age group for regression
# NOTE: this is a more limited time period than for general spatiotemporal model

for(age_level in c(1, 2, 3, 4)){ #}, 5, 6, 7)){
  filtered_fips <- county_week_by_age4 %>%
    filter(age4 == age_level) %>%
    ungroup()%>%
    filter(week >= ymd("2020-10-01"),
           week <= ymd("2021-04-25")) %>%
    mutate(enough = ifelse(samp_size >= 5, 1, 0)) %>% # is NA for imputed county-weeks
    group_by(fips) %>%
    summarise(sum_samp = sum(enough, na.rm = T)) %>%
    filter(sum_samp == 30) %>% # 30 weeks
    pull(fips)

  if(age_level == 1){
    age1_fips <- filtered_fips
  }else if (age_level == 2){
    age2_fips <- filtered_fips
  }else if(age_level == 3){
    age3_fips <- filtered_fips
  }else if(age_level == 4){
    age4_fips <- filtered_fips
  }
  # else if (age_level == 5){
  #   age5_fips <- filtered_fips
  # }else if(age_level == 6){
  #   age6_fips <- filtered_fips
  # }else if(age_level == 7){
  #   age7_fips <- filtered_fips
  # }
}

large_enough_age_fips <-
  intersect(age1_fips,
            intersect(age2_fips,
                      intersect(age3_fips, age4_fips)))
                                # intersect(age4_fips,
                                #           intersect(age5_fips,
                                #                     intersect(age6_fips, age7_fips))))))

states_to_model <- data.frame(fips = large_enough_age_fips) %>%
  left_join(df.fips) %>%
  pull(state) %>%
  unique()


# county_week_by_gender <- read_csv("data/group_means_rake/contact_by_gender_week_county_trunc72.csv",
#                                col_types = "Dfiddii") %>%
#   mutate(nonhh_contacts_rd = round(non_hh_contacts)) %>%
#   filter(week > ymd("2020-04-30"), week < ymd("2021-05-01"))
# 
# for(gender_level in c(1, 2)){
#   filtered_fips <- county_week_by_gender %>%
#     filter(gender == gender_level) %>%
#     ungroup()%>%
#     filter(week >= ymd("2020-10-01"),
#            week <= ymd("2021-04-25")) %>%
#     mutate(enough = ifelse(samp_size >= 5, 1, 0)) %>%
#     group_by(fips) %>%
#     summarise(sum_samp = sum(enough, na.rm = T)) %>%
#     filter(sum_samp == 30) %>% # 30 weeks
#     pull(fips)
# 
#   if(gender_level == 1){
#     gender1_fips <- filtered_fips
#   }else if (gender_level == 2){
#     gender2_fips <- filtered_fips
#   }
# }
# 
# large_enough_gender_fips <- intersect(gender1_fips, gender2_fips)
# 
# states_to_model <- data.frame(fips = large_enough_gender_fips) %>%
#   left_join(df.fips) %>%
#   pull(state) %>%
#   unique()


# county_week_by_setting <- read_csv("data/group_means_rake/contact_by_setting_week_county_trunc72.csv",
#                                    col_types = "Difdi") %>%
#   filter(week > ymd("2020-04-30"), week < ymd("2021-05-01")) %>%
#   filter(!is.na(non_hh_contacts))
# 
# for(setting_level in c("social", "other", "shopping", "work")){
#   filtered_fips <- county_week_by_setting %>%
#     filter(setting == setting_level) %>%
#     ungroup()%>%
#     filter(week >= ymd("2020-10-01"),
#            week <= ymd("2021-04-25")) %>%
#     mutate(enough = ifelse(samp_size >= 10, 1, 0)) %>%
#     group_by(fips) %>%
#     summarise(sum_samp = sum(enough, na.rm = T)) %>%
#     filter(sum_samp == 30) %>% # 30 weeks
#     pull(fips)
#   
#   if(setting_level == "social"){
#     social_fips <- filtered_fips
#   }else if (setting_level == "other"){
#     other_fips <- filtered_fips
#   }else if (setting_level == "shopping"){
#     shopping_fips <- filtered_fips
#   }else if (setting_level == "work"){
#     work_fips <- filtered_fips
#   }
# }
# # they should all be the same since one individual has the different categories of contacts
# # these also will be all the same fips as urban/rural since it's really just county based, not setting
# large_enough_setting_fips <- intersect(social_fips,
#                                        intersect(other_fips,
#                                                  intersect(shopping_fips, work_fips)))
# 
# states_to_model <- data.frame(fips = large_enough_setting_fips) %>%
#   left_join(df.fips) %>%
#   pull(state) %>%
#   unique()


# ------------------------ #
#### RUN THE MAIN MODEL ####
# ------------------------ #

folder_name <- "age4_72trunc_m1" # TODO: CHANGE FOLDER ACCORDING TO TRUNCATION AND CATEGORY
require(gratia)

for(age_level in c(1, 2, 3, 4)){
  folder_sublevel = paste0("age", age_level)
  contact_data <- county_week_by_age4 %>%
    filter(age4 == age_level)

# for(gender_level in c(1, 2)){
#   
#   folder_sublevel <- paste0("gender", gender_level)
#   contact_data <- county_week_by_gender %>%
#     filter(gender == gender_level)
  
  # for(setting_level in c("social", "other", "shopping", "work")){
  #   folder_sublevel <- setting_level
  #   contact_data <- county_week_by_setting %>%
  #     filter(setting == setting_level)
  
  to_model <- contact_data %>% ungroup() %>% 
    mutate(week_rank = as.integer(as.numeric(week)/7)) %>% # create a numeric dummy for week to feed to mgcv
    mutate(week_rank = week_rank - min(week_rank) + 1) %>% # set min val of dummy to 1
    ungroup() %>% 
    left_join(df.fips) %>% 
    filter(!is.na(state),
           state %in% states_to_model)
  
  states <- unique(to_model$state)
  
  n_week <- to_model %>% pull(week_rank) %>% unique() %>% length()
  
  all_predictions <- data.frame()
  
  # fit the model for each state separately
  # since we are fitting means, there's a bit less concern about heavy tails
  #   though we still saw these tails in the mean outliers, so should we use a different family?
  
  for(state.fit in states){
    
    print(state.fit)
    
    # extract data for that state only
    df.fit <- to_model %>% 
      filter(state == state.fit) %>% 
      mutate(id = row_number(),
             fips = as.factor(fips))
    
    
    if (state.fit == 'DC'){
      # bam is gam for very large datasets
      fit <- bam(non_hh_contacts ~ 1 + s(week_rank, bs = 'cs', k = 10), # 1 not necessary, cs = shrinkage version of cubic regression spline
                 data = df.fit, # shrinkage version penalizes linear/constant splines as well to allow terms to be dropped from model effectively
                 weights = samp_size, # weighted by number of observations, weight of 2 effectively means made same obs twice
                 method = 'fREML', # fast REML default with bam since lots of data
                 # don't include gamma here or will get linear fit
                 control = list(trace = TRUE)) # turns on diagnostic output
      
    }
    else{
      
      if (length(unique(df.fit$fips)) > 10){ 
        # num counties within state, but still just fitting one model per state, but fips is a covariate
        # model might be separate for each state, but still individual predictions for each fips
        
        fit <- bam(non_hh_contacts ~ s(week_rank, k = 30, m = 2) + s(week_rank, fips, bs = 'fs', k = 15, m = 1), # factor-smooth interaction, 
                   #fips is factor, so diff spline per fips (this approach closer to random effects than if did by factor level, SHARED SMOOTHNESS PARAM)
                   data = df.fit,
                   weights = samp_size,
                   method = 'fREML',
                   control = list(trace = TRUE),
                   gamma = 2, # increases penalty to force smoother model
                   discrete = T) # "discretize covariates for storage and efficiency reasons. If discrete is TRUE, 
        # a number or a vector of numbers for each smoother term, then discretization happens"
        # discrete is only difference maybe just cause so many factors have to help speed up
      }else{
        # fit the model
        fit <- bam(non_hh_contacts ~ s(week_rank, k = 30, m = 2) + s(week_rank, fips, bs = 'fs', k = 15, m = 1),
                   data = df.fit, # m = 2 means order of penalty, 2 is for normal cubic spline penalty with 2nd derivatives
                   weights = samp_size,
                   method = 'fREML', # fast reml
                   gamma = 2, # increases penalty to force smoother model
                   control = list(trace = TRUE))
        
      }
    } # this makes a prediction even when I have no data in that fips
    
    # extract the smoothed values, imputing for those missing from the data
    newd <- predict(fit, 
                    newdata = df.fit %>% complete(fips, week_rank), # predict fips weeks with missing data
                    se.fit = TRUE,
                    type = "response") %>% # pull out fit and se.fit individually, 
      #then do joins to create tibble 
      as_tibble() %>%
      mutate(id = row_number()) %>% 
      left_join(df.fit %>% complete(fips, week_rank) %>% 
                  mutate(id = row_number())) %>% # add model input data, same num rows once use complete
      select(-id, -week) %>%
      left_join(df.fit %>% # not sure this is necessary?
                  select(week, week_rank) %>%
                  unique()) %>%
      select(-name, -state) %>%
      left_join(df.fips %>% mutate(fips = as.factor(fips))) %>% # need to have all fips factor levels here for later joining with other states I think
      mutate(fit = as.numeric(fit), se.fit = as.numeric(se.fit)) # JT addition bc these two cols are arrays
    
    all_predictions <- all_predictions %>% bind_rows(newd)
    
    write_csv(newd, glue('data/output/', folder_name, '/state_fits/posterior_draws-contact_no_hh-', state.fit, '-', folder_sublevel, '.csv'))
    
    saveRDS(fit, glue('data/output/', folder_name,'/state_fits/posterior_draws-contact_model_fit-', state.fit, '-', folder_sublevel, '.rds'))
  }
  
  ## save fit checks ##
  my_log <- file(paste0("data/output/", folder_name, "/checking_model_fits_", folder_sublevel, ".txt")) # File name of output log
  sink(my_log, append = TRUE, type = "output") # Writing console output to log file
  sink(my_log, append = TRUE, type = "message")
  for(state.fit in states){
    print(state.fit)
    par(mfrow = c(2, 2))
    model <- readRDS(paste0("data/output/", folder_name, "/state_fits/posterior_draws-contact_model_fit-", state.fit, '-', folder_sublevel, ".rds"))
    gam.check(model)
    appraise(model, method = "simulate", n_simulate = 500) # can't save gam.check plots but can save these
    ggsave(paste0("data/output/", folder_name, "/gam_check_plots/", state.fit,'-', folder_sublevel, ".pdf"), height = 6, width = 12)
  }
  
  closeAllConnections()
  
  all_predictions <- all_predictions %>%
    mutate(week = ymd(week),
           month = ymd(floor_date(week, unit = "month")),
           fips = as.numeric(levels(fips))[as.integer(fips)]) %>%
    left_join(urb_rur_codes)
  write_csv(all_predictions, paste0("data/output/", folder_name, "/fitted_predictions_", folder_sublevel, ".csv"))
  
}

#### rewrite predictions with residual data ####
# 
for(age_level in c(1, 2, 3, 4)){
  folder_sublevel = paste0("age", age_level)
  contact_data <- county_week_by_age4 %>%
    filter(age4 == age_level)

# for(gender_level in c(1, 2)){
#   folder_sublevel <- paste0("gender", gender_level)
#   contact_data <- county_week_by_gender %>%
#     filter(gender == gender_level)

# for(setting_level in c("social", "other", "shopping", "work")){
#   folder_sublevel <- setting_level
#   contact_data <- county_week_by_setting %>%
#     filter(setting == setting_level)
  
  to_model <- contact_data %>% ungroup() %>% 
    mutate(week_rank = as.integer(as.numeric(week)/7)) %>% # create a numeric dummy for week to feed to mgcv
    mutate(week_rank = week_rank - min(week_rank) + 1) %>% # set min val of dummy to 1
    ungroup() %>% 
    left_join(df.fips) %>% 
    filter(!is.na(state),
           state %in% states_to_model)
  
  states <- unique(to_model$state)
  n_week <- to_model %>% pull(week_rank) %>% unique() %>% length()
  all_predictions <- data.frame()
  
  for(state.fit in states){
    
    print(state.fit)
    
    # extract data for that state only
    df.fit <- to_model %>% 
      filter(state == state.fit) %>% 
      mutate(id = row_number(),
             fips = as.factor(fips))
    
    fit <- readRDS(glue('data/output/', folder_name,'/state_fits/posterior_draws-contact_model_fit-', state.fit, '-', folder_sublevel, '.rds'))
    
    # extract the smoothed values, imputing for those missing from the data
    newd <- predict(fit, 
                    newdata = df.fit %>% complete(fips, week_rank), # predict fips weeks with missing data
                    se.fit = TRUE,
                    type = "response") %>% # pull out fit and se.fit individually, 
      #then do joins to create tibble 
      as_tibble() %>%
      mutate(id = row_number()) %>% 
      left_join(df.fit %>% complete(fips, week_rank) %>% 
                  mutate(id = row_number())) %>% # add model input data, same num rows once use complete
      select(-id, -week) %>%
      left_join(df.fit %>% # not sure this is necessary?
                  select(week, week_rank) %>%
                  unique()) %>%
      select(-name, -state) %>%
      left_join(df.fips %>% mutate(fips = as.factor(fips))) %>% # need to have all fips factor levels here for later joining with other states I think
      mutate(fit = as.numeric(fit), se.fit = as.numeric(se.fit)) # JT addition bc these two cols are arrays
    
    resid_to_join <- data.frame(residual = residuals(fit)) %>% 
      bind_cols(df.fit %>% select(week, fips)) %>% 
      left_join(df.fips %>% mutate(fips = as.factor(fips))) %>% 
      select(-name, -state)
    
    updates <- newd %>% left_join(resid_to_join, by = c("fips", "week"))
    
    all_predictions <- all_predictions %>% bind_rows(updates)
    
    write_csv(all_predictions, glue('data/output/', folder_name, '/state_fits_wresid/posterior_draws-contact_no_hh-', state.fit, '-', folder_sublevel, '.csv'))
    
  }
  
  all_predictions <- all_predictions %>%
    mutate(week = ymd(week),
           month = ymd(floor_date(week, unit = "month")),
           fips = as.numeric(levels(fips))[as.integer(fips)]) %>%
    left_join(urb_rur_codes)
  write_csv(all_predictions, paste0("data/output/", folder_name, "/fitted_predictions_", folder_sublevel, "_update.csv"))
  
}


#### RUN DIAGNOSTICS ####

for(age_level in c(1, 2, 3, 4)){

  folder_sublevel = paste0("age", age_level)
  contact_data <- county_week_by_age4 %>%
    filter(age4 == age_level)

# for(gender_level in c(1, 2)){
# 
#   folder_sublevel = paste0("gender", gender_level)
#   contact_data <- county_week_by_gender %>%
#     filter(gender == gender_level)
# 
#     predictions <- read_csv(paste0("data/output/", folder_name, "/fitted_predictions_", folder_sublevel, "_update.csv"),
#                             col_types = ifelse(grepl("age", folder_sublevel) | grepl("gender", folder_sublevel),
#                             "ddiifddiiDccdDccfi", "ddiifdiDccdDccfi"))

# for(setting_level in c("social", "other", "shopping", "work")){
#   folder_sublevel <- setting_level
#   contact_data <- county_week_by_setting %>%
#     filter(setting == setting_level)
  
  predictions <- read_csv(paste0("data/output/", folder_name, "/fitted_predictions_", folder_sublevel, "_update.csv"),
                          col_types = ifelse(grepl("age", folder_sublevel) | grepl("gender", folder_sublevel),
                                             "ddiifddiiDccdDccfi", "ddiifdiDccdDccfi"))
  
  soi <- sample(unique(predictions$state), 9)
  predictions %>%
    filter(state %in% soi) %>%
    ggplot(aes(x = week, y = residual)) +
    geom_line(aes(group = fips), alpha = 0.5, col = "grey") +
    geom_smooth() +
    theme_bw() +
    facet_wrap(~state) -> plot
  
  print(plot)
  
  predictions <- predictions %>%
    mutate(residual_capped = ifelse(abs(residual) > 10, sign(residual) * 11, residual))
  woi <- sample(unique(predictions$week), 10)
  for(i in 1:length(woi)){
    map <- CountyChoropleth$new(predictions %>%
                                  filter(week == woi[[i]]) %>%
                                  mutate(fips = as.numeric(fips)) %>%
                                  mutate(region = ifelse(fips == 2158, 2270,
                                                         ifelse(fips == 46102, 46113, fips)),
                                         value = residual_capped))
    map$ggplot_polygon <- geom_polygon(aes(fill = value), color = NA, size = 0.01)
    map$set_num_colors(1)
    map$ggplot_scale <- scale_fill_gradientn(colors=met.brewer("Hiroshige"),
                                             limits = c(-11, 11),
                                             #trans = scales::log10_trans(),
                                             name = "Residual")
    map <- map$render()
    print(map + labs(caption = paste0(folder_sublevel, " Week of ", woi[[i]])))
  }
  
  # fitted vs residual, looks ok for higher sample sizes
  predictions %>% 
    ggplot(aes(x = non_hh_contacts, y = fit, col = log(samp_size))) + 
    geom_point(alpha = 0.25) + 
    scale_color_viridis_c() +
    geom_abline(col = "red") -> plot2
  print(plot2)
  
  
  # check for residual autocorrelation within fips codes
  eligible_fips <- predictions %>%
    group_by(fips) %>% 
    summarise(mean_samp = mean(samp_size)) %>% #will be NA if ever imputed
    filter(!is.na(mean_samp)) %>% 
    pull(fips)
  fooi <- sample(eligible_fips, 50)
  
  corr_data <- predictions %>% 
    filter(fips %in% eligible_fips) %>% 
    group_by(fips) %>% 
    reframe(corr = as.vector(acf(residual, type = "correlation", plot = F, lag.max = 4)$acf),
            lag = as.vector(acf(residual, type = "correlation", plot = F, lag.max = 4)$lag),
            corr_partial = c(NA, pacf(residual, type = "correlation", plot = F, lag.max = 4)$acf)) %>% 
    ungroup()
  
  print(corr_data %>% 
          filter(lag != 0) %>% 
          ggplot(aes(x = corr, y = stat(count) / sum(count))) + 
          geom_histogram(col = "white") +
          facet_wrap(~lag) +
          theme_bw() +
          labs(y = "Frequency"))
  
  print(corr_data %>% 
          filter(lag != 0) %>% 
          ggplot(aes(x = corr_partial, y = stat(count) / sum(count))) + 
          geom_histogram(col = "white") +
          facet_wrap(~lag) +
          theme_bw() +
          labs(y = "Frequency"))
  
  for(i in 1:4){
    map <- CountyChoropleth$new(corr_data %>% 
                                  filter(lag == i) %>% 
                                  mutate(fips = as.numeric(fips)) %>% 
                                  mutate(region = ifelse(fips == 2158, 2270, 
                                                         ifelse(fips == 46102, 46113, fips)),
                                         value = corr))
    map$ggplot_polygon <- geom_polygon(aes(fill = value), color = NA, size = 0.01)
    map$set_num_colors(1)
    map$ggplot_scale <- scale_fill_gradientn(colors=met.brewer("Hiroshige"), 
                                             limits = c(-0.6, 0.6),
                                             #trans = scales::log10_trans(),
                                             name = "Residual\nautocorrelation")
    map <- map$render()
    print(map + labs(caption = paste0("Lag ", i)))
    # partial
    map <- CountyChoropleth$new(corr_data %>% 
                                  filter(lag == i) %>% 
                                  mutate(fips = as.numeric(fips)) %>% 
                                  mutate(region = ifelse(fips == 2158, 2270, 
                                                         ifelse(fips == 46102, 46113, fips)),
                                         value = corr_partial))
    map$ggplot_polygon <- geom_polygon(aes(fill = value), color = NA, size = 0.01)
    map$set_num_colors(1)
    map$ggplot_scale <- scale_fill_gradientn(colors=met.brewer("Hiroshige"), 
                                             limits = c(-0.6, 0.6),
                                             #trans = scales::log10_trans(),
                                             name = "Residual\npartial\nautocorrelation")
    map <- map$render()
    print(map + labs(caption = paste0("Lag ", i)))
   }
  
  # violin plot of lag in urban vs rural
  print(corr_data %>% 
          left_join(urb_rur_codes %>% select(fips, ur_code)) %>% 
          filter(lag != 0) %>% 
          ggplot(aes(x = ur_code, y = corr, fill = ur_code)) +
          geom_violin(alpha = 0.5) + 
          geom_boxplot(alpha = 0.5, width = 0.35) +
          scale_fill_viridis_d(direction = 1, end = 0.95) +
          facet_wrap(~lag) +
          theme_bw() +
          geom_hline(yintercept = 0, col = "red", lty = "dashed")) 
  
  print(corr_data %>% 
          left_join(urb_rur_codes %>% select(fips, ur_code)) %>% 
          filter(lag != 0) %>% 
          ggplot(aes(x = ur_code, y = corr_partial, fill = ur_code)) +
          geom_violin(alpha = 0.5) + 
          geom_boxplot(alpha = 0.5, width = 0.35) +
          scale_fill_viridis_d(direction = 1, end = 0.95) +
          facet_wrap(~lag) +
          theme_bw() +
          geom_hline(yintercept = 0, col = "red", lty = "dashed")) 
  
  print(corr_data %>%
          filter(lag != 0) %>% 
          filter(fips %in% fooi) %>%
          ggplot(aes(x = lag, y = corr)) +
          geom_col(width = 0.1) +
          facet_wrap(~fips, nrow = 5) +
          theme_bw() +
          geom_hline(yintercept = 0) +
          labs(y = "autocorrelation", x = "lag (weeks)") +
          geom_hline(yintercept = 0.2, col = "royalblue", lty = "dashed") +
          geom_hline(yintercept = -0.2, col = "royalblue", lty = "dashed") +
          scale_x_continuous(breaks = seq(from = 0, to = 4, by = 1)))
  
  print(corr_data %>% 
          filter(lag != 0) %>% 
          filter(fips %in% fooi) %>%
          ggplot(aes(x = lag, y = corr_partial)) +
          geom_col(width = 0.1) +
          facet_wrap(~fips, nrow = 5) +
          theme_bw() +
          geom_hline(yintercept = 0) +
          labs(y = "partial-autocorrelation", x = "lag (weeks)") +
          geom_hline(yintercept = 0.2, col = "royalblue", lty = "dashed") +
          geom_hline(yintercept = -0.2, col = "royalblue", lty = "dashed") +
          scale_x_continuous(breaks = seq(from = 1, to = 4, by = 1)))
  
}

