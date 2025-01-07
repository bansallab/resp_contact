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

df.fips <- read_csv('data/input/state_and_county_fips_master.csv')

urb_rur_codes <- read_excel("data/input/NCHSURCodes2013.xlsx") %>% rename(fips = `FIPS code`) %>% 
  mutate(`CBSA 2012 pop` = as.integer(`CBSA 2012 pop`),
         `County 2012 pop` = as.integer(`County 2012 pop`)) %>% 
  dplyr::select(fips, `State Abr.`, `County name`, `CBSA title`, `2013 code`, 
         `County 2012 pop`) %>% 
  rename(state = `State Abr.`, county = `County name`, area = `CBSA title`,
         ur_code = `2013 code`, population = `County 2012 pop`) %>%  # can ignore warnings
  mutate(ur_code = as.factor(ur_code))

# census regions from: https://github.com/cphalpert/census-regions/blob/master/us%20census%20bureau%20regions%20and%20divisions.csv
census_regions <- read_csv("data/input/census_regions.csv", col_types = "ccfc") %>% 
  rename(census_region = Region)

state_week_by_race <- read_csv("data/group_means_rake/contact_by_race_week_state_trunc72.csv",
                               col_types = "Dfcddi") %>% # check these
  mutate(nonhh_contacts_rd = round(non_hh_contacts)) %>%
  filter(week > ymd("2020-05-31"), week < ymd("2021-05-01"))

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

states_to_model <- large_enough_race_states

# ------------------------ #
#### RUN THE MAIN MODEL ####
# ------------------------ #

folder_name <- "ethrace_72trunc_region4_m1"
require(gratia)

for(race_level in unique(state_week_by_race$race_cat_col)){
  
  print(race_level)
  
  folder_sublevel = paste0(race_level)
  contact_data <- state_week_by_race %>%
    filter(race_cat_col == race_level)
  
  to_model <- contact_data %>% ungroup() %>% 
    mutate(week_rank = as.integer(as.numeric(week)/7)) %>% # create a numeric dummy for week to feed to mgcv
    mutate(week_rank = week_rank - min(week_rank) + 1) %>% # set min val of dummy to 1
    ungroup() %>% 
    left_join(census_regions, by = c("state" = "State Code")) %>% 
    filter(!is.na(state),
           state %in% states_to_model)
  # I think state still needs to be a factor, but not out here bc we don't want it to be aware of other levels
  
  regions_to_model <- unique(to_model$census_region)
  
  n_week <- to_model %>% pull(week_rank) %>% unique() %>% length()
  
  all_predictions <- data.frame()
  
  # fit the model for each state separately
  # since we are fitting means, there's a bit less concern about heavy tails
  
  for(region.fit in regions_to_model){
    
    print(region.fit)
    
    # extract data for that state only
    df.fit <- to_model %>% 
      filter(census_region == region.fit) %>% 
      mutate(id = row_number(),
             state = as.factor(state))
    
    fit <- bam(non_hh_contacts ~ s(week_rank, k = 30, m = 2) + s(week_rank, state, bs = 'fs', k = 15, m = 1),
               data = df.fit, # m = 2 means order of penalty, 2 is for normal cubic spline penalty with 2nd derivatives
               weights = samp_size,
               method = 'fREML', # fast reml
               gamma = 2, # increases penalty to force smoother model
               control = list(trace = TRUE))
    
    # extract the smoothed values, imputing for those missing from the data
    newd <- predict(fit, 
                    newdata = df.fit %>% complete(state, week_rank), 
                    se.fit = TRUE,
                    type = "response") %>% # pull out fit and se.fit individually, 
      #then do joins to create tibble 
      as_tibble() %>%
      mutate(id = row_number()) %>% 
      left_join(df.fit %>% complete(state, week_rank) %>% 
                  mutate(id = row_number())) %>% 
      select(-id) %>% 
      mutate(fit = as.numeric(fit), se.fit = as.numeric(se.fit)) # JT addition bc these two cols are arrays
    
    
    all_predictions <- all_predictions %>% bind_rows(newd)
    
    newd %>% ggplot(aes(x = week, y = fit)) + 
      geom_line(aes(group = state, col = state)) + 
      geom_line(aes(y = non_hh_contacts, group = state, col = state), lty = "dashed") + 
      facet_wrap(~state) +
      labs(caption = paste0(race_level)) +
      theme(legend.position = "none") -> plot
    print(plot)
    ggsave(paste0('data/output/', folder_name, '/obs_vs_fit/', region.fit, '-', folder_sublevel, '.pdf'),
           height = 5, width = 8)
    
    write_csv(newd, glue('data/output/', folder_name, '/region_fits/posterior_draws-contact_no_hh-', region.fit, '-', folder_sublevel, '.csv'))
    
    saveRDS(fit, glue('data/output/', folder_name,'/region_fits/posterior_draws-contact_model_fit-', region.fit, '-', folder_sublevel, '.rds'))
  }
  
  print(paste0("finished models for ", folder_sublevel, " category"))
  
  ## save fit checks ##
  my_log <- file(paste0("data/output/", folder_name, "/checking_model_fits_", folder_sublevel, ".txt")) # File name of output log
  sink(my_log, append = TRUE, type = "output") # Writing console output to log file
  sink(my_log, append = TRUE, type = "message")
  for(region.fit in regions_to_model){
    print(region.fit)
    par(mfrow = c(2, 2))
    model <- readRDS(paste0("data/output/", folder_name, "/region_fits/posterior_draws-contact_model_fit-", region.fit, '-', folder_sublevel, ".rds"))
    gam.check(model)
    appraise(model, method = "simulate", n_simulate = 500) # can't save gam.check plots but can save these
    ggsave(paste0("data/output/", folder_name, "/gam_check_plots/", region.fit, '-', folder_sublevel, ".pdf"), height = 6, width = 12)
  }
  
  closeAllConnections()
  
  all_predictions <- all_predictions %>%
    mutate(week = ymd(week),
           month = ymd(floor_date(week, unit = "month"))) #%>% 
  #fips = as.numeric(levels(fips))[as.integer(fips)]) %>%
  #left_join(urb_rur_codes)
  write_csv(all_predictions, paste0("data/output/", folder_name, "/fitted_predictions_", folder_sublevel, ".csv"))
  
}


for(race_level in unique(state_week_by_race$race_cat_col)){
  
  print(race_level)
  
  folder_sublevel = paste0(race_level)
  contact_data <- state_week_by_race %>%
    filter(race_cat_col == race_level)
  
  predictions <- read_csv(paste0("data/output/", folder_name, "/fitted_predictions_", folder_sublevel, ".csv"),
                          col_types = "ddciDfddiifcccD")
  
  to_model <- contact_data %>% ungroup() %>% 
    mutate(week_rank = as.integer(as.numeric(week)/7)) %>% # create a numeric dummy for week to feed to mgcv
    mutate(week_rank = week_rank - min(week_rank) + 1) %>% # set min val of dummy to 1
    ungroup() %>% 
    left_join(census_regions, by = c("state" = "State Code")) %>% 
    filter(#!is.na(state),
      state %in% states_to_model)
  
  regions_to_model <- unique(to_model$census_region)
  
  # plot model residuals
  
  all_residuals <- data.frame()
  for (region.fit in regions_to_model){
    mod <- readRDS(glue('data/output/', folder_name, '/region_fits/posterior_draws-contact_model_fit-', region.fit, '-', folder_sublevel, '.rds'))
    all_residuals <- all_residuals %>% bind_rows(data.frame(residual = residuals(mod, type = "response")))
  }
  
  all_residuals <- all_residuals %>% 
    bind_cols(to_model)
  
  soi <- sample(unique(all_residuals$state), 9)
  # residuals over time, I think we don't want to see any trends
  all_residuals %>%
    filter(state %in% soi) %>%
    ggplot(aes(x = week, y = residual)) +
    geom_line(aes(group = state), alpha = 0.5, col = "grey") +
    geom_smooth() +
    theme_bw() +
    facet_wrap(~state) -> plot
  
  print(plot)
  
  data(state.regions)
  
  all_residuals <- all_residuals %>%
    mutate(residual_capped = ifelse(abs(residual) > 10, sign(residual) * 11, residual)) %>% 
    left_join(state.regions, by = c("state" = "abb"))
  
  woi <- sample(unique(all_residuals$week), 10)
  for(i in 1:length(woi)){
    map <- StateChoropleth$new(all_residuals %>%
                                 filter(week == woi[[i]]) %>%
                                 mutate(value = residual_capped))
    map$ggplot_polygon <- geom_polygon(aes(fill = value), color = NA, linewidth = 0.01)
    map$set_num_colors(1)
    map$show_labels <- FALSE
    map$ggplot_scale <- scale_fill_gradientn(colors=met.brewer("Hiroshige"),
                                             limits = c(-11, 11),
                                             #trans = scales::log10_trans(),
                                             name = "Residual")
    map <- map$render()
    print(map + labs(caption = paste0(folder_sublevel, " Week of ", woi[[i]])))
  }
  
 
  # check for residual autocorrelation within fips codes
  
  corr_data <- all_residuals %>% 
    group_by(state) %>% 
    reframe(corr = as.vector(acf(residual, type = "correlation", plot = F, lag.max = 4)$acf),
            lag = as.vector(acf(residual, type = "correlation", plot = F, lag.max = 4)$lag),
            corr_partial = c(NA, pacf(residual, type = "correlation", plot = F, lag.max = 4)$acf)) %>% 
    ungroup()
  
  print(corr_data %>% 
          filter(lag != 0) %>% 
          ggplot(aes(x = corr, y = after_stat(count) / sum(count))) + 
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
    map <- StateChoropleth$new(corr_data %>% 
                                 filter(lag == i) %>% 
                                 mutate(value = corr) %>% 
                                 left_join(state.regions, by = c("state" = "abb")))
    map$ggplot_polygon <- geom_polygon(aes(fill = value), color = NA, size = 0.01)
    map$set_num_colors(1)
    map$show_labels <- FALSE
    map$ggplot_scale <- scale_fill_gradientn(colors=met.brewer("Hiroshige"), 
                                             limits = c(-0.6, 0.6),
                                             #trans = scales::log10_trans(),
                                             name = "Residual\nautocorrelation")
    map <- map$render()
    print(map + labs(caption = paste0("Lag ", i)))
    # partial
    map <- StateChoropleth$new(corr_data %>% 
                                 filter(lag == i) %>% 
                                 mutate(value = corr_partial) %>% 
                                 left_join(state.regions, by = c("state" = "abb")))
    map$ggplot_polygon <- geom_polygon(aes(fill = value), color = NA, size = 0.01)
    map$set_num_colors(1)
    map$show_labels <- FALSE
    map$ggplot_scale <- scale_fill_gradientn(colors=met.brewer("Hiroshige"), 
                                             limits = c(-0.6, 0.6),
                                             #trans = scales::log10_trans(),
                                             name = "Residual\npartial\nautocorrelation")
    map <- map$render()
    print(map + labs(caption = paste0("Lag ", i)))
  }
  
  print(corr_data %>%
          filter(lag != 0) %>% 
          ggplot(aes(x = lag, y = corr)) +
          geom_col(width = 0.1) +
          facet_wrap(~state, nrow = 5) +
          theme_bw() +
          geom_hline(yintercept = 0) +
          labs(y = "autocorrelation", x = "lag (weeks)") +
          geom_hline(yintercept = 0.2, col = "royalblue", lty = "dashed") +
          geom_hline(yintercept = -0.2, col = "royalblue", lty = "dashed") +
          scale_x_continuous(breaks = seq(from = 0, to = 4, by = 1)))
  
  print(corr_data %>% 
          filter(lag != 0) %>% 
          ggplot(aes(x = lag, y = corr_partial)) +
          geom_col(width = 0.1) +
          facet_wrap(~state, nrow = 5) +
          theme_bw() +
          geom_hline(yintercept = 0) +
          labs(y = "partial-autocorrelation", x = "lag (weeks)") +
          geom_hline(yintercept = 0.2, col = "royalblue", lty = "dashed") +
          geom_hline(yintercept = -0.2, col = "royalblue", lty = "dashed") +
          scale_x_continuous(breaks = seq(from = 1, to = 4, by = 1)))
  
}

