# -------------- #
# Libraries

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

df.fips <- read_csv('data/input/state_and_county_fips_master.csv')

county_week <- read_csv("data/group_means_rake/contact_by_county_week_trunc72.csv", 
                        col_types = "iDddddddddi") %>% 
  mutate(nonhh_contacts_rd = round(non_hh_contacts)) %>% 
  filter(week > ymd("2020-05-31"), week < ymd("2021-05-01"))

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

# ------------------------- #
#### RUN THE MAIN MODEL ####
# ------------------------- #

folder_name <- "normal_gamma2_72trunc"

to_model <- county_week %>% ungroup() %>% 
  mutate(week_rank = as.integer(as.numeric(week)/7)) %>% # create a numeric dummy for week to feed to mgcv
  mutate(week_rank = week_rank - min(week_rank) + 1) %>% # set min val of dummy to 1
  ungroup() %>% 
  left_join(df.fips) %>% 
  filter(!is.na(state)) 

states <- unique(to_model$state)

n_week <- to_model %>% pull(week_rank) %>% unique() %>% length()

# fit the model for each state separately
# since we are fitting means, there's a bit less concern about heavy tails

for(state.fit in states){
  
  print(state.fit)
  
  # extract data for that state only
  df.fit <- to_model %>% 
    filter(state == state.fit) %>% 
    mutate(id = row_number(),
           fips = as.factor(fips)) # I think convert to factor here and not earlier so complete doesn't think other levels exist
  
  
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
      
      fit <- bam(non_hh_contacts ~ s(week_rank, k = 30, m = 2) + s(week_rank, fips, bs = 'fs', k = 15, m = 2), # factor-smooth interaction, 
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
      fit <- bam(non_hh_contacts ~ s(week_rank, k = 30, m = 2) + s(week_rank, fips, bs = 'fs', k = 15, m = 2),
                 data = df.fit, # m = 2 means order of penalty, 2 is for normal cubic spline penalty with 2nd derivatives
                 weights = samp_size,
                 method = 'fREML', # fast reml
                 gamma = 2, # increases penalty to force smoother model
                 control = list(trace = TRUE))
      
    }
  } # this makes a prediction even when I have no data in that fips
  
  # extract the smoothed values, imputing for those missing from the data
  newd <- predict(fit, 
                  newdata = df.fit %>% complete(fips, week_rank), 
                  se.fit = TRUE,
                  type = "response") %>% # pull out fit and se.fit individually, 
    #then do joins to create tibble 
    as_tibble() %>%
    mutate(id = row_number()) %>% 
    left_join(df.fit %>% complete(fips, week_rank) %>% 
                mutate(id = row_number())) %>% 
    select(-id, -week) %>% 
    left_join(df.fit %>% 
                select(week, week_rank) %>% 
                unique()) %>% 
    select(-name, -state) %>% 
    left_join(df.fips %>% mutate(fips = as.factor(fips))) %>% 
    mutate(fit = as.numeric(fit), se.fit = as.numeric(se.fit)) # JT addition bc these two cols are arrays
  
  
  write_csv(newd, glue('data/output/', folder_name, '/state_fits/posterior_draws-contact_no_hh-', state.fit, '.csv'))
  
  saveRDS(fit, glue('data/output/', folder_name, '/state_fits/posterior_draws-contact_model_fit-', state.fit, '.rds'))
}

# --------------------- #
#### check model fit ####
# --------------------- #

require(gratia)
my_log <- file(paste0("data/output/", folder_name, "/checking_model_fits.txt")) # File name of output log
sink(my_log, append = TRUE, type = "output") # Writing console output to log file
sink(my_log, append = TRUE, type = "message")
for(state.fit in states){
  print(state.fit)
  par(mfrow = c(2, 2))
  model <- readRDS(paste0("data/output/", folder_name, "/state_fits/posterior_draws-contact_model_fit-", state.fit, ".rds"))
  gam.check(model)
  appraise(model, method = "simulate", n_simulate = 500) # can't save gam.check plots but can save these
  ggsave(paste0("data/output/", folder_name, "/gam_check_plots/", state.fit, ".pdf"), height = 6, width = 12)
}

closeAllConnections()


# -------------------------- #
#### plot model residuals ####
# -------------------------- #

states <- unique((df.fips %>% filter(fips != 66, !is.na(state)))$state)

all_residuals <- data.frame()
for (state.fit in states){
  mod <- readRDS(glue('data/output/', folder_name, '/state_fits/posterior_draws-contact_model_fit-', state.fit, '.rds'))
  all_residuals <- all_residuals %>% bind_rows(data.frame(residual = residuals(mod, type = "response")))
}

all_residuals <- all_residuals %>% 
  bind_cols(to_model)

soi <- sample(unique(all_residuals$state), 9)
all_residuals %>% 
  filter(state %in% soi) %>% 
  ggplot(aes(x = week, y = residual)) + 
  geom_line(aes(group = fips), alpha = 0.5, col = "grey") + 
  geom_smooth() + 
  theme_bw() +
  facet_wrap(~state)

all_residuals <- all_residuals %>% 
  mutate(residual_capped = ifelse(abs(residual) > 10, sign(residual) * 11, residual))
woi <- sample(unique(all_residuals$week), 10)
for(i in 1:length(woi)){
  map <- CountyChoropleth$new(all_residuals %>% 
                                filter(week == woi[[i]]) %>% 
                                mutate(fips = as.numeric(fips)) %>% 
                                mutate(region = ifelse(fips == 2158, 2270, 
                                                       ifelse(fips == 46102, 46113, fips)),
                                       value = residual_capped))
  map$ggplot_polygon <- geom_polygon(aes(fill = value), color = NA, size = 0.01)
  map$set_num_colors(1)
  map$ggplot_scale <- scale_fill_gradientn(colors=met.brewer("Hiroshige"), 
                                           limits = c(-3, 3),
                                           #trans = scales::log10_trans(),
                                           name = "Residual")
  map <- map$render()
  print(map + labs(caption = paste0("Week of ", woi[[i]])))
}


# --------------------------------------------------------- #
#### check for residual autocorrelation within fips codes ####
# --------------------------------------------------------- #

# what should we do about cases where the county is missing observations for a week
#   and therefore there is no residual?
eligible_fips <- all_residuals %>%
  group_by(fips) %>% 
  summarise(n = n()) %>% 
  filter(n > 40) %>% 
  pull(fips)
fooi <- sample(eligible_fips, 50)

corr_data <- all_residuals %>% 
  filter(fips %in% eligible_fips) %>% 
  group_by(fips) %>% 
  reframe(corr = as.vector(acf(residual, type = "correlation", plot = F, lag.max = 4)$acf),
          lag = as.vector(acf(residual, type = "correlation", plot = F, lag.max = 4)$lag),
          corr_partial = c(NA, pacf(residual, type = "correlation", plot = F, lag.max = 4)$acf)) %>% 
  ungroup()

corr_data %>% 
  filter(lag != 0) %>% 
  ggplot(aes(x = corr, y = stat(count) / sum(count))) + 
  geom_histogram(col = "white") +
  facet_wrap(~lag) +
  theme_bw() +
  labs(y = "Frequency")

corr_data %>% 
  filter(lag != 0) %>% 
  ggplot(aes(x = corr_partial, y = stat(count) / sum(count))) + 
  geom_histogram(col = "white") +
  facet_wrap(~lag) +
  theme_bw() +
  labs(y = "Frequency")

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
corr_data %>% 
  left_join(urb_rur_codes %>% select(fips, ur_code)) %>% 
  filter(lag != 0) %>% 
  ggplot(aes(x = ur_code, y = corr, fill = ur_code)) +
  geom_violin(alpha = 0.5) + 
  geom_boxplot(alpha = 0.5, width = 0.35) +
  scale_fill_viridis_d(direction = 1, end = 0.95) +
  facet_wrap(~lag) +
  theme_bw() +
  geom_hline(yintercept = 0, col = "red", lty = "dashed") 

corr_data %>% 
  left_join(urb_rur_codes %>% select(fips, ur_code)) %>% 
  filter(lag != 0) %>% 
  ggplot(aes(x = ur_code, y = corr_partial, fill = ur_code)) +
  geom_violin(alpha = 0.5) + 
  geom_boxplot(alpha = 0.5, width = 0.35) +
  scale_fill_viridis_d(direction = 1, end = 0.95) +
  facet_wrap(~lag) +
  theme_bw() +
  geom_hline(yintercept = 0, col = "red", lty = "dashed") 

corr_data %>%
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
  scale_x_continuous(breaks = seq(from = 0, to = 4, by = 1))

corr_data %>% 
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
  scale_x_continuous(breaks = seq(from = 1, to = 4, by = 1))


# investigate big residuals
big_residuals <- all_residuals %>% 
  #filter(abs(residual) > 5) %>% 
  left_join(urb_rur_codes)
big_residuals %>% ggplot(aes(x = samp_size, y = abs(residual), col = ur_code)) + 
  geom_point(alpha = 0.2) +
  scale_x_log10() +
  scale_color_viridis_d() +
  theme_bw()

big_residuals %>% 
  mutate(is_big = abs(residual) > 5) %>% 
  group_by(week) %>% 
  summarise(prop_big = sum(is_big)/n()) %>% 
  ggplot(aes(x = week, y = prop_big)) + 
  geom_point()


# ----------------------------------------- #
####      generate predicted values      ####
# ----------------------------------------- #

states <- unique((df.fips %>% filter(fips != 66, !is.na(state)))$state)

all_predictions <- data.frame()
for (state.fit in states){
  print(state.fit) # paste gamma_ in front of posterior below
  predictions <- read_csv(glue('data/output/', folder_name, '/state_fits/posterior_draws-contact_no_hh-',
                               state.fit, '.csv'),
                          col_types = "ddiiddddiiDcc")
  all_predictions <- all_predictions %>% bind_rows(predictions)
}

all_predictions <- all_predictions %>%
  mutate(week = ymd(week),
         month = ymd(floor_date(week, unit = "month"))) %>%
  left_join(urb_rur_codes)
write_csv(all_predictions, paste0("data/output/", folder_name, "/fitted_predictions.csv"))
