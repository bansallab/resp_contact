### Performs raking on age, gender for use contact data analysis
### Juliana Taube

### GOAL: come up with new county-week weights which we will use to resample the data/take weighted mean
###   to feed into the gam models
### APPROACH: to come up with these weights, use raking

### start with data from underlying population on a few demographic characteristics: age, sex
### compare these distributions to the distributions in the data (county-week aggregated) --> either use survey package or anesrake

{
  library(tidyverse)
  library(lubridate)
  library(vroom)
  library(anesrake)
}

#### ACS changes for all raking ####

all_acs <- read_csv("data/output/acs_target_data_for_contact_raking.csv",
                    col_types = list(fips = col_integer(),
                                     NAME = col_character(),
                                     all_18up = col_double(),
                                     prop_18to24 = col_double(),
                                     prop_25to34 = col_double(),
                                     prop_35to44 = col_double(),
                                     prop_45to54 = col_double(),
                                     prop_55to64 = col_double(),
                                     prop_65to74 = col_double(),
                                     prop_75up = col_double(),   
                                     prop_male = col_double(),
                                     prop_female = col_double(),
                                     prop_lt_hs = col_double(),
                                     prop_hs = col_double(),
                                     prop_lt_coll = col_double(),
                                     prop_2yrcoll = col_double(), 
                                     prop_4yrcoll = col_double(),
                                     prop_gt_coll = col_double(),
                                     prop_hisp =col_double(),
                                     prop_white = col_double(),
                                     prop_black = col_double(),
                                     prop_asian = col_double(),
                                     prop_fourcat = col_double(),
                                     prop_fivecat = col_double()))

all_acs <- all_acs %>% mutate(prop_18to34 = prop_18to24 + prop_25to34,
                              prop_35to54 = prop_35to44 + prop_45to54,
                              prop_55up = prop_55to64 + prop_65to74 + prop_75up,
                              prop_HSOrLess = prop_lt_hs + prop_hs,
                              prop_SomeAllColl = prop_lt_coll + prop_2yrcoll + prop_4yrcoll,
                              prop_PostColl = prop_gt_coll)

all_acs_test <- all_acs
all_acs_test[all_acs_test == 0] <- 0.0000000001

#----------------------------------#
#### raking on age and sex only ####
#----------------------------------#

age_sex_rake_data_cat <- read_csv("data/output/contact_data_to_rake_age_sex.csv", 
                                  col_types = list(date = col_date(),
                                                   week = col_date(),
                                                   month = col_date(),
                                                   avoid_contact = col_factor(as.character(seq(1, 4, 1))),
                                                   worry = col_factor(as.character(seq(1, 4, 1))),
                                                   contacts_work = col_double(),
                                                   contacts_shop = col_double(),
                                                   contacts_hh_gathering = col_double(),
                                                   contacts_other = col_double(),
                                                   gender = col_factor(as.character(seq(1, 2, 1))), # now just 2
                                                   age = col_factor(as.character(seq(1, 7, 1))),
                                                   fips = col_integer(),
                                                   given_weight = col_double(),
                                                   positive_test = col_factor(as.character(seq(1, 3, 1))),
                                                   positive_test_14d = col_factor(as.character(seq(1, 3, 1))), 
                                                   education = col_factor(as.character(seq(1, 7, 1))),
                                                   response_id = col_integer(),
                                                   num_hh = col_double(),
                                                   num_hh_contact = col_integer(),
                                                   outside_contacts = col_integer(),
                                                   caseid = col_integer(),
                                                   age_cat = col_factor(levels = c("age18to24", "age25to34",
                                                                                   "age35to44", "age45to54",
                                                                                   "age55to64", "age65to74",
                                                                                   "age75up")),
                                                   sex_cat = col_factor(levels = c("female", "male"))))


age_sex_rake_data_cat <- age_sex_rake_data_cat %>% filter(fips != 2261) # 614 responses removed


# inputter is a list of target values, list contains vectors 
# what's tricky is that we have different targets for each county
# need to apply anesrake to each county-week using a for loop

# for a given county week we need a list - this will be constant across weeks for target county but dataframe will change 
# now need data frame that matches targets

# need columns of this facebook data to be my desired attributes
# try collapsing age categories
age_sex_rake_data_cat <- age_sex_rake_data_cat %>% 
  mutate(age_cat_col = as.factor(case_when(age_cat == "age18to24" ~ "age18to34",
                                           age_cat == "age25to34" ~ "age18to34", #"age25to34",
                                           age_cat == "age35to44" ~ "age35to54",
                                           age_cat == "age45to54" ~ "age35to54",
                                           age_cat == "age55to64" ~ "age55up",
                                           age_cat == "age65to74" ~ "age55up",
                                           age_cat == "age75up" ~ "age55up")))


#### age and sex raking first ####

# I think RDS might be a bit more efficient than CSV
if("contact_raking_weights_age_sex_23_01_10.RDS" %in% list.files("data/raking/")){
  all_weights_age_sex <- readRDS("data/raking/contact_raking_weights_age_sex_23_01_10.RDS")
  non_convergent_age_sex <- readRDS("data/raking/contact_raking_nonconvergence_age_sex_23_01_10.RDS")
  counties <- readRDS("data/raking/counties_progress_23_01_10.RDS")
  # because only save after every county, might need to revise these read ins and filter out anything from that county
  # so can write over if repeat some weeks
}else{
  all_weights_age_sex <- data.frame(caseid = c(), weight = c())
  non_convergent_age_sex <- data.frame(month = c(), fips = c())
  # these are all the county-week combinations I need to do
  counties <- age_sex_rake_data_cat %>% group_by(fips) %>% 
    summarise(done = 0,
              raked_wks = NA) %>% 
    ungroup()
}

# try just marking counties as done instead of weeks, county_wks we used group_by(fips, week)

# Can I save my progress as I go and mark the item as done?
# Ok I think need to keep track of which county-weeks completed in the done column
#   of the foo dataframe
# But need to save the weights assigned as we go to each observation
# This might slow things down a bunch since so much saving, but worth it since
#   it should allow us to pick up where we left off
# Marking as done should occur after saving

# another option is to do one week at a time

todo <- which(counties$done==0) # gives row number
weeks <- age_sex_rake_data_cat %>% pull(week) %>% unique()

#### how many can we rake check ####
for(i in todo){
  
  # GET FB RESPONSE DATA for county
  fbdoi_county <- age_sex_rake_data_cat %>% filter(fips == counties$fips[i])
  weeks_raked <- 0
  
  # FOR EACH WEEK
  for(j in 1:length(weeks)){
    
    # GET FB RESPONSE DATA for week 
    fbdoi <- fbdoi_county %>% filter(week == weeks[j])
    
    num_age_levels <- length(unique(fbdoi$age_cat_col))
    num_sex_levels <- length(unique(fbdoi$sex_cat))
    
    # PERFORM RAKING
    if(num_age_levels == 3 & num_sex_levels == 2){
      weeks_raked <- weeks_raked + 1
    }
  }
  
  # saving for each county (if interrupted, may have to redo some weeks in a county??)
  counties$done[i] <- 1 # mark county as completed
  counties$raked_wks[i] <- weeks_raked
  
  saveRDS(counties, "data/raking/counties_rakeable_23_01_15.RDS")
  
}

raked_prop <- counties %>% rowwise() %>% mutate(prop = raked_wks/length(weeks))
obs <- age_sex_rake_data_cat %>% group_by(fips, week) %>% summarise(num_obs = n())
mean_obs <- obs %>% ungroup() %>% group_by(fips) %>% summarise(mean_obs = mean(num_obs))
raked_prop <- raked_prop %>% left_join(mean_obs, by = "fips")



# # test
# todo <- todo[1:100,]

my_log <- file("data/raking/raking_log_contact_age_sex.txt") # File name of output log
sink(my_log, append = TRUE, type = "output") # Writing console output to log file
sink(my_log, append = TRUE, type = "message")

# FOR EACH COUNTY
for(i in todo){
  
  # GET ACS TARGET DATA
  # targets just need once per county
  acsoi <- all_acs_test %>% filter(fips == counties$fips[i])
  # age
  agetarg <- c(acsoi$prop_18to34, acsoi$prop_35to54, acsoi$prop_55up)
  names(agetarg) <- c("age18to34", "age35to54", "age55up")
  # sex/gender
  sextarg <- c(acsoi$prop_male, acsoi$prop_female)
  names(sextarg) <- c("male", "female")
  
  targets <- list(agetarg, sextarg)
  names(targets) <- c("age_cat_col", "sex_cat")
  
  # GET FB RESPONSE DATA for county
  fbdoi_county <- age_sex_rake_data_cat %>% filter(fips == counties$fips[i])
  
  # FOR EACH WEEK
  for(j in 1:length(weeks)){
    
    # GET FB RESPONSE DATA for week 
    fbdoi <- fbdoi_county %>% filter(week == weeks[j])
    
    num_age_levels <- length(unique(fbdoi$age_cat_col))
    num_sex_levels <- length(unique(fbdoi$sex_cat))
    
    # PERFORM RAKING
    if(nrow(fbdoi) >= 3){
      # if(num_age_levels == 3 & num_sex_levels == 2){
      # These responses get weight NA based on mask files & code, meaning they are excluded from my analysis
      # (reasonable cause we can't make conclusion about county with that sample size,
      # I think you could actually use an even higher threshold)
      out <- anesrake(inputter = targets, dataframe = as.data.frame(fbdoi), caseid = fbdoi$caseid,
                      type = "nolim", verbose = F, cap = 10, maxit = 100) 
      # nolim means include all variables so I don't get error that data are too close to target
      outweights <- data.frame(caseid = out$caseid, weight = out$weightvec)
      all_weights_age_sex <- all_weights_age_sex %>% bind_rows(outweights)
      
      # print(out$converge)
      # SAVE COUNTY-WEEK IF NO CONVERGENCE
      if(!"Complete convergence was achieved" %in% out$converge){
        non_convergent_age_sex <- non_convergent_age_sex %>% bind_rows(data.frame(week = weeks[j], fips = counties$fips[i]))
        # saveRDS(non_convergent_age_sex, "data/raking/contact_raking_nonconvergence_age_sex_23_01_10.RDS")
      }
    }
  }
  
  # saving for each county (if interrupted, will have to redo some weeks in a county)
  # next step if this is still too slow is to save every XX iterations, or even by state
  saveRDS(all_weights_age_sex, "data/raking/contact_raking_weights_age_sex_23_01_10.RDS")
  saveRDS(non_convergent_age_sex, "data/raking/contact_raking_nonconvergence_age_sex_23_01_10.RDS") # moved here after run
  
  print(paste0("County ", counties$fips[i], " done"))
  counties$done[i] <- 1 # mark county as completed
  
  saveRDS(counties, "data/raking/counties_progress_23_01_10.RDS")
  
  # Pause to space out pull requests
  # Sys.sleep(2)
}

closeAllConnections()

# all_weights_age_sex <- readRDS("data/raking/contact_raking_weights_age_sex_23_01_10.RDS")
# non_convergent_age_sex <- readRDS("data/raking/contact_raking_nonconvergence_age_sex_23_01_10.RDS")
# counties <- readRDS("data/raking/counties_progress_23_01_10.RDS")

write_csv(non_convergent_age_sex, "data/raking/contact_raking_nonconvergence_age_sex.csv")
non_convergent_age_sex <- read_csv("data/raking/contact_raking_nonconvergence_age_sex.csv",
                                   col_types = "Di") %>%
  mutate(did_not_converge = 1)

contact_weighted_age_sex <- age_sex_rake_data_cat %>% left_join(all_weights_age_sex, by = "caseid")
write_csv(contact_weighted_age_sex, "data/raking/contact_raking_weights_age_sex.csv")

# correct nonconvergent weights to all be one
contact_corrected_age_sex <- contact_weighted_age_sex %>%
  left_join(non_convergent_age_sex) %>%
  ungroup() %>%
  rowwise() %>%
  mutate(updated_weight = ifelse(is.na(did_not_converge), weight, 1))
write_csv(contact_corrected_age_sex, "data/raking/contact_raking_weights_age_sex_corrected.csv")
# if an observation belongs to a county with fewer than 3 observations in a week, it has weight NA


#-------------------------------#
#### now do age sex and race ####
#-------------------------------#
all_acs <- read_csv("data/output/acs_target_state_data_for_contact_raking.csv",
                    col_types = list(state_fips = col_integer(),
                                     NAME = col_character(),
                                     all_18up = col_double(),
                                     prop_18to24 = col_double(),
                                     prop_25to34 = col_double(),
                                     prop_35to44 = col_double(),
                                     prop_45to54 = col_double(),
                                     prop_55to64 = col_double(),
                                     prop_65to74 = col_double(),
                                     prop_75up = col_double(),   
                                     prop_male = col_double(),
                                     prop_female = col_double(),
                                     prop_hisp =col_double(),
                                     prop_white = col_double(),
                                     prop_black = col_double(),
                                     prop_asian = col_double(),
                                     prop_fourcat = col_double(),
                                     prop_fivecat = col_double()))

all_acs <- all_acs %>% mutate(prop_18to34 = prop_18to24 + prop_25to34,
                              prop_35to54 = prop_35to44 + prop_45to54,
                              prop_55up = prop_55to64 + prop_65to74 + prop_75up)

all_acs_test <- all_acs
all_acs_test[all_acs_test == 0] <- 0.0000000001

age_sex_race_data <- read_csv("data/output/contact_data_to_rake_age_sex_race.csv",
                              col_types = list(date = col_date(),
                                               week = col_date(),
                                               month = col_date(),
                                               avoid_contact = col_factor(as.character(seq(1, 4, 1))),
                                               worry = col_factor(as.character(seq(1, 4, 1))),
                                               contacts_work = col_double(),
                                               contacts_shop = col_double(),
                                               contacts_hh_gathering = col_double(),
                                               contacts_other = col_double(),
                                               gender = col_factor(as.character(seq(1, 2, 1))), # now just 2
                                               age = col_factor(as.character(seq(1, 7, 1))),
                                               fips = col_integer(),
                                               given_weight = col_double(),
                                               positive_test = col_factor(as.character(seq(1, 3, 1))),
                                               positive_test_14d = col_factor(as.character(seq(1, 3, 1))), 
                                               education = col_factor(as.character(seq(1, 7, 1))),
                                               response_id = col_integer(),
                                               num_hh = col_double(),
                                               num_hh_contact = col_integer(),
                                               outside_contacts = col_integer(),
                                               caseid = col_integer(),
                                               age_cat = col_factor(levels = c("age18to24", "age25to34",
                                                                               "age35to44", "age45to54",
                                                                               "age55to64", "age65to74",
                                                                               "age75up")),
                                               sex_cat = col_factor(levels = c("female", "male")),
                                               raceeth_cat = col_factor(levels = c("HAll", "NHWhite", "NHBlack",
                                                                                   "NHAsian", "NHMultOther", "NHAIANNHPI")),
                                               raceethnicity = col_factor(levels = c("Hispanic", "NonHispanicAmericanIndianAlaskaNative",
                                                                                     "NonHispanicAsian", "NonHispanicBlackAfricanAmerican",
                                                                                     "NonHispanicMultipleOther", "NonHispanicNativeHawaiianPacificIslander",
                                                                                     "NonHispanicWhite")))
)

df.fips <- read_csv('data/input/state_and_county_fips_master.csv', 
                    col_types = "ccc") %>% 
  mutate(state_fips = as.integer(ifelse(nchar(fips) == 5, 
                                        substr(fips, 1, 2),
                                        substr(fips, 1, 1))),
         fips = as.integer(fips)) 
age_sex_race_data_state <- age_sex_race_data %>% 
  mutate(fips = ifelse(fips == 2158, 2270, 
                       ifelse(fips == 46102, 46113, fips))) %>% 
  left_join(df.fips) %>% 
  filter(fips != 2261) #  responses removed

# inputter is a list of target values, list contains vectors 
# what's tricky is that we have different targets for each county
# need to apply anesrake to each county-week using a for loop

# for a given county week we need a list - this will be constant across weeks for target county but dataframe will change 
# now need data frame that matches targets

# need columns of this facebook data to be my desired attributes
# try collapsing age categories
age_sex_race_data_state_cat <- age_sex_race_data_state %>% 
  mutate(age_cat_col = as.factor(case_when(age_cat == "age18to24" ~ "age18to34",
                                           age_cat == "age25to34" ~ "age18to34", #"age25to34",
                                           age_cat == "age35to44" ~ "age35to54",
                                           age_cat == "age45to54" ~ "age35to54",
                                           age_cat == "age55to64" ~ "age55up",
                                           age_cat == "age65to74" ~ "age55up",
                                           age_cat == "age75up" ~ "age55up")),
         race_cat_col = as.factor(case_when(raceeth_cat == "NHAIANNHPI" ~ "other",
                                            raceeth_cat == "NHMultOther" ~ "other",
                                            raceeth_cat == "HAll" ~ "hispanic",
                                            raceeth_cat == "NHWhite" ~ "white",
                                            raceeth_cat == "NHAsian" ~ "asian",
                                            raceeth_cat == "NHBlack" ~ "black")))


all_weights_age_sex_race <- data.frame(caseid = c(), weight = c())
non_convergent_age_sex_race <- data.frame(month = c(), fips = c())
# these are all the county-week combinations I need to do
states <- age_sex_race_data_state_cat %>% group_by(state_fips) %>% 
  summarise(done = 0,
            raked_wks = NA) %>% 
  ungroup()

todo <- which(states$done==0) # gives row number
months <- age_sex_race_data_state_cat %>% pull(month) %>% unique()


my_log <- file("data/raking/raking_log_contact_age_sex_race.txt") # File name of output log
sink(my_log, append = TRUE, type = "output") # Writing console output to log file
sink(my_log, append = TRUE, type = "message")

# FOR EACH COUNTY
for(i in todo){
  
  # GET ACS TARGET DATA
  # targets just need once per county
  acsoi <- all_acs_test %>% filter(state_fips == states$state_fips[i])
  # age
  agetarg <- c(acsoi$prop_18to34, acsoi$prop_35to54, acsoi$prop_55up)
  names(agetarg) <- c("age18to34", "age35to54", "age55up")
  # sex/gender
  sextarg <- c(acsoi$prop_male, acsoi$prop_female)
  names(sextarg) <- c("male", "female")
  # race/ethnicity
  racetarg <- c(acsoi$prop_white, acsoi$prop_hisp, acsoi$prop_black, acsoi$prop_asian, acsoi$prop_fivecat)
  names(racetarg) <- c("white", "hispanic", "black", "asian", "other")
  
  targets <- list(agetarg, sextarg, racetarg)
  names(targets) <- c("age_cat_col", "sex_cat", "race_cat_col")
  
  # GET FB RESPONSE DATA for county
  fbdoi_state <- age_sex_race_data_state_cat %>% filter(state_fips == states$state_fips[i])
  
  # FOR EACH WEEK
  for(j in 1:length(months)){
    
    # GET FB RESPONSE DATA for week 
    fbdoi <- fbdoi_state %>% filter(month == months[j])
    
    num_age_levels <- length(unique(fbdoi$age_cat_col))
    num_sex_levels <- length(unique(fbdoi$sex_cat))
    num_race_levels <- length(unique(fbdoi$race_cat_col))
    
    # PERFORM RAKING
    #if(nrow(fbdoi) >= 3){
    if(num_age_levels == 3 & num_sex_levels == 2 & num_race_levels == 5){
      # These responses get weight NA based on mask files & code, meaning they are excluded from my analysis
      # (reasonable cause we can't make conclusion about county with that sample size,
      # I think you could actually use an even higher threshold)
      out <- anesrake(inputter = targets, dataframe = as.data.frame(fbdoi), caseid = fbdoi$caseid,
                      type = "nolim", verbose = F, cap = 10, maxit = 100) # USED 100 ON PREVIOUS RUN
      # nolim means include all variables so I don't get error that data are too close to target
      outweights <- data.frame(caseid = out$caseid, weight = out$weightvec)
      all_weights_age_sex_race <- all_weights_age_sex_race %>% bind_rows(outweights)
      
      # print(out$converge)
      # SAVE COUNTY-WEEK IF NO CONVERGENCE
      if(!"Complete convergence was achieved" %in% out$converge){
        non_convergent_age_sex_race <- non_convergent_age_sex_race %>% 
          bind_rows(data.frame(month = months[j], state = states$state_fips[i]))
        # saveRDS(non_convergent_age_sex, "data/raking/contact_raking_nonconvergence_age_sex_23_01_10.RDS")
      }
    }
  }
  
  # saving for each county (if interrupted, will have to redo some weeks in a county)
  # next step if this is still too slow is to save every XX iterations, or even by state
  saveRDS(all_weights_age_sex_race, 
          "data/raking/contact_raking_weights_age_sex_race5_23_07_21.RDS")
  saveRDS(non_convergent_age_sex_race, 
          "data/raking/contact_raking_nonconvergence_age_sex_race5_23_07_21.RDS") # moved here after run
  
  print(paste0("State ", states$state_fips[i], " done"))
  states$done[i] <- 1 # mark county as completed
  
  saveRDS(states, "data/raking/states_race_progress_23_07_21.RDS")
  
  # Pause to space out pull requests
  # Sys.sleep(2)
}

closeAllConnections()

# all_weights_age_sex <- readRDS("data/raking/contact_raking_weights_age_sex_23_01_10.RDS")
# non_convergent_age_sex <- readRDS("data/raking/contact_raking_nonconvergence_age_sex_23_01_10.RDS")
# counties <- readRDS("data/raking/counties_progress_23_01_10.RDS")

# no nonconvergence in state month
write_csv(non_convergent_age_sex_race, "data/raking/contact_raking_nonconvergence_age_sex_race5_statemonth.csv")
non_convergent_age_sex_race <- read_csv("data/raking/contact_raking_nonconvergence_age_sex_race5_statemonth.csv",
                                        col_types = "Di") %>%
  mutate(did_not_converge = 1)

contact_weighted_age_sex_race <- age_sex_race_data_state_cat %>% left_join(all_weights_age_sex_race, by = "caseid")
write_csv(contact_weighted_age_sex_race, "data/raking/contact_raking_weights_age_sex_race5_statemonth.csv")

# correct nonconvergent weights to all be one
# contact_corrected_age_sex_race <- contact_weighted_age_sex_race %>%
#   left_join(non_convergent_age_sex_race) %>%
#   ungroup() %>%
#   rowwise() %>%
#   mutate(updated_weight = ifelse(is.na(did_not_converge), weight, 1))
# since no correction needed
write_csv(contact_weighted_age_sex_race, "data/raking/contact_raking_weights_age_sex_race5_statemonth_corrected.csv")
# if an observation belongs to a county with fewer than 3 observations in a week, it has weight NA

#-------------------------------------#
#### now do age sex AT STATE MONTH ####
#-------------------------------------#
all_acs <- read_csv("data/output/acs_target_state_data_for_contact_raking.csv",
                    col_types = list(state_fips = col_integer(),
                                     NAME = col_character(),
                                     all_18up = col_double(),
                                     prop_18to24 = col_double(),
                                     prop_25to34 = col_double(),
                                     prop_35to44 = col_double(),
                                     prop_45to54 = col_double(),
                                     prop_55to64 = col_double(),
                                     prop_65to74 = col_double(),
                                     prop_75up = col_double(),   
                                     prop_male = col_double(),
                                     prop_female = col_double(),
                                     prop_hisp =col_double(),
                                     prop_white = col_double(),
                                     prop_black = col_double(),
                                     prop_asian = col_double(),
                                     prop_fourcat = col_double(),
                                     prop_fivecat = col_double()))

all_acs <- all_acs %>% mutate(prop_18to34 = prop_18to24 + prop_25to34,
                              prop_35to54 = prop_35to44 + prop_45to54,
                              prop_55up = prop_55to64 + prop_65to74 + prop_75up)

all_acs_test <- all_acs
all_acs_test[all_acs_test == 0] <- 0.0000000001

age_sex_data <- read_csv("data/output/contact_data_to_rake_age_sex.csv",
                         col_types = list(date = col_date(),
                                          week = col_date(),
                                          month = col_date(),
                                          avoid_contact = col_factor(as.character(seq(1, 4, 1))),
                                          worry = col_factor(as.character(seq(1, 4, 1))),
                                          contacts_work = col_double(),
                                          contacts_shop = col_double(),
                                          contacts_hh_gathering = col_double(),
                                          contacts_other = col_double(),
                                          gender = col_factor(as.character(seq(1, 2, 1))), # now just 2
                                          age = col_factor(as.character(seq(1, 7, 1))),
                                          fips = col_integer(),
                                          given_weight = col_double(),
                                          positive_test = col_factor(as.character(seq(1, 3, 1))),
                                          positive_test_14d = col_factor(as.character(seq(1, 3, 1))), 
                                          education = col_factor(as.character(seq(1, 7, 1))),
                                          response_id = col_integer(),
                                          num_hh = col_double(),
                                          num_hh_contact = col_integer(),
                                          outside_contacts = col_integer(),
                                          caseid = col_integer(),
                                          age_cat = col_factor(levels = c("age18to24", "age25to34",
                                                                          "age35to44", "age45to54",
                                                                          "age55to64", "age65to74",
                                                                          "age75up")),
                                          sex_cat = col_factor(levels = c("female", "male"))))


df.fips <- read_csv('data/input/state_and_county_fips_master.csv', 
                    col_types = "ccc") %>% 
  mutate(state_fips = as.integer(ifelse(nchar(fips) == 5, 
                                        substr(fips, 1, 2),
                                        substr(fips, 1, 1))),
         fips = as.integer(fips)) 
age_sex_data_state <- age_sex_data %>% 
  mutate(fips = ifelse(fips == 2158, 2270, 
                       ifelse(fips == 46102, 46113, fips))) %>% 
  left_join(df.fips) %>% 
  filter(fips != 2261) #  responses removed

# inputter is a list of target values, list contains vectors 
# what's tricky is that we have different targets for each county
# need to apply anesrake to each county-week using a for loop

# for a given county week we need a list - this will be constant across weeks for target county but dataframe will change 
# now need data frame that matches targets

# need columns of this facebook data to be my desired attributes
# try collapsing age categories
age_sex_data_state_cat <- age_sex_data_state %>% 
  mutate(age_cat_col = as.factor(case_when(age_cat == "age18to24" ~ "age18to34",
                                           age_cat == "age25to34" ~ "age18to34", #"age25to34",
                                           age_cat == "age35to44" ~ "age35to54",
                                           age_cat == "age45to54" ~ "age35to54",
                                           age_cat == "age55to64" ~ "age55up",
                                           age_cat == "age65to74" ~ "age55up",
                                           age_cat == "age75up" ~ "age55up")))


all_weights_age_sex <- data.frame(caseid = c(), weight = c())
non_convergent_age_sex <- data.frame(month = c(), fips = c())
# these are all the county-week combinations I need to do
states <- age_sex_data_state_cat %>% group_by(state_fips) %>% 
  summarise(done = 0,
            raked_wks = NA) %>% 
  ungroup()

todo <- which(states$done==0) # gives row number
months <- age_sex_data_state_cat %>% pull(month) %>% unique()


my_log <- file("data/raking/raking_log_contact_age_sex_state.txt") # File name of output log
sink(my_log, append = TRUE, type = "output") # Writing console output to log file
sink(my_log, append = TRUE, type = "message")

# FOR EACH COUNTY
for(i in todo){
  
  # GET ACS TARGET DATA
  # targets just need once per county
  acsoi <- all_acs_test %>% filter(state_fips == states$state_fips[i])
  # age
  agetarg <- c(acsoi$prop_18to34, acsoi$prop_35to54, acsoi$prop_55up)
  names(agetarg) <- c("age18to34", "age35to54", "age55up")
  # sex/gender
  sextarg <- c(acsoi$prop_male, acsoi$prop_female)
  names(sextarg) <- c("male", "female")
  
  targets <- list(agetarg, sextarg)
  names(targets) <- c("age_cat_col", "sex_cat")
  
  # GET FB RESPONSE DATA for county
  fbdoi_state <- age_sex_data_state_cat %>% filter(state_fips == states$state_fips[i])
  
  # FOR EACH WEEK
  for(j in 1:length(months)){
    
    # GET FB RESPONSE DATA for week 
    fbdoi <- fbdoi_state %>% filter(month == months[j])
    
    num_age_levels <- length(unique(fbdoi$age_cat_col))
    num_sex_levels <- length(unique(fbdoi$sex_cat))
    
    # PERFORM RAKING
    #if(nrow(fbdoi) >= 3){
    if(num_age_levels == 3 & num_sex_levels == 2){
      # These responses get weight NA based on mask files & code, meaning they are excluded from my analysis
      # (reasonable cause we can't make conclusion about county with that sample size,
      # I think you could actually use an even higher threshold)
      out <- anesrake(inputter = targets, dataframe = as.data.frame(fbdoi), caseid = fbdoi$caseid,
                      type = "nolim", verbose = F, cap = 10, maxit = 100) # USED 100 ON PREVIOUS RUN
      # nolim means include all variables so I don't get error that data are too close to target
      outweights <- data.frame(caseid = out$caseid, weight = out$weightvec)
      all_weights_age_sex <- all_weights_age_sex %>% bind_rows(outweights)
      
      # print(out$converge)
      # SAVE COUNTY-WEEK IF NO CONVERGENCE
      if(!"Complete convergence was achieved" %in% out$converge){
        non_convergent_age_sex <- non_convergent_age_sex %>% 
          bind_rows(data.frame(month = months[j], state = states$state_fips[i]))
        # saveRDS(non_convergent_age_sex, "data/raking/contact_raking_nonconvergence_age_sex_23_01_10.RDS")
      }
    }
  }
  
  # saving for each county (if interrupted, will have to redo some weeks in a county)
  # next step if this is still too slow is to save every XX iterations, or even by state
  saveRDS(all_weights_age_sex, 
          "data/raking/contact_raking_weights_age_sex_state.RDS")
  saveRDS(non_convergent_age_sex, 
          "data/raking/contact_raking_nonconvergence_age_sex_state.RDS") # moved here after run
  
  print(paste0("State ", states$state_fips[i], " done"))
  states$done[i] <- 1 # mark county as completed
  
  saveRDS(states, "data/raking/states_progress_23_07_31.RDS")
  
  # Pause to space out pull requests
  # Sys.sleep(2)
}

closeAllConnections()

all_weights_age_sex <- readRDS("data/raking/contact_raking_weights_age_sex_state.RDS")
# non_convergent_age_sex <- readRDS("data/raking/contact_raking_nonconvergence_age_sex_23_01_10.RDS")
# counties <- readRDS("data/raking/counties_progress_23_01_10.RDS")

# no nonconvergence in state month
write_csv(non_convergent_age_sex, "data/raking/contact_raking_nonconvergence_age_sex_statemonth.csv")
non_convergent_age_sex <- read_csv("data/raking/contact_raking_nonconvergence_age_sex_statemonth.csv",
                                   col_types = "Di") %>%
  mutate(did_not_converge = 1)

contact_weighted_age_sex <- age_sex_data_state_cat %>% left_join(all_weights_age_sex, by = "caseid")
write_csv(contact_weighted_age_sex, "data/raking/contact_raking_weights_age_sex_statemonth.csv")

# correct nonconvergent weights to all be one
# contact_corrected_age_sex_race <- contact_weighted_age_sex_race %>%
#   left_join(non_convergent_age_sex_race) %>%
#   ungroup() %>%
#   rowwise() %>%
#   mutate(updated_weight = ifelse(is.na(did_not_converge), weight, 1))
# since no correction needed
write_csv(contact_weighted_age_sex, "data/raking/contact_raking_weights_age_sex_statemonth_corrected.csv")
# if an observation belongs to a county with fewer than 3 observations in a week, it has weight NA