# first get fb data on age, sex, education, and group to county-week in desired duration, 
#   then we calc these distributions for county week
{
  library(tidyverse)
  library(lubridate)
  library(vroom)
  library(tidylog)
}

# need to read in all columns we could possibly want for analysis here so that they stay attached to their
#   weight value
# responses with race and ethnicity may need to be raked separately because they don't cover our entire time frame
# perhaps we will leave race out of it, the sample sizes aren't great

# load datasets 06/2020 - 04/2021
path <- # TODO: INSERT PATH TO DIRECTORY WITH INDIVIDUAL DATA FILES, [1:11]

cols <- cols_only(
  fips = col_integer(),
  StartDatetime = col_datetime(),  # Time survey begun
  weight = col_double(),
  A2b = col_double(), #  How many people are in your household *including yourself*?
  A5_1 = col_double(), # How many people are in your household *including yourself*? children, <18
  A5_2 = col_double(), # How many people are in your household *including yourself*? adults, 18-64
  A5_3 = col_double(), # How many people are in your household *including yourself*? seniors, >=65
  B11 = col_factor(as.character(seq(1, 3, 1))), # positive test ever
  B10a = col_factor(as.character(seq(1, 3, 1))), # positive test in last 14 days
  C10_1_1 = col_double(), # direct contact outside household at work
  C10_2_1 = col_double(), # direct contact outside household shopping
  C10_3_1 = col_double(), # direct contact outside household social gatherings
  C10_4_1 = col_double(), # direct contact outside household other
  C7 = col_factor(as.character(seq(1, 4, 1))), # To what extent are you intentionally avoiding contact with other people?
  C9 = col_factor(as.character(seq(1, 4, 1))), # how worried that you or family member will get severely ill from covid
  D2 = col_factor(as.character(seq(1, 7, 1))), # What is your age (1-9)?
  D3 = col_double(), # how many children under 18 staying in household
  D4 = col_double(), # how many adults between 18 and 64 staying in household, *not including yourself*
  D5 = col_double(), # how many adults65 or older staying in household, *not including yourself*
  D1 = col_factor(as.character(seq(1, 5, 1))), # gender
  A2 = col_double(), # number sick in household, including self, requires fever
  D8 = col_factor(as.character(seq(1, 7, 1)))) # education

# load FB data
data <- paste0(# TODO: INSERT PATH TO DIRECTORY WITH INDIVIDUAL DATA FILES,
                  path) %>%
  map(vroom, # maps function to a vector, vroom is faster version of read_csv
      delim = ',',
      col_types = cols) %>% # map passes back a list of each of the separate paths
  bind_rows() %>%
  mutate(response_id = row_number())

## we'd like to perform as much cleaning as possible before raking
df.clean <- data %>%
  filter(!is.na(fips), fips < 60000) %>%  # 3% removed
  filter((A2b >= 0) | is.na(A2b)) %>% # non-neg or NA num hh, 0 removed
  filter((A5_1 >= 0) | is.na(A5_1), # non-neg or NA num hh <18
         (A5_2 >= 0) | is.na(A5_2), # non-neg or NA num hh 18-64
         (A5_3 >= 0 )| is.na(A5_3)) %>% # non-neg or NA num hh >= 65, <1% removed for this whole thing
  filter(!(is.na(A2b) & is.na(A5_1) & is.na(A5_2) & is.na(A5_3))) %>% # non NA num in hh, putting this back but might not need, 1% removed
  filter(!(is.na(C10_1_1) & is.na(C10_2_1) & is.na(C10_3_1) & is.na(C10_4_1))) %>% # non NA num outside hh, 22% removed
  rowwise() %>%  # to make it rowwise operation
  mutate(num_hh = ifelse(!is.na(A2b), A2b, sum(A5_1, A5_2, A5_3, na.rm = T))) %>% # combine hh count vars, ignore NA
  filter(num_hh >= 0) %>% # no negative hh sizes, none removed
  mutate(num_hh_contact = ifelse(num_hh - 1 < 0, 0, num_hh - 1),  # subtract include yourself
         outside_contacts = sum(C10_1_1, C10_2_1, C10_3_1, C10_4_1, na.rm = T)) %>%
  # filter(num_hh_contact <= 17, # 99th percentile
  #        outside_contacts <= 400) %>% # 99th percentile, 2% removed total (makes sense)
  rowwise() %>%
  mutate(num_hh = ifelse(is.na(num_hh), NA, as.integer(num_hh)), # some of these are still zero
         num_hh_contact = ifelse(is.na(num_hh_contact), NA, as.integer(num_hh_contact)), # these as.integer() round down
         outside_contacts = as.integer(outside_contacts)) %>% 
  rename(age = D2,
         worry = C9,
         education = D8,
         gender = D1,
         avoid_contact = C7,
         positive_test = B11,
         positive_test_14d = B10a,
         contacts_work = C10_1_1,
         contacts_shop = C10_2_1,
         contacts_hh_gathering = C10_3_1,
         contacts_other = C10_4_1) %>% 
  select(-c(A2b, A5_1, A5_2, A5_3, D3, D4, D5, A2))

write_csv(df.clean, "data/output/contact_data_cleaned_without_date.csv")


df.clean.nodate <- read_csv("data/output/contact_data_cleaned_without_date.csv", 
                            col_types = list(StartDatetime = col_datetime(),
                                             avoid_contact = col_factor(as.character(seq(1, 4, 1))),
                                             worry = col_factor(as.character(seq(1, 4, 1))),
                                             contacts_work = col_double(),
                                             contacts_shop = col_double(),
                                             contacts_hh_gathering = col_double(),
                                             contacts_other = col_double(),
                                             gender = col_factor(as.character(seq(1, 5, 1))),
                                             age = col_factor(as.character(seq(1, 7, 1))),
                                             fips = col_integer(),
                                             weight = col_double(),
                                             positive_test = col_factor(as.character(seq(1, 3, 1))),
                                             B10a = col_factor(as.character(seq(1, 3, 1))), # change to positive_test_14d
                                             education = col_factor(as.character(seq(1, 7, 1))),
                                             response_id = col_integer(),
                                             num_hh = col_double(),
                                             num_hh_contact = col_integer(),
                                             outside_contacts = col_integer())
)


df.clean.date <- df.clean.nodate %>%
  mutate(date = date(StartDatetime),
         week = floor_date(date, unit = "week"),
         month = floor_date(date, unit = "month"))

write_csv(df.clean.date, "data/output/contact_data_cleaned_with_date.csv")

df.clean.date <- read_csv("data/output/contact_data_cleaned_with_date.csv",
                          col_types = list(date = col_date(),
                                           week = col_date(),
                                           month = col_date(),
                                           avoid_contact = col_factor(as.character(seq(1, 4, 1))),
                                           worry = col_factor(as.character(seq(1, 4, 1))),
                                           contacts_work = col_double(),
                                           contacts_shop = col_double(),
                                           contacts_hh_gathering = col_double(),
                                           contacts_other = col_double(),
                                           gender = col_factor(as.character(seq(1, 5, 1))),
                                           age = col_factor(as.character(seq(1, 7, 1))),
                                           fips = col_integer(),
                                           weight = col_double(),
                                           positive_test = col_factor(as.character(seq(1, 3, 1))),
                                           positive_test_14d = col_factor(as.character(seq(1, 3, 1))), 
                                           education = col_factor(as.character(seq(1, 7, 1))),
                                           response_id = col_integer(),
                                           num_hh = col_double(),
                                           num_hh_contact = col_integer(),
                                           outside_contacts = col_integer())
)

age_sex_rake_data <- df.clean.date %>% 
  filter(!is.na(gender), !is.na(age)) %>% # need these vars for raking, 3% removed
  filter(gender %in% c(1, 2)) %>% # ACS can't compare non-binary, prefer not to answer, or other genders; assuming gender == sex, 2% removed
  select(-StartDatetime)

# don't aggregate because anesrake wants to give a weight to each response
age_sex_rake_data_cat <- age_sex_rake_data %>% 
  mutate(age_cat = as.factor(case_when(age == 1 ~ "age18to24",
                                       age == 2 ~ "age25to34",
                                       age == 3 ~ "age35to44",
                                       age == 4 ~ "age45to54",
                                       age == 5 ~ "age55to64",
                                       age == 6 ~ "age65to74",
                                       age == 7 ~ "age75up")),
         sex_cat = as.factor(case_when(gender == 1 ~ "male",
                                       gender == 2 ~ "female"))) %>% 
  rename(given_weight = weight) %>% 
  mutate(fips = ifelse(fips == 2270, 2158, ifelse(fips == 46113, 46102, fips)))

# add row numbers for age_sex now that not feeding into education
age_sex_rake_data_cat <- age_sex_rake_data_cat %>% 
  mutate(caseid = row_number())

write_csv(age_sex_rake_data_cat, "data/output/contact_data_to_rake_age_sex.csv")

#### NOW ADD RACE/ETHNICITY DATA ####
# race/ethnicity data should start in sept 2020 

path <- # TODO: INSERT PATH TO DIRECTORY WITH INDIVIDUAL DATA FILES, [1:8]
  
cols <- cols_only(
  fips = col_integer(),
  StartDatetime = col_datetime(),  # Time survey begun
  weight = col_double(),
  A2b = col_double(), #  How many people are in your household *including yourself*?
  A5_1 = col_double(), # How many people are in your household *including yourself*? children, <18
  A5_2 = col_double(), # How many people are in your household *including yourself*? adults, 18-64
  A5_3 = col_double(), # How many people are in your household *including yourself*? seniors, >=65
  B11 = col_factor(as.character(seq(1, 3, 1))), # positive test ever
  B10a = col_factor(as.character(seq(1, 3, 1))), # positive test in last 14 days
  C10_1_1 = col_double(), # direct contact outside household at work
  C10_2_1 = col_double(), # direct contact outside household shopping
  C10_3_1 = col_double(), # direct contact outside household social gatherings
  C10_4_1 = col_double(), # direct contact outside household other
  C7 = col_factor(as.character(seq(1, 4, 1))), # To what extent are you intentionally avoiding contact with other people?
  C9 = col_factor(as.character(seq(1, 4, 1))), # how worried that you or family member will get severely ill from covid
  D2 = col_factor(as.character(seq(1, 7, 1))), # What is your age (1-9)?
  D3 = col_double(), # how many children under 18 staying in household
  D4 = col_double(), # how many adults between 18 and 64 staying in household, *not including yourself*
  D5 = col_double(), # how many adults65 or older staying in household, *not including yourself*
  D1 = col_factor(as.character(seq(1, 5, 1))), # gender
  A2 = col_double(), # number sick in household, including self, requires fever
  D8 = col_factor(as.character(seq(1, 7, 1))), # education
  raceethnicity = col_factor(levels = c("Hispanic", "NonHispanicAmericanIndianAlaskaNative",
                                        "NonHispanicAsian", "NonHispanicBlackAfricanAmerican",
                                        "NonHispanicMultipleOther", "NonHispanicNativeHawaiianPacificIslander",
                                        "NonHispanicWhite")))

# load FB data
data <- paste0(# TODO: INSERT PATH TO DIRECTORY WITH INDIVIDUAL DATA FILES,
                path) %>%
  map(vroom, # maps function to a vector, vroom is faster version of read_csv
      delim = ',',
      col_types = cols) %>% # map passes back a list of each of the separate paths
  bind_rows() %>%
  mutate(response_id = row_number())
# should I be worried about the parsing issues here? investigated for each individual month and found no issues

## we'd like to perform as much cleaning as possible before raking
df.clean <- data %>%
  filter(!is.na(fips), fips < 60000) %>%  # 3% removed
  filter((A2b >= 0) | is.na(A2b)) %>% # non-neg or NA num hh, 0 removed
  filter((A5_1 >= 0) | is.na(A5_1), # non-neg or NA num hh <18
         (A5_2 >= 0) | is.na(A5_2), # non-neg or NA num hh 18-64
         (A5_3 >= 0 )| is.na(A5_3)) %>% # non-neg or NA num hh >= 65, <1% removed for this whole thing
  filter(!(is.na(A2b) & is.na(A5_1) & is.na(A5_2) & is.na(A5_3))) %>% # non NA num in hh, putting this back but might not need, 1% removed
  filter(!(is.na(C10_1_1) & is.na(C10_2_1) & is.na(C10_3_1) & is.na(C10_4_1))) %>% # non NA num outside hh, 26% removed
  rowwise() %>%  # to make it rowwise operation
  mutate(num_hh = ifelse(!is.na(A2b), A2b, sum(A5_1, A5_2, A5_3, na.rm = T))) %>% # combine hh count vars, ignore NA
  filter(num_hh >= 0) %>% # no negative hh sizes, none removed
  mutate(num_hh_contact = ifelse(num_hh - 1 < 0, 0, num_hh - 1),  # subtract include yourself
         outside_contacts = sum(C10_1_1, C10_2_1, C10_3_1, C10_4_1, na.rm = T)) %>%
  filter(! is.na(outside_contacts)) %>% 
  # filter(num_hh_contact <= 17, # 99th percentile
  #        outside_contacts <= 400) %>% # 99th percentile, 2% removed total (makes sense)
  rowwise() %>%
  mutate(num_hh = as.integer(num_hh), # some of these are still zero
         num_hh_contact = as.integer(num_hh_contact), # these as.integer() round down
         outside_contacts = as.integer(outside_contacts)) %>% 
  rename(age = D2,
         worry = C9,
         education = D8,
         gender = D1,
         avoid_contact = C7,
         positive_test = B11,
         positive_test_14d = B10a,
         contacts_work = C10_1_1,
         contacts_shop = C10_2_1,
         contacts_hh_gathering = C10_3_1,
         contacts_other = C10_4_1) %>% 
  select(-c(A2b, A5_1, A5_2, A5_3, D3, D4, D5, A2))

write_csv(df.clean, "data/output/contact_data_race_cleaned_without_date.csv")


df.clean.nodate <- read_csv("data/output/contact_data_race_cleaned_without_date.csv", 
                            col_types = list(StartDatetime = col_datetime(),
                                             avoid_contact = col_factor(as.character(seq(1, 4, 1))),
                                             worry = col_factor(as.character(seq(1, 4, 1))),
                                             contacts_work = col_double(),
                                             contacts_shop = col_double(),
                                             contacts_hh_gathering = col_double(),
                                             contacts_other = col_double(),
                                             gender = col_factor(as.character(seq(1, 5, 1))),
                                             age = col_factor(as.character(seq(1, 7, 1))),
                                             fips = col_integer(),
                                             weight = col_double(),
                                             positive_test = col_factor(as.character(seq(1, 3, 1))),
                                             positive_test_14d = col_factor(as.character(seq(1, 3, 1))), # change to positive_test_14d
                                             education = col_factor(as.character(seq(1, 7, 1))),
                                             response_id = col_integer(),
                                             num_hh = col_double(),
                                             num_hh_contact = col_integer(),
                                             outside_contacts = col_integer(),
                                             raceethnicity = col_factor(levels = c("Hispanic", "NonHispanicAmericanIndianAlaskaNative",
                                                                                   "NonHispanicAsian", "NonHispanicBlackAfricanAmerican",
                                                                                   "NonHispanicMultipleOther", "NonHispanicNativeHawaiianPacificIslander",
                                                                                   "NonHispanicWhite")))
)


df.clean.date <- df.clean.nodate %>%
  mutate(date = date(StartDatetime),
         week = floor_date(date, unit = "week"),
         month = floor_date(date, unit = "month"))

write_csv(df.clean.date, "data/output/contact_data_race_cleaned_with_date.csv")

df.clean.date <- read_csv("data/output/contact_data_race_cleaned_with_date.csv",
                          col_types = list(date = col_date(),
                                           week = col_date(),
                                           month = col_date(),
                                           avoid_contact = col_factor(as.character(seq(1, 4, 1))),
                                           worry = col_factor(as.character(seq(1, 4, 1))),
                                           contacts_work = col_double(),
                                           contacts_shop = col_double(),
                                           contacts_hh_gathering = col_double(),
                                           contacts_other = col_double(),
                                           gender = col_factor(as.character(seq(1, 5, 1))),
                                           age = col_factor(as.character(seq(1, 7, 1))),
                                           fips = col_integer(),
                                           weight = col_double(),
                                           positive_test = col_factor(as.character(seq(1, 3, 1))),
                                           positive_test_14d = col_factor(as.character(seq(1, 3, 1))), 
                                           education = col_factor(as.character(seq(1, 7, 1))),
                                           response_id = col_integer(),
                                           num_hh = col_double(),
                                           num_hh_contact = col_integer(),
                                           outside_contacts = col_integer(),
                                           raceethnicity = col_factor(levels = c("Hispanic", "NonHispanicAmericanIndianAlaskaNative",
                                                                                 "NonHispanicAsian", "NonHispanicBlackAfricanAmerican",
                                                                                 "NonHispanicMultipleOther", "NonHispanicNativeHawaiianPacificIslander",
                                                                                 "NonHispanicWhite")))
)


# RACE RAKING PREP
# I think best bet is 4 categories: Hispanic, White, Black, Other/Mult
age_sex_race_rake_data <- df.clean.date %>% 
  filter(!is.na(gender), !is.na(age), !is.na(raceethnicity)) %>% # need these vars for raking, 8% removed
  filter(gender %in% c(1, 2)) %>% # ACS can't compare non-binary, prefer not to answer, or other genders; assuming gender == sex, 2% removed
  select(-StartDatetime)

age_sex_race_rake_data_cat <- age_sex_race_rake_data %>% 
  mutate(age_cat = as.factor(case_when(age == 1 ~ "age18to24",
                                       age == 2 ~ "age25to34",
                                       age == 3 ~ "age35to44",
                                       age == 4 ~ "age45to54",
                                       age == 5 ~ "age55to64",
                                       age == 6 ~ "age65to74",
                                       age == 7 ~ "age75up")),
         sex_cat = as.factor(case_when(gender == 1 ~ "male",
                                       gender == 2 ~ "female")),
         raceeth_cat = as.factor(case_when(raceethnicity == "NonHispanicAmericanIndianAlaskaNative" ~ "NHAIANNHPI",
                                           raceethnicity == "NonHispanicNativeHawaiianPacificIslander" ~ "NHAIANNHPI",
                                           raceethnicity == "NonHispanicMultipleOther" ~ "NHMultOther",
                                           raceethnicity == "NonHispanicWhite" ~ "NHWhite",
                                           raceethnicity == "NonHispanicAsian" ~ "NHAsian",
                                           raceethnicity == "NonHispanicBlackAfricanAmerican" ~ "NHBlack",
                                           raceethnicity == "Hispanic" ~ "HAll")),
         caseid = row_number()) %>% 
  rename(given_weight = weight) %>% 
  mutate(fips = ifelse(fips == 2270, 2158, ifelse(fips == 46113, 46102, fips)))

write_csv(age_sex_race_rake_data_cat, "data/output/contact_data_to_rake_age_sex_race.csv")