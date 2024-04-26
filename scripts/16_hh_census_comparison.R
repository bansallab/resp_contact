# we are using acs data for this
library(tidycensus)
library(tidyverse)
library(tidylog)
library(lubridate)
# INSERT CENSUS API KEY
Sys.getenv("CENSUS_API_KEY")

# acs example
v21 <- load_variables(2021, "acs5", cache = TRUE)
#View(v21)

# technically using variables that are household size by owner or renter status
# vehicle size only did 1, 2, 3, 4
# households by owner/renter
hh_ownership <- get_acs(geography = "county", 
                        variables = c(owner_tot = "B25009_002",
                                      owner_1 = "B25009_003",
                                      owner_2 = "B25009_004",
                                      owner_3 = "B25009_005",
                                      owner_4 = "B25009_006",
                                      owner_5 = "B25009_007",
                                      owner_6 = "B25009_008",
                                      owner_7plus = "B25009_009",
                                      renter_tot = "B25009_010",
                                      renter_1 = "B25009_011",
                                      renter_2 = "B25009_012",
                                      renter_3 = "B25009_013",
                                      renter_4 = "B25009_014",
                                      renter_5 = "B25009_015",
                                      renter_6 = "B25009_016",
                                      renter_7plus = "B25009_017"),
                        summary_var = c("B25009_001"),
                        output = "wide", #other option is tidy
                        year = 2021) %>% 
  rename(total_hh = summary_est)

hh_owner_size <- hh_ownership %>% rowwise() %>% 
  mutate(one_person = owner_1E + renter_1E,
         two_people = owner_2E + renter_2E,
         three_people = owner_3E + renter_3E,
         four_people = owner_4E + renter_4E,
         five_people = owner_5E + renter_5E,
         six_people = owner_6E + renter_6E,
         seven_plus_people = owner_7plusE + renter_7plusE,
         .keep = "unused") %>% 
  select(-ends_with("M")) %>% 
  select(-c(summary_moe, owner_totE, renter_totE))

hh_owner_size_g <- hh_owner_size %>% 
  gather("hh_size", "num_hh", 4:10) %>% 
  mutate(prop_hh = num_hh / total_hh,
         hh_size_num = case_when(grepl("one", hh_size) ~ 1,
                                 grepl("two", hh_size) ~ 2,
                                 grepl("three", hh_size) ~ 3,
                                 grepl("four", hh_size) ~ 4,
                                 grepl("five", hh_size) ~ 5,
                                 grepl("six", hh_size) ~ 6,
                                 grepl("seven", hh_size) ~ 7)) %>% 
  mutate(num_ppl = num_hh * hh_size_num) %>% 
  group_by(GEOID) %>% 
  mutate(total_ppl = sum(num_ppl)) %>% 
  ungroup() %>% 
  rowwise() %>% 
  mutate(prop_ppl = num_ppl/total_ppl,
         fips = as.integer(GEOID))


# load raked data
raked_data <- read_csv("data/raking/contact_raking_weights_age_sex_corrected.csv", 
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
                                        weight = col_double(),
                                        updated_weight = col_double(),
                                        did_not_converge = col_integer(),
                                        age_cat_col = col_factor(levels = c("age18to34", "age35to54", 
                                                                            "age55up")))) %>% 
  filter(!is.na(updated_weight)) %>% # remove observations from county-weeks with < 3 responses, <1% of responses
  filter(week < ymd("2021-05-01"))

# group any households 7+
# when people respond to the survey they are saying number including themselves: num_hh
# but we should really be getting a sense of number of households, not number of people 
#   (would have to multiply for that)

# are young people reporting dormmates?

survey_hh_data <- raked_data %>% 
  mutate(hh_size_num = ifelse(num_hh >= 7, 7, as.integer(num_hh)), # rounds down
         hh_size_num = ifelse(hh_size_num == 0, 1, hh_size_num), # assumed 0s didn't include themselves
         fips = ifelse(fips == 2270, 2158, ifelse(fips == 46113, 46102, fips))) %>% 
  group_by(fips, hh_size_num) %>% 
  summarise(count = n(),
            rewt_count = sum(updated_weight)) %>% # otherwise reweighting not accounted for
  ungroup() %>% 
  group_by(fips) %>% 
  mutate(prop = count / sum(count),
         rewt_prop = rewt_count / sum(rewt_count))

combine <- survey_hh_data %>% left_join(hh_owner_size_g %>% select(fips, hh_size_num, prop_hh))

foi <- c(4003, 6031, 12113, 13077, 13199, 13253, 17061, 18179, 19107, 
         20151, 22025, 26085, 27053, 29165, 36097, 37105, 39159, 40023, 
         41017, 41063, 42127, 46007, 47135, 51093, 51163)


samp_size <- combine %>% filter(fips %in% foi) %>% 
  group_by(fips) %>% 
  summarise(tot_samp = sum(count))
#foi <- sample(unique(combine$fips), 25)

fills <- c("CTIS" = "royalblue", "ACS" = "orange")
combine %>%  
  filter(fips %in% foi) %>% 
  ggplot(aes(x = hh_size_num)) +
  geom_bar(aes(y = rewt_prop, fill = "CTIS"), stat = "identity", alpha = 0.5, col = NA) +
  geom_bar(aes(y = prop_hh, fill = "ACS"), stat = "identity", alpha = 0.5, col = NA) +
  scale_x_continuous(breaks = c(1, 2, 3, 4, 5, 6, 7)) +
  labs(x = "household size", y = "frequency", fill = "Dataset") +
  facet_wrap(~fips) +
  scale_fill_manual(values = fills)
ggsave("figures/supp/hh-size-comp-raked-weighted.pdf", height = 8, width = 10)

# make map of mean hh contacts per county versus mean hh size
# take ratio and show that it is close to 1
# from https://covid19.census.gov/datasets/21843f238cbb46b08615fc53e19e0daf/explore
# https://covid19.census.gov/datasets/21843f238cbb46b08615fc53e19e0daf
avg_hh_size <- read_csv("data/input/Average_Household_Size_and_Population_Density_-_County.csv") %>% 
  mutate(fips = as.integer(as.numeric(GEOID))) %>% 
  rename(census_hh_size = B25010_001E) %>% 
  select(fips, census_hh_size)
mean_hh_contact <- raked_data %>% 
  mutate(hh_size_num = ifelse(num_hh >= 7, 7, as.integer(num_hh)), # rounds down
         hh_size_num = ifelse(hh_size_num == 0, 1, hh_size_num), # assumed 0s didn't include themselves
         fips = ifelse(fips == 2270, 2158, ifelse(fips == 46113, 46102, fips))) %>% 
  group_by(fips) %>%
  summarise(mean_hh_size = weighted.mean(hh_size_num, updated_weight),
            samp_size = n()) %>% 
  ungroup() %>% 
  left_join(avg_hh_size) %>% 
  mutate(hh_size_ratio = mean_hh_size/census_hh_size)


map <- CountyChoropleth$new(mean_hh_contact %>% rename(region = fips, value = hh_size_ratio))
map$ggplot_polygon <- geom_polygon(aes(fill = value), color = NA, linewidth = 0.01)
map$set_num_colors(1)
map$ggplot_scale <- scale_fill_gradientn(colors=met.brewer("Benedictus"), 
                                         limits = c(0, 2),
                                         name = "Household size ratio")
map <- map$render()
map    
ggsave("figures/supp/hh-size-ratio-obs-to-census.pdf", height = 5, width = 8)
