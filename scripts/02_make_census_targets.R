# we are using acs data for this
{
  library(tidycensus)
  library(tidyverse)
}
# INSERT CENSUS API KEY
Sys.getenv("CENSUS_API_KEY")

# acs example
# v21 <- load_variables(2021, "acs5", cache = TRUE)
# View(v21)

### EXTRACTING DESIRED ACS DATA ### 
# need age, race, and sex
# denominator to calculate prop of county residents in each category should be population over 18
# FB categories c("18-24", "25-34", "35-44", "45-54", "55-64", "65-74", "75+")
men_by_age <- get_acs(geography = "county", 
                      variables = c(male_18to19 = "B01001_007", male_20 = "B01001_008", male_21 = "B01001_009", 
                                    male_22to24 = "B01001_010", male_25to29 = "B01001_011", male_30to34 = "B01001_012", 
                                    male_35to39 = "B01001_013", male_40to44 = "B01001_014", male_45to49 = "B01001_015",
                                    male_50to54 = "B01001_016", male_55to59 = "B01001_017", male_60and61 = "B01001_018",
                                    male_62to64 = "B01001_019", male_65and66 = "B01001_020", male_67to69 = "B01001_021", 
                                    male_70to74 = "B01001_022", male_75to79 = "B01001_023", male_80to84 = "B01001_024", 
                                    male_85plus = "B01001_025"),
                      output = "wide",
                      year = 2021)

men_by_age_agg <- men_by_age %>% 
  mutate(male_18to24 = male_18to19E + male_20E+ male_21E + male_22to24E,
         male_25to34 =  male_25to29E + male_30to34E,
         male_35to44 = male_35to39E + male_40to44E,
         male_45to54 = male_45to49E + male_50to54E, 
         male_55to64 = male_55to59E + male_60and61E + male_62to64E,
         male_65to74 = male_65and66E + male_67to69E+ male_70to74E,
         male_75up = male_75to79E + male_80to84E + male_85plusE,
         male_18up = male_18to19E + male_20E+ male_21E + male_22to24E + male_25to29E + male_30to34E + male_35to39E + male_40to44E +
           male_45to49E + male_50to54E + male_55to59E + male_60and61E + male_62to64E + male_65and66E + male_67to69E+ male_70to74E +
           male_75to79E + male_80to84E + male_85plusE,
         .keep = "unused") %>% 
  select(-ends_with("M")) #selecting out margin of error estimates for this df

# need male population for each county in age groupds 18-34, 35-54, and 55+ for 
#   poststratification variance calculation
men_by_age_poststrat <- men_by_age_agg %>% 
  mutate(male_18to34 = male_18to24 + male_25to34,
         male_35to54 = male_35to44 + male_45to54,
         male_55up = male_55to64 + male_65to74 + male_75up,
         .keep = "unused")

#Women by age
women_by_age <- get_acs(geography = "county", 
                        variables = c(female_18to19 = "B01001_031", female_20 = "B01001_032", female_21 = "B01001_033", 
                                      female_22to24 = "B01001_034", female_25to29 = "B01001_035", female_30to34 = "B01001_036", 
                                      female_35to39 = "B01001_037", female_40to44 = "B01001_038", female_45to49 = "B01001_039", 
                                      female_50to54 = "B01001_040", female_55to59 = "B01001_041", female_60and61 = "B01001_042", 
                                      female_62to64 = "B01001_043", female_65and66 = "B01001_044", female_67to69 = "B01001_045", 
                                      female_70to74 = "B01001_046", female_75to79 = "B01001_047", female_80to84 = "B01001_048", 
                                      female_85plus = "B01001_049"),
                        output = "wide",
                        year = 2021)

women_by_age_agg <- women_by_age %>% 
  mutate(female_18to24 = female_18to19E + female_20E+ female_21E + female_22to24E,
         female_25to34 =  female_25to29E + female_30to34E,
         female_35to44 = female_35to39E + female_40to44E,
         female_45to54 = female_45to49E + female_50to54E, 
         female_55to64 = female_55to59E + female_60and61E + female_62to64E,
         female_65to74 = female_65and66E + female_67to69E+ female_70to74E,
         female_75up = female_75to79E + female_80to84E + female_85plusE,
         female_18up = female_18to19E + female_20E+ female_21E + female_22to24E + female_25to29E + female_30to34E + female_35to39E + female_40to44E +
           female_45to49E + female_50to54E + female_55to59E + female_60and61E + female_62to64E + female_65and66E + female_67to69E+ female_70to74E +
           female_75to79E + female_80to84E + female_85plusE,
         .keep = "unused") %>% 
  select(-ends_with("M")) #selecting out margin of error estimates for this df

women_by_age_poststrat <- women_by_age_agg %>% 
  mutate(female_18to34 = female_18to24 + female_25to34,
         female_35to54 = female_35to44 + female_45to54,
         female_55up = female_55to64 + female_65to74 + female_75up,
         .keep = "unused")

poststrat_data <- men_by_age_poststrat %>% left_join(women_by_age_poststrat) %>% 
  mutate(all_18up = male_18up + female_18up)
write_csv(poststrat_data, "data/output/acs_data_for_poststrat_var_calc.csv")

#Male and female separated age distributions
age_by_sex <- men_by_age_agg %>%
  left_join(women_by_age_agg)

#Total in each age group, percentage of population in that age group
age_dist <- age_by_sex %>% 
  mutate(all_18to24 = female_18to24 + male_18to24,
         all_25to34 = female_25to34 + male_25to34,
         all_35to44 = female_35to44 + male_35to44,
         all_45to54 = female_45to54 + male_45to54, 
         all_55to64 = female_55to64 + male_55to64,
         all_65to74 = female_65to74 + male_65to74,
         all_75up = female_75up + male_75up,
         all_18up = female_18up + male_18up,
         prop_18to24 = all_18to24 / all_18up,
         prop_25to34 = all_25to34 / all_18up,
         prop_35to44 = all_35to44 / all_18up,
         prop_45to54 = all_45to54 / all_18up, 
         prop_55to64 = all_55to64 / all_18up,
         prop_65to74 = all_65to74 / all_18up,
         prop_75up = all_75up / all_18up,
         .keep = "unused")

sex_dist <- age_by_sex %>% 
  mutate(male = male_18to24 + male_25to34 + male_35to44 + male_45to54 + male_55to64 + male_65to74 + male_75up,
         female = female_18to24 + female_25to34 + female_35to44 + female_45to54 + female_55to64 + female_65to74 + female_75up,
         all_18up = female_18up + male_18up,
         prop_male = male/all_18up,
         prop_female = female/all_18up,
         .keep = "unused")


# education attainment for pop 25+ or sex by educational attainment for pop 25+ available but i think we need 18+ to match fb which has sex and age
education_by_age_by_sex <- get_acs(geography = "county",
                                   variables = c(m_18to24_lt_9th = "B15001_004", m_18to24_lt_dip = "B15001_005",
                                                 m_18to24_hs = "B15001_006", m_18to24_lt_coll = "B15001_007",
                                                 m_18to24_2yrcoll = "B15001_008", m_18to24_4yrcoll = "B15001_009",
                                                 m_18to24_gt_coll = "B15001_010", m_25to34_lt_9th = "B15001_012",
                                                 m_25to34_lt_dip = "B15001_013", m_25to34_hs = "B15001_014",
                                                 m_25to34_lt_coll = "B15001_015", m_25to34_2yrcoll = "B15001_016",
                                                 m_25to34_4yrcoll = "B15001_017", m_25to34_gt_coll = "B15001_018",
                                                 m_35to44_lt_9th = "B15001_020", m_35to44_lt_dip = "B15001_021",
                                                 m_35to44_hs = "B15001_022", m_35to44_lt_coll = "B15001_023",
                                                 m_35to44_2yrcoll = "B15001_024", m_35to44_4yrcoll = "B15001_025",
                                                 m_35to44_gt_coll = "B15001_026", m_45to64_lt_9th = "B15001_028",
                                                 m_45to64_lt_dip = "B15001_029", m_45to64_hs = "B15001_030",
                                                 m_45to64_lt_coll = "B15001_031", m_45to64_2yrcoll = "B15001_032",
                                                 m_45to64_4yrcoll = "B15001_033", m_45to64_gt_coll = "B15001_034",
                                                 m_65up_lt_9th = "B15001_036", m_65up_lt_dip = "B15001_037",
                                                 m_65up_hs = "B15001_038", m_65up_lt_coll = "B15001_039",
                                                 m_65up_2yrcoll = "B15001_040", m_65up_4yrcoll = "B15001_041",
                                                 m_65up_gt_coll = "B15001_042", f_18to24_lt_9th = "B15001_045",
                                                 f_18to24_lt_dip = "B15001_046", f_18to24_hs = "B15001_047",
                                                 f_18to24_lt_coll = "B15001_048", f_18to24_2yrcoll = "B15001_049",
                                                 f_18to24_4yrcoll = "B15001_050", f_18to24_gt_coll = "B15001_051",
                                                 f_25to34_lt_9th = "B15001_053", f_25to34_lt_dip = "B15001_054",
                                                 f_25to34_hs = "B15001_055", f_25to34_lt_coll = "B15001_056",
                                                 f_25to34_2yrcoll = "B15001_057", f_25to34_4yrcoll = "B15001_058",
                                                 f_25to34_gt_coll = "B15001_059", f_35to44_lt_9th = "B15001_061",
                                                 f_35to44_lt_dip = "B15001_062", f_35to44_hs = "B15001_063",
                                                 f_35to44_lt_coll = "B15001_064", f_35to44_2yrcoll = "B15001_065",
                                                 f_35to44_4yrcoll = "B15001_066", f_35to44_gt_coll = "B15001_067",
                                                 f_45to64_lt_9th = "B15001_069", f_45to64_lt_dip = "B15001_070",
                                                 f_45to64_hs = "B15001_071", f_45to64_lt_coll = "B15001_072",
                                                 f_45to64_2yrcoll = "B15001_073", f_45to64_4yrcoll = "B15001_074",
                                                 f_45to64_gt_coll = "B15001_075", f_65up_lt_9th = "B15001_077",
                                                 f_65up_lt_dip = "B15001_078", f_65up_hs = "B15001_079",
                                                 f_65up_lt_coll = "B15001_080", f_65up_2yrcoll = "B15001_081",
                                                 f_65up_4yrcoll = "B15001_082", f_65up_gt_coll = "B15001_083"),
                                   output = "wide",
                                   year = 2021)

education_agg <- education_by_age_by_sex %>% 
  mutate(edu_lt_hs = m_18to24_lt_9thE + m_18to24_lt_dipE + m_25to34_lt_9thE + m_25to34_lt_dipE + m_35to44_lt_9thE + m_35to44_lt_dipE +
           m_45to64_lt_9thE + m_45to64_lt_dipE + m_65up_lt_9thE + m_65up_lt_dipE + f_18to24_lt_9thE + f_18to24_lt_dipE +
           f_25to34_lt_9thE + f_25to34_lt_dipE + f_35to44_lt_9thE + f_35to44_lt_dipE + f_45to64_lt_9thE + f_45to64_lt_dipE + 
           f_65up_lt_9thE + f_65up_lt_dipE,
         edu_hs = m_18to24_hsE + m_25to34_hsE + m_35to44_hsE + m_45to64_hsE + m_65up_hsE +
           f_18to24_hsE + f_25to34_hsE + f_35to44_hsE + f_45to64_hsE + f_65up_hsE,
         edu_lt_coll = m_18to24_lt_collE + m_25to34_lt_collE + m_35to44_lt_collE + m_45to64_lt_collE + m_65up_lt_collE + 
           f_18to24_lt_collE + f_25to34_lt_collE + f_35to44_lt_collE + f_45to64_lt_collE + f_65up_lt_collE,
         edu_2yrcoll = m_18to24_2yrcollE + m_25to34_2yrcollE + m_35to44_2yrcollE + m_45to64_2yrcollE + m_65up_2yrcollE +
           f_18to24_2yrcollE + f_25to34_2yrcollE + f_35to44_2yrcollE + f_45to64_2yrcollE + f_65up_2yrcollE,
         edu_4yrcoll = m_18to24_4yrcollE + m_25to34_4yrcollE + m_35to44_4yrcollE + m_45to64_4yrcollE + m_65up_4yrcollE +
           f_18to24_4yrcollE + f_25to34_4yrcollE + f_35to44_4yrcollE + f_45to64_4yrcollE + f_65up_4yrcollE,
         edu_gtcoll = m_18to24_gt_collE + m_25to34_gt_collE + m_35to44_gt_collE + m_45to64_gt_collE + m_65up_gt_collE +
           f_18to24_gt_collE + f_25to34_gt_collE + f_35to44_gt_collE + f_45to64_gt_collE + f_65up_gt_collE,
         .keep = "unused") %>% 
  select(-ends_with('M'))


# now need to get 18up denominator into this dataset and race
denom <- sex_dist %>% select(GEOID, all_18up)
education_dist <- education_agg %>% left_join(denom, by = c("GEOID")) %>% 
  mutate(prop_lt_hs = edu_lt_hs/all_18up,
         prop_hs = edu_hs/all_18up,
         prop_lt_coll = edu_lt_coll/all_18up,
         prop_2yrcoll = edu_2yrcoll/all_18up,
         prop_4yrcoll = edu_4yrcoll/all_18up,
         prop_gt_coll = edu_gtcoll/all_18up)


# lastly need race and ethnicity
# we need only 18+
race_by_age <- get_acs(geography = "county",
                       variables = c(
                         # hispanic men
                         male_18to19_hisp = "B01001I_007", male_20to24_hisp = "B01001I_008", male_25to29_hisp = "B01001I_009",
                         male_30to34_hisp = "B01001I_010", male_35to44_hisp = "B01001I_011", male_45to54_hisp = "B01001I_012",
                         male_55to64_hisp = "B01001I_013", male_65to74_hisp = "B01001I_014", male_75to84_hisp = "B01001I_015",
                         male_85plus_hisp = "B01001I_016",
                         # hispanic women
                         female_18to19_hisp = "B01001I_022", female_20to24_hisp = "B01001I_023", female_25to29_hisp = "B01001I_024",
                         female_30to34_hisp = "B01001I_025", female_35to44_hisp = "B01001I_026", female_45to54_hisp = "B01001I_027",
                         female_55to64_hisp = "B01001I_028", female_65to74_hisp = "B01001I_029", female_75to84_hisp = "B01001I_030",
                         female_85plus_hisp = "B01001I_031",
                         # white, non-hispanic men
                         male_18to19_white = "B01001H_007", male_20to24_white = "B01001H_008", male_25to29_white = "B01001H_009",
                         male_30to34_white = "B01001H_010", male_35to44_white = "B01001H_011", male_45to54_white = "B01001H_012",
                         male_55to64_white = "B01001H_013", male_65to74_white = "B01001H_014", male_75to84_white = "B01001H_015",
                         male_85plus_white = "B01001H_016",
                         # white, non-hispanic women
                         female_18to19_white = "B01001H_022", female_20to24_white = "B01001H_023", female_25to29_white = "B01001H_024",
                         female_30to34_white = "B01001H_025", female_35to44_white = "B01001H_026", female_45to54_white = "B01001H_027",
                         female_55to64_white = "B01001H_028", female_65to74_white = "B01001H_029", female_75to84_white = "B01001H_030",
                         female_85plus_white = "B01001H_031",
                         # black men
                         male_18to19_black = "B01001B_007", male_20to24_black = "B01001B_008", male_25to29_black = "B01001B_009",
                         male_30to34_black = "B01001B_010", male_35to44_black = "B01001B_011", male_45to54_black = "B01001B_012",
                         male_55to64_black = "B01001B_013", male_65to74_black = "B01001B_014", male_75to84_black = "B01001B_015",
                         male_85plus_black = "B01001B_016",
                         # black women
                         female_18to19_black = "B01001B_022", female_20to24_black = "B01001B_023", female_25to29_black = "B01001B_024",
                         female_30to34_black = "B01001B_025", female_35to44_black = "B01001B_026", female_45to54_black = "B01001B_027",
                         female_55to64_black = "B01001B_028", female_65to74_black = "B01001B_029", female_75to84_black = "B01001B_030",
                         female_85plus_black = "B01001B_031",
                         # AmerIndian AKNative men
                         male_18to19_aian = "B01001C_007", male_20to24_aian = "B01001C_008", male_25to29_aian = "B01001C_009",
                         male_30to34_aian = "B01001C_010", male_35to44_aian = "B01001C_011", male_45to54_aian = "B01001C_012",
                         male_55to64_aian = "B01001C_013", male_65to74_aian = "B01001C_014", male_75to84_aian = "B01001C_015",
                         male_85plus_aian = "B01001C_016",
                         # AmerIndian AKNative women
                         female_18to19_aian = "B01001C_022", female_20to24_aian = "B01001C_023", female_25to29_aian = "B01001C_024",
                         female_30to34_aian = "B01001C_025", female_35to44_aian = "B01001C_026", female_45to54_aian = "B01001C_027",
                         female_55to64_aian = "B01001C_028", female_65to74_aian = "B01001C_029", female_75to84_aian = "B01001C_030",
                         female_85plus_aian = "B01001C_031",
                         # asian men
                         male_18to19_asian = "B01001D_007", male_20to24_asian = "B01001D_008", male_25to29_asian = "B01001D_009",
                         male_30to34_asian = "B01001D_010", male_35to44_asian = "B01001D_011", male_45to54_asian = "B01001D_012",
                         male_55to64_asian = "B01001D_013", male_65to74_asian = "B01001D_014", male_75to84_asian = "B01001D_015",
                         male_85plus_asian = "B01001D_016",
                         # asian women
                         female_18to19_asian = "B01001D_022", female_20to24_asian = "B01001D_023", female_25to29_asian = "B01001D_024",
                         female_30to34_asian = "B01001D_025", female_35to44_asian = "B01001D_026", female_45to54_asian = "B01001D_027",
                         female_55to64_asian = "B01001D_028", female_65to74_asian = "B01001D_029", female_75to84_asian = "B01001D_030",
                         female_85plus_asian = "B01001D_031",
                         # NativeHawaiian and PacificIslander men
                         male_18to19_nhpi = "B01001E_007", male_20to24_nhpi = "B01001E_008", male_25to29_nhpi = "B01001E_009",
                         male_30to34_nhpi = "B01001E_010", male_35to44_nhpi = "B01001E_011", male_45to54_nhpi = "B01001E_012",
                         male_55to64_nhpi = "B01001E_013", male_65to74_nhpi = "B01001E_014", male_75to84_nhpi = "B01001E_015",
                         male_85plus_nhpi = "B01001E_016",
                         # NativeHawaiian and PacificIslander women
                         female_18to19_nhpi = "B01001E_022", female_20to24_nhpi = "B01001E_023", female_25to29_nhpi = "B01001E_024",
                         female_30to34_nhpi = "B01001E_025", female_35to44_nhpi = "B01001E_026", female_45to54_nhpi = "B01001E_027",
                         female_55to64_nhpi = "B01001E_028", female_65to74_nhpi = "B01001E_029", female_75to84_nhpi = "B01001E_030",
                         female_85plus_nhpi = "B01001E_031",
                         # other men
                         male_18to19_other = "B01001F_007", male_20to24_other = "B01001F_008", male_25to29_other = "B01001F_009",
                         male_30to34_other = "B01001F_010", male_35to44_other = "B01001F_011", male_45to54_other = "B01001F_012",
                         male_55to64_other = "B01001F_013", male_65to74_other = "B01001F_014", male_75to84_other = "B01001F_015",
                         male_85plus_other = "B01001F_016",
                         # other women
                         female_18to19_other = "B01001F_022", female_20to24_other = "B01001F_023", female_25to29_other = "B01001F_024",
                         female_30to34_other = "B01001F_025", female_35to44_other = "B01001F_026", female_45to54_other = "B01001F_027",
                         female_55to64_other = "B01001F_028", female_65to74_other = "B01001F_029", female_75to84_other = "B01001F_030",
                         female_85plus_other = "B01001F_031",
                         # two or more men (check this doesn't overlap with other?)
                         male_18to19_twoplus = "B01001G_007", male_20to24_twoplus = "B01001G_008", male_25to29_twoplus = "B01001G_009",
                         male_30to34_twoplus = "B01001G_010", male_35to44_twoplus = "B01001G_011", male_45to54_twoplus = "B01001G_012",
                         male_55to64_twoplus = "B01001G_013", male_65to74_twoplus = "B01001G_014", male_75to84_twoplus = "B01001G_015",
                         male_85plus_twoplus = "B01001G_016",
                         # two or more women
                         female_18to19_twoplus = "B01001G_022", female_20to24_twoplus = "B01001G_023", female_25to29_twoplus = "B01001G_024",
                         female_30to34_twoplus = "B01001G_025", female_35to44_twoplus = "B01001G_026", female_45to54_twoplus = "B01001G_027",
                         female_55to64_twoplus = "B01001G_028", female_65to74_twoplus = "B01001G_029", female_75to84_twoplus = "B01001G_030",
                         female_85plus_twoplus = "B01001G_031"),
                       output = "wide",
                       year = 2021)

# now sum up within single race categories, since we aren't interested in age breakdown, just needed over 18
race_agg_by_sex <- race_by_age %>%
  mutate(hisp_men = male_18to19_hispE + male_20to24_hispE + male_25to29_hispE + male_30to34_hispE + male_35to44_hispE +
           male_45to54_hispE + male_55to64_hispE + male_65to74_hispE + male_75to84_hispE + male_85plus_hispE,
         hisp_women = female_18to19_hispE + female_20to24_hispE + female_25to29_hispE + female_30to34_hispE + female_35to44_hispE +
           female_45to54_hispE + female_55to64_hispE + female_65to74_hispE + female_75to84_hispE + female_85plus_hispE,
         white_men = male_18to19_whiteE + male_20to24_whiteE + male_25to29_whiteE + male_30to34_whiteE + male_35to44_whiteE +
           male_45to54_whiteE + male_55to64_whiteE + male_65to74_whiteE + male_75to84_whiteE + male_85plus_whiteE,
         white_women = female_18to19_whiteE + female_20to24_whiteE + female_25to29_whiteE + female_30to34_whiteE + female_35to44_whiteE +
           female_45to54_whiteE + female_55to64_whiteE + female_65to74_whiteE + female_75to84_whiteE + female_85plus_whiteE,
         black_men = male_18to19_blackE + male_20to24_blackE + male_25to29_blackE + male_30to34_blackE + male_35to44_blackE +
           male_45to54_blackE + male_55to64_blackE + male_65to74_blackE + male_75to84_blackE + male_85plus_blackE,
         black_women = female_18to19_blackE + female_20to24_blackE + female_25to29_blackE + female_30to34_blackE + female_35to44_blackE +
           female_45to54_blackE + female_55to64_blackE + female_65to74_blackE + female_75to84_blackE + female_85plus_blackE,
         aian_men = male_18to19_aianE + male_20to24_aianE + male_25to29_aianE + male_30to34_aianE + male_35to44_aianE +
           male_45to54_aianE + male_55to64_aianE + male_65to74_aianE + male_75to84_aianE + male_85plus_aianE,
         aian_women = female_18to19_aianE + female_20to24_aianE + female_25to29_aianE + female_30to34_aianE + female_35to44_aianE +
           female_45to54_aianE + female_55to64_aianE + female_65to74_aianE + female_75to84_aianE + female_85plus_aianE,
         asian_men = male_18to19_asianE + male_20to24_asianE + male_25to29_asianE + male_30to34_asianE + male_35to44_asianE +
           male_45to54_asianE + male_55to64_asianE + male_65to74_asianE + male_75to84_asianE + male_85plus_asianE,
         asian_women = female_18to19_asianE + female_20to24_asianE + female_25to29_asianE + female_30to34_asianE + female_35to44_asianE +
           female_45to54_asianE + female_55to64_asianE + female_65to74_asianE + female_75to84_asianE + female_85plus_asianE,
         nhpi_men = male_18to19_nhpiE + male_20to24_nhpiE + male_25to29_nhpiE + male_30to34_nhpiE + male_35to44_nhpiE +
           male_45to54_nhpiE + male_55to64_nhpiE + male_65to74_nhpiE + male_75to84_nhpiE + male_85plus_nhpiE,
         nhpi_women = female_18to19_nhpiE + female_20to24_nhpiE + female_25to29_nhpiE + female_30to34_nhpiE + female_35to44_nhpiE +
           female_45to54_nhpiE + female_55to64_nhpiE + female_65to74_nhpiE + female_75to84_nhpiE + female_85plus_nhpiE,
         other_men = male_18to19_otherE + male_20to24_otherE + male_25to29_otherE + male_30to34_otherE + male_35to44_otherE +
           male_45to54_otherE + male_55to64_otherE + male_65to74_otherE + male_75to84_otherE + male_85plus_otherE,
         other_women = female_18to19_otherE + female_20to24_otherE + female_25to29_otherE + female_30to34_otherE + female_35to44_otherE +
           female_45to54_otherE + female_55to64_otherE + female_65to74_otherE + female_75to84_otherE + female_85plus_otherE,
         twoplus_men = male_18to19_twoplusE + male_20to24_twoplusE + male_25to29_twoplusE + male_30to34_twoplusE + male_35to44_twoplusE +
           male_45to54_twoplusE + male_55to64_twoplusE + male_65to74_twoplusE + male_75to84_twoplusE + male_85plus_twoplusE,
         twoplus_women = female_18to19_twoplusE + female_20to24_twoplusE + female_25to29_twoplusE + female_30to34_twoplusE + female_35to44_twoplusE +
           female_45to54_twoplusE + female_55to64_twoplusE + female_65to74_twoplusE + female_75to84_twoplusE + female_85plus_twoplusE)

race_agg <- race_agg_by_sex %>%
  transmute(GEOID = GEOID,
            name = NAME,
            hisp = hisp_men + hisp_women,
            white = white_men + white_women,
            black = black_men + black_women,
            aian = aian_men + aian_women,
            asian = asian_men + asian_women,
            nhpi = nhpi_men + nhpi_women,
            other = other_men + other_women,
            twoplus = twoplus_men + twoplus_women,
            fourcat = aian + asian + nhpi + other + twoplus,
            fivecat = aian + nhpi + other + twoplus)

# now need to get 18up denominator into this dataset
race_dist <- race_agg %>% left_join(denom, by = c("GEOID")) %>%
  mutate(prop_hisp = hisp/all_18up,
         prop_white = white/all_18up,
         prop_black = black/all_18up,
         prop_asian = asian/all_18up,
         prop_fourcat = fourcat/all_18up,
         prop_fivecat = fivecat/all_18up)


#### COMBINE ALL TOGETHER ####
all_acs <- age_dist %>% 
  left_join(sex_dist) %>% 
  left_join(education_dist) %>% 
  left_join(race_dist) %>% 
  mutate(fips = as.integer(GEOID)) %>% 
  select(fips, NAME, all_18up, contains("prop"))
#counts has the counts as well
write_csv(all_acs, "data/output/acs_target_data_for_contact_raking.csv")

#### BY STATE NOT COUNTY ####
### EXTRACTING DESIRED ACS DATA ### 
# need age, race, and sex
# denominator to calculate prop of county residents in each category should be population over 18
# FB categories c("18-24", "25-34", "35-44", "45-54", "55-64", "65-74", "75+")
men_by_age <- get_acs(geography = "state", 
                      variables = c(male_18to19 = "B01001_007", male_20 = "B01001_008", male_21 = "B01001_009", 
                                    male_22to24 = "B01001_010", male_25to29 = "B01001_011", male_30to34 = "B01001_012", 
                                    male_35to39 = "B01001_013", male_40to44 = "B01001_014", male_45to49 = "B01001_015",
                                    male_50to54 = "B01001_016", male_55to59 = "B01001_017", male_60and61 = "B01001_018",
                                    male_62to64 = "B01001_019", male_65and66 = "B01001_020", male_67to69 = "B01001_021", 
                                    male_70to74 = "B01001_022", male_75to79 = "B01001_023", male_80to84 = "B01001_024", 
                                    male_85plus = "B01001_025"),
                      output = "wide",
                      year = 2021)

men_by_age_agg <- men_by_age %>% 
  mutate(male_18to24 = male_18to19E + male_20E+ male_21E + male_22to24E,
         male_25to34 =  male_25to29E + male_30to34E,
         male_35to44 = male_35to39E + male_40to44E,
         male_45to54 = male_45to49E + male_50to54E, 
         male_55to64 = male_55to59E + male_60and61E + male_62to64E,
         male_65to74 = male_65and66E + male_67to69E+ male_70to74E,
         male_75up = male_75to79E + male_80to84E + male_85plusE,
         male_18up = male_18to19E + male_20E+ male_21E + male_22to24E + male_25to29E + male_30to34E + male_35to39E + male_40to44E +
           male_45to49E + male_50to54E + male_55to59E + male_60and61E + male_62to64E + male_65and66E + male_67to69E+ male_70to74E +
           male_75to79E + male_80to84E + male_85plusE,
         .keep = "unused") %>% 
  select(-ends_with("M")) #selecting out margin of error estimates for this df

#Women by age
women_by_age <- get_acs(geography = "state", 
                        variables = c(female_18to19 = "B01001_031", female_20 = "B01001_032", female_21 = "B01001_033", 
                                      female_22to24 = "B01001_034", female_25to29 = "B01001_035", female_30to34 = "B01001_036", 
                                      female_35to39 = "B01001_037", female_40to44 = "B01001_038", female_45to49 = "B01001_039", 
                                      female_50to54 = "B01001_040", female_55to59 = "B01001_041", female_60and61 = "B01001_042", 
                                      female_62to64 = "B01001_043", female_65and66 = "B01001_044", female_67to69 = "B01001_045", 
                                      female_70to74 = "B01001_046", female_75to79 = "B01001_047", female_80to84 = "B01001_048", 
                                      female_85plus = "B01001_049"),
                        output = "wide",
                        year = 2021)

women_by_age_agg <- women_by_age %>% 
  mutate(female_18to24 = female_18to19E + female_20E+ female_21E + female_22to24E,
         female_25to34 =  female_25to29E + female_30to34E,
         female_35to44 = female_35to39E + female_40to44E,
         female_45to54 = female_45to49E + female_50to54E, 
         female_55to64 = female_55to59E + female_60and61E + female_62to64E,
         female_65to74 = female_65and66E + female_67to69E+ female_70to74E,
         female_75up = female_75to79E + female_80to84E + female_85plusE,
         female_18up = female_18to19E + female_20E+ female_21E + female_22to24E + female_25to29E + female_30to34E + female_35to39E + female_40to44E +
           female_45to49E + female_50to54E + female_55to59E + female_60and61E + female_62to64E + female_65and66E + female_67to69E+ female_70to74E +
           female_75to79E + female_80to84E + female_85plusE,
         .keep = "unused") %>% 
  select(-ends_with("M")) #selecting out margin of error estimates for this df

#Male and female separated age distributions
age_by_sex <- men_by_age_agg %>%
  left_join(women_by_age_agg)

#Total in each age group, percentage of population in that age group
age_dist <- age_by_sex %>% 
  mutate(all_18to24 = female_18to24 + male_18to24,
         all_25to34 = female_25to34 + male_25to34,
         all_35to44 = female_35to44 + male_35to44,
         all_45to54 = female_45to54 + male_45to54, 
         all_55to64 = female_55to64 + male_55to64,
         all_65to74 = female_65to74 + male_65to74,
         all_75up = female_75up + male_75up,
         all_18up = female_18up + male_18up,
         prop_18to24 = all_18to24 / all_18up,
         prop_25to34 = all_25to34 / all_18up,
         prop_35to44 = all_35to44 / all_18up,
         prop_45to54 = all_45to54 / all_18up, 
         prop_55to64 = all_55to64 / all_18up,
         prop_65to74 = all_65to74 / all_18up,
         prop_75up = all_75up / all_18up,
         .keep = "unused")

sex_dist <- age_by_sex %>% 
  mutate(male = male_18to24 + male_25to34 + male_35to44 + male_45to54 + male_55to64 + male_65to74 + male_75up,
         female = female_18to24 + female_25to34 + female_35to44 + female_45to54 + female_55to64 + female_65to74 + female_75up,
         all_18up = female_18up + male_18up,
         prop_male = male/all_18up,
         prop_female = female/all_18up,
         .keep = "unused")

# now need to get 18up denominator into this dataset and race
denom <- sex_dist %>% select(GEOID, all_18up)

# lastly need race and ethnicity
# we need only 18+
race_by_age <- get_acs(geography = "state",
                       variables = c(
                         # hispanic men
                         male_18to19_hisp = "B01001I_007", male_20to24_hisp = "B01001I_008", male_25to29_hisp = "B01001I_009",
                         male_30to34_hisp = "B01001I_010", male_35to44_hisp = "B01001I_011", male_45to54_hisp = "B01001I_012",
                         male_55to64_hisp = "B01001I_013", male_65to74_hisp = "B01001I_014", male_75to84_hisp = "B01001I_015",
                         male_85plus_hisp = "B01001I_016",
                         # hispanic women
                         female_18to19_hisp = "B01001I_022", female_20to24_hisp = "B01001I_023", female_25to29_hisp = "B01001I_024",
                         female_30to34_hisp = "B01001I_025", female_35to44_hisp = "B01001I_026", female_45to54_hisp = "B01001I_027",
                         female_55to64_hisp = "B01001I_028", female_65to74_hisp = "B01001I_029", female_75to84_hisp = "B01001I_030",
                         female_85plus_hisp = "B01001I_031",
                         # white, non-hispanic men
                         male_18to19_white = "B01001H_007", male_20to24_white = "B01001H_008", male_25to29_white = "B01001H_009",
                         male_30to34_white = "B01001H_010", male_35to44_white = "B01001H_011", male_45to54_white = "B01001H_012",
                         male_55to64_white = "B01001H_013", male_65to74_white = "B01001H_014", male_75to84_white = "B01001H_015",
                         male_85plus_white = "B01001H_016",
                         # white, non-hispanic women
                         female_18to19_white = "B01001H_022", female_20to24_white = "B01001H_023", female_25to29_white = "B01001H_024",
                         female_30to34_white = "B01001H_025", female_35to44_white = "B01001H_026", female_45to54_white = "B01001H_027",
                         female_55to64_white = "B01001H_028", female_65to74_white = "B01001H_029", female_75to84_white = "B01001H_030",
                         female_85plus_white = "B01001H_031",
                         # black men
                         male_18to19_black = "B01001B_007", male_20to24_black = "B01001B_008", male_25to29_black = "B01001B_009",
                         male_30to34_black = "B01001B_010", male_35to44_black = "B01001B_011", male_45to54_black = "B01001B_012",
                         male_55to64_black = "B01001B_013", male_65to74_black = "B01001B_014", male_75to84_black = "B01001B_015",
                         male_85plus_black = "B01001B_016",
                         # black women
                         female_18to19_black = "B01001B_022", female_20to24_black = "B01001B_023", female_25to29_black = "B01001B_024",
                         female_30to34_black = "B01001B_025", female_35to44_black = "B01001B_026", female_45to54_black = "B01001B_027",
                         female_55to64_black = "B01001B_028", female_65to74_black = "B01001B_029", female_75to84_black = "B01001B_030",
                         female_85plus_black = "B01001B_031",
                         # AmerIndian AKNative men
                         male_18to19_aian = "B01001C_007", male_20to24_aian = "B01001C_008", male_25to29_aian = "B01001C_009",
                         male_30to34_aian = "B01001C_010", male_35to44_aian = "B01001C_011", male_45to54_aian = "B01001C_012",
                         male_55to64_aian = "B01001C_013", male_65to74_aian = "B01001C_014", male_75to84_aian = "B01001C_015",
                         male_85plus_aian = "B01001C_016",
                         # AmerIndian AKNative women
                         female_18to19_aian = "B01001C_022", female_20to24_aian = "B01001C_023", female_25to29_aian = "B01001C_024",
                         female_30to34_aian = "B01001C_025", female_35to44_aian = "B01001C_026", female_45to54_aian = "B01001C_027",
                         female_55to64_aian = "B01001C_028", female_65to74_aian = "B01001C_029", female_75to84_aian = "B01001C_030",
                         female_85plus_aian = "B01001C_031",
                         # asian men
                         male_18to19_asian = "B01001D_007", male_20to24_asian = "B01001D_008", male_25to29_asian = "B01001D_009",
                         male_30to34_asian = "B01001D_010", male_35to44_asian = "B01001D_011", male_45to54_asian = "B01001D_012",
                         male_55to64_asian = "B01001D_013", male_65to74_asian = "B01001D_014", male_75to84_asian = "B01001D_015",
                         male_85plus_asian = "B01001D_016",
                         # asian women
                         female_18to19_asian = "B01001D_022", female_20to24_asian = "B01001D_023", female_25to29_asian = "B01001D_024",
                         female_30to34_asian = "B01001D_025", female_35to44_asian = "B01001D_026", female_45to54_asian = "B01001D_027",
                         female_55to64_asian = "B01001D_028", female_65to74_asian = "B01001D_029", female_75to84_asian = "B01001D_030",
                         female_85plus_asian = "B01001D_031",
                         # NativeHawaiian and PacificIslander men
                         male_18to19_nhpi = "B01001E_007", male_20to24_nhpi = "B01001E_008", male_25to29_nhpi = "B01001E_009",
                         male_30to34_nhpi = "B01001E_010", male_35to44_nhpi = "B01001E_011", male_45to54_nhpi = "B01001E_012",
                         male_55to64_nhpi = "B01001E_013", male_65to74_nhpi = "B01001E_014", male_75to84_nhpi = "B01001E_015",
                         male_85plus_nhpi = "B01001E_016",
                         # NativeHawaiian and PacificIslander women
                         female_18to19_nhpi = "B01001E_022", female_20to24_nhpi = "B01001E_023", female_25to29_nhpi = "B01001E_024",
                         female_30to34_nhpi = "B01001E_025", female_35to44_nhpi = "B01001E_026", female_45to54_nhpi = "B01001E_027",
                         female_55to64_nhpi = "B01001E_028", female_65to74_nhpi = "B01001E_029", female_75to84_nhpi = "B01001E_030",
                         female_85plus_nhpi = "B01001E_031",
                         # other men
                         male_18to19_other = "B01001F_007", male_20to24_other = "B01001F_008", male_25to29_other = "B01001F_009",
                         male_30to34_other = "B01001F_010", male_35to44_other = "B01001F_011", male_45to54_other = "B01001F_012",
                         male_55to64_other = "B01001F_013", male_65to74_other = "B01001F_014", male_75to84_other = "B01001F_015",
                         male_85plus_other = "B01001F_016",
                         # other women
                         female_18to19_other = "B01001F_022", female_20to24_other = "B01001F_023", female_25to29_other = "B01001F_024",
                         female_30to34_other = "B01001F_025", female_35to44_other = "B01001F_026", female_45to54_other = "B01001F_027",
                         female_55to64_other = "B01001F_028", female_65to74_other = "B01001F_029", female_75to84_other = "B01001F_030",
                         female_85plus_other = "B01001F_031",
                         # two or more men (check this doesn't overlap with other?)
                         male_18to19_twoplus = "B01001G_007", male_20to24_twoplus = "B01001G_008", male_25to29_twoplus = "B01001G_009",
                         male_30to34_twoplus = "B01001G_010", male_35to44_twoplus = "B01001G_011", male_45to54_twoplus = "B01001G_012",
                         male_55to64_twoplus = "B01001G_013", male_65to74_twoplus = "B01001G_014", male_75to84_twoplus = "B01001G_015",
                         male_85plus_twoplus = "B01001G_016",
                         # two or more women
                         female_18to19_twoplus = "B01001G_022", female_20to24_twoplus = "B01001G_023", female_25to29_twoplus = "B01001G_024",
                         female_30to34_twoplus = "B01001G_025", female_35to44_twoplus = "B01001G_026", female_45to54_twoplus = "B01001G_027",
                         female_55to64_twoplus = "B01001G_028", female_65to74_twoplus = "B01001G_029", female_75to84_twoplus = "B01001G_030",
                         female_85plus_twoplus = "B01001G_031"),
                       output = "wide",
                       year = 2021)

# now sum up within single race categories, since we aren't interested in age breakdown, just needed over 18
race_agg_by_sex <- race_by_age %>%
  mutate(hisp_men = male_18to19_hispE + male_20to24_hispE + male_25to29_hispE + male_30to34_hispE + male_35to44_hispE +
           male_45to54_hispE + male_55to64_hispE + male_65to74_hispE + male_75to84_hispE + male_85plus_hispE,
         hisp_women = female_18to19_hispE + female_20to24_hispE + female_25to29_hispE + female_30to34_hispE + female_35to44_hispE +
           female_45to54_hispE + female_55to64_hispE + female_65to74_hispE + female_75to84_hispE + female_85plus_hispE,
         white_men = male_18to19_whiteE + male_20to24_whiteE + male_25to29_whiteE + male_30to34_whiteE + male_35to44_whiteE +
           male_45to54_whiteE + male_55to64_whiteE + male_65to74_whiteE + male_75to84_whiteE + male_85plus_whiteE,
         white_women = female_18to19_whiteE + female_20to24_whiteE + female_25to29_whiteE + female_30to34_whiteE + female_35to44_whiteE +
           female_45to54_whiteE + female_55to64_whiteE + female_65to74_whiteE + female_75to84_whiteE + female_85plus_whiteE,
         black_men = male_18to19_blackE + male_20to24_blackE + male_25to29_blackE + male_30to34_blackE + male_35to44_blackE +
           male_45to54_blackE + male_55to64_blackE + male_65to74_blackE + male_75to84_blackE + male_85plus_blackE,
         black_women = female_18to19_blackE + female_20to24_blackE + female_25to29_blackE + female_30to34_blackE + female_35to44_blackE +
           female_45to54_blackE + female_55to64_blackE + female_65to74_blackE + female_75to84_blackE + female_85plus_blackE,
         aian_men = male_18to19_aianE + male_20to24_aianE + male_25to29_aianE + male_30to34_aianE + male_35to44_aianE +
           male_45to54_aianE + male_55to64_aianE + male_65to74_aianE + male_75to84_aianE + male_85plus_aianE,
         aian_women = female_18to19_aianE + female_20to24_aianE + female_25to29_aianE + female_30to34_aianE + female_35to44_aianE +
           female_45to54_aianE + female_55to64_aianE + female_65to74_aianE + female_75to84_aianE + female_85plus_aianE,
         asian_men = male_18to19_asianE + male_20to24_asianE + male_25to29_asianE + male_30to34_asianE + male_35to44_asianE +
           male_45to54_asianE + male_55to64_asianE + male_65to74_asianE + male_75to84_asianE + male_85plus_asianE,
         asian_women = female_18to19_asianE + female_20to24_asianE + female_25to29_asianE + female_30to34_asianE + female_35to44_asianE +
           female_45to54_asianE + female_55to64_asianE + female_65to74_asianE + female_75to84_asianE + female_85plus_asianE,
         nhpi_men = male_18to19_nhpiE + male_20to24_nhpiE + male_25to29_nhpiE + male_30to34_nhpiE + male_35to44_nhpiE +
           male_45to54_nhpiE + male_55to64_nhpiE + male_65to74_nhpiE + male_75to84_nhpiE + male_85plus_nhpiE,
         nhpi_women = female_18to19_nhpiE + female_20to24_nhpiE + female_25to29_nhpiE + female_30to34_nhpiE + female_35to44_nhpiE +
           female_45to54_nhpiE + female_55to64_nhpiE + female_65to74_nhpiE + female_75to84_nhpiE + female_85plus_nhpiE,
         other_men = male_18to19_otherE + male_20to24_otherE + male_25to29_otherE + male_30to34_otherE + male_35to44_otherE +
           male_45to54_otherE + male_55to64_otherE + male_65to74_otherE + male_75to84_otherE + male_85plus_otherE,
         other_women = female_18to19_otherE + female_20to24_otherE + female_25to29_otherE + female_30to34_otherE + female_35to44_otherE +
           female_45to54_otherE + female_55to64_otherE + female_65to74_otherE + female_75to84_otherE + female_85plus_otherE,
         twoplus_men = male_18to19_twoplusE + male_20to24_twoplusE + male_25to29_twoplusE + male_30to34_twoplusE + male_35to44_twoplusE +
           male_45to54_twoplusE + male_55to64_twoplusE + male_65to74_twoplusE + male_75to84_twoplusE + male_85plus_twoplusE,
         twoplus_women = female_18to19_twoplusE + female_20to24_twoplusE + female_25to29_twoplusE + female_30to34_twoplusE + female_35to44_twoplusE +
           female_45to54_twoplusE + female_55to64_twoplusE + female_65to74_twoplusE + female_75to84_twoplusE + female_85plus_twoplusE)

race_agg <- race_agg_by_sex %>%
  transmute(GEOID = GEOID,
            name = NAME,
            hisp = hisp_men + hisp_women,
            white = white_men + white_women,
            black = black_men + black_women,
            aian = aian_men + aian_women,
            asian = asian_men + asian_women,
            nhpi = nhpi_men + nhpi_women,
            other = other_men + other_women,
            twoplus = twoplus_men + twoplus_women,
            fourcat = aian + asian + nhpi + other + twoplus,
            fivecat = aian + nhpi + other + twoplus)

# now need to get 18up denominator into this dataset
race_dist <- race_agg %>% left_join(denom, by = c("GEOID")) %>%
  mutate(prop_hisp = hisp/all_18up,
         prop_white = white/all_18up,
         prop_black = black/all_18up,
         prop_asian = asian/all_18up,
         prop_fourcat = fourcat/all_18up,
         prop_fivecat = fivecat/all_18up)


#### COMBINE ALL TOGETHER ####
all_acs <- age_dist %>% 
  left_join(sex_dist) %>% 
  left_join(race_dist) %>% 
  mutate(state_fips = as.integer(GEOID)) %>% 
  select(state_fips, NAME, all_18up, contains("prop"))

write_csv(all_acs, "data/output/acs_target_state_data_for_contact_raking.csv")
