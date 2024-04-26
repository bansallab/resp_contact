
{
  library(tidyverse)
  library(tidylog)
  library(choroplethr)
  library(viridis)
  library(MetBrewer)
}

#### social distancing Safegraph data ####

new_data <- read_csv("data/input/social_distancing_county_2019_2020_2021_visitorcount.csv",
                     col_types = "Diddddddd") %>% 
  rename(fips = countyFIPS) %>% 
  filter(fips < 60000)

spatiotemporal_fits <- read_csv("data/output/normal_gamma2_72trunc/fitted_predictions.csv",
                                col_types = "ddiiddddddddiiDccDccf") %>% ungroup()

check_corr <- spatiotemporal_fits %>% 
  filter(week >= ymd("2020-09-01"),
         week < ymd("2021-01-01")) %>% 
  left_join(new_data %>% rename(week = date), by = c("fips", "week"))

check_corr %>% ggplot(aes(x = fit, y = prop_visitor_maxvisitor)) + 
  geom_point(alpha = 0.05) +
  geom_smooth(method = "lm") +
  theme_bw() +
  labs(x = "Contact", y = "Mobility metric")
ggsave("figures/supp/mobility-contact-correlation.pdf", height = 5, width = 8)

#### (1) normalized data ####
# calculate ratio of visitor proportions in fall 2019 to 2020
# then use this scalar to multiply baseline contact to see how much higher it would be
# in a time like 2019 where there is no threat of disease

# already performed the normalization 

# calculate each fips' mean normalized mobility for 2019 and 2020
mob_19 <- new_data %>% 
  filter(date > ymd("2019-09-30") & date < ymd("2020-01-01")) %>% 
  group_by(fips) %>% 
  summarise(mean_mob_19 = mean(prop_visitor_maxvisitor, na.rm = T)) 

mob_20 <- new_data %>% 
  filter(date > ymd("2020-09-30") & date < ymd("2021-01-01")) %>% 
  group_by(fips) %>% 
  summarise(mean_mob_20 = mean(prop_visitor_maxvisitor, na.rm = T)) # fips mean contact 2020

safegraph_ratio <- mob_19 %>% 
  left_join(mob_20) %>% 
  mutate(phi = mean_mob_19/mean_mob_20) %>% 
  filter(!is.na(phi))

midpoint <- 1
grad_mid <- (midpoint-min(safegraph_ratio$phi))/(max(safegraph_ratio$phi)-min(safegraph_ratio$phi))
map <- CountyChoropleth$new(safegraph_ratio %>% rename(value = phi, region = fips))
map$ggplot_polygon <- geom_polygon(aes(fill = value), color = NA, linewidth = 0.01)
map$set_num_colors(1)
map$ggplot_scale <- scale_fill_gradientn(colors=met.brewer("Hiroshige"), 
                                         values = c(1, grad_mid, 0),
                                         name = "Mean ratio")
map <- map$render()
map  


write_csv(safegraph_ratio, "data/output/mobility_19_20_fall_ratio_new_norm.csv")

## we also need to aggregate this to the state level for race/ethnicity baseline calculation
df.fips <- read_csv('data/input/state_and_county_fips_master.csv')
new_data_state <- new_data %>% 
  mutate(fips = ifelse(fips == 2158, 2270,
                       ifelse(fips == 46102, 46113, fips))) %>% 
  left_join(df.fips)

mob_19_state <- new_data_state %>% 
  filter(date > ymd("2019-09-30") & date < ymd("2020-01-01")) %>% 
  group_by(state) %>% 
  summarise(mean_mob_19 = mean(prop_visitor_maxvisitor, na.rm = T)) 

mob_20_state <- new_data_state %>% 
  filter(date > ymd("2020-09-30") & date < ymd("2021-01-01")) %>% 
  group_by(state) %>% 
  summarise(mean_mob_20 = mean(prop_visitor_maxvisitor, na.rm = T)) 

library(choroplethrMaps)
data("state.regions")

safegraph_ratio <- mob_19_state %>% 
  left_join(mob_20_state) %>% 
  mutate(phi = mean_mob_19/mean_mob_20) %>% 
  filter(!is.na(phi)) %>% 
  left_join(state.regions, by = c("state" = "abb"))


midpoint <- 1
grad_mid <- (midpoint-min(safegraph_ratio$phi))/(max(safegraph_ratio$phi)-min(safegraph_ratio$phi))
map <- StateChoropleth$new(safegraph_ratio %>% rename(value = phi))
map$ggplot_polygon <- geom_polygon(aes(fill = value), color = NA, linewidth = 0.01)
map$set_num_colors(1)
map$show_labels <- FALSE
map$ggplot_scale <- scale_fill_gradientn(colors=met.brewer("Hiroshige"), 
                                         values = c(1, grad_mid, 0),
                                         name = "Mean ratio")
map <- map$render()
map  

write_csv(safegraph_ratio, "data/output/mobility_19_20_fall_ratio_new_norm_state.csv")

