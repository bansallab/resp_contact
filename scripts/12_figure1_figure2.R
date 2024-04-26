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
}

mob_scaled_baseline_data <- read_csv("data/output/baseline_contact_by_county_week.csv",
                                     col_types = "ddiDDididdddiiiddddfcciddddcccciddddddddddddddddddddd")
mob_scaled_baseline_data$ur_code <- factor(mob_scaled_baseline_data$ur_code, 
                                           levels = c("1", "2", "3", "4", "5", "6"))

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

spatiotemporal_fits <- read_csv("spatiotemporal/normal_gamma2_72trunc/fitted_predictions.csv",
                                col_types = "ddiiddddddddiiDccDccf") %>% ungroup()


supp_df <- mob_scaled_baseline_data %>% 
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

ggsave("figures/supp/fit-roll4-stringencyz.pdf", height = 8, width = 14)


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
  pivot_longer(cols = c(fit_contact, baseline_contact, fit_seplus, fit_seminus, baseline_seplus, baseline_seminus), 
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
                                                "Buffalo, NE", "Pondera, MT"))
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
                 labels = c("Oct", "Jan 2021",
                            "Apr"),
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
                       fips = c(4013, 1015, 12086, 27003, 30073, 31019, 33011, 34013, 48453, 53033)) %>%
  left_join(fips_I_want_shorter) %>% 
  mutate(fips = as.factor(full_name))

fig2b + 
  geom_text(data = dat_text,
            mapping = aes(x = ymd("2020-12-10"), y = 10.5, label = label),
            color = "#1E7D79",
            size = 6, 
            fontface = "bold") + 
  geom_text(data = dat_text,
            mapping = aes(x = ymd("2020-12-10"), y = 14, label = label2),
            color = "#3F3E66",
            size = 6,
            hjust = 0.33,
            fontface = "bold") -> fig2b_fin


fig2a_data <- combine_data %>% 
  dplyr::select(fips, week, contact_fit) %>% 
  group_by(fips) %>% 
  mutate(z_contact = c(scale(contact_fit)),
         mean_contact = mean(contact_fit)) %>% 
  ungroup() %>% 
  mutate(rel_mean_contact = mean_contact/mean(contact_fit))

(fig2a_data %>% 
    ggplot(aes(x = week, y = z_contact, group = fips, col = rel_mean_contact)) + # this intercept is incorporating the random effect
    geom_line(alpha = 0.3) +
    theme_bw() +
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
    scale_color_gradientn(colors = met.brewer("OKeeffe1"), limits = c(0.45, 1.5),
                          breaks = c(0.5, 1, 1.5), 
                          labels = c("below", "mean", "above"),
                          values = c(1, (1-0.45)/(1.5-0.45), 0)) -> fig2a)

# add national incidence below temporal contact curve
nyt_national_roll %>% 
  mutate(national_cases_roll3c = zoo::rollmean(national_cases, k = 3, fill = NA, align = "center")) %>% 
  filter(week >= ymd("2020-06-07"),
         week <= ymd("2021-04-25")) %>% 
  ggplot(aes(x = week, y = scale(national_cases_roll3c))) + # cases_avg is rolling average, smooths drops in weekend/holidays
  geom_line(linewidth = 1, col = "black") +
  theme_classic() +
  scale_x_date(expand = c(0,0)) +#, breaks = seq(as.Date("2020-09-15"), as.Date("2021-05-15"), by = "1 month"), date_labels ="%B") +
  theme(panel.background = element_rect(fill = "transparent"), # bg of the panel
        plot.background = element_rect(fill = "transparent", color = NA), # bg of the plot
        axis.title.x = element_blank(),
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks = element_blank(), # change to just y to line up
        axis.title.y = element_blank(),
        axis.line.y = element_blank(),
        axis.line.x = element_blank()) -> case_curve

vp <- viewport(width = 0.731, height = 0.14, x = 0.483, y = 0.685)


pdf("figures/fig1.pdf", height = 8, width = 10)
ggarrange(fig2a, fig2b_fin, ncol = 1, heights = c(5, 6), 
          labels = "AUTO", font.label = list(size = 28))
print(case_curve, vp = vp)
dev.off()


#### BASELINE VS PANDEMIC MAP ####
baseline_map <- mob_scaled_baseline_data %>% 
  filter(fips < 15000 | fips >= 16000) %>% # removing hawaii, alaska already removed
  ungroup() %>% 
  mutate(baseline_rel = scale_baseline/mean(scale_baseline),
         pandemic_rel = contact_fit/mean(contact_fit)) %>%
  group_by(fips, POPESTIMATE2020) %>% 
  summarise(mean_baseline = mean(scale_baseline),
            mean_baseline_rel = mean(baseline_rel),
            mean_pandemic = mean(contact_fit),
            mean_pandemic_rel = mean(pandemic_rel)) 

library(choroplethrMaps)
data(state.regions) # might have to run example in help
states_i_want <- state.regions %>% filter(! abb %in% c("HI", "AK")) %>% dplyr::select(region)
state_zoom = states_i_want$region

library(NatParksPalettes)
midpoint <- 1
{
  # grad_mid <- (midpoint-min(baseline_map$mean_baseline_rel))/
  #   (max(baseline_map$mean_baseline_rel)-min(baseline_map$mean_baseline_rel))
  grad_mid <- (1-0.45)/(1.5-0.45)
  bmap <- CountyChoropleth$new(baseline_map %>% rename(value = mean_baseline_rel, region = fips))
  bmap$ggplot_polygon <- geom_polygon(aes(fill = value), color = NA, linewidth = 0.01)
  bmap$set_num_colors(1)
  bmap$ggplot_scale <- scale_fill_gradientn(colors = met.brewer("Hiroshige"), 
                                            limits = c(0.45, 1.5),
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
  print(bmap)
}

{
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
  print(pmap)
}

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


ann_text <- data.frame(x = c(1, 6), y = 5,
                       setting = factor("Non-pandemic", 
                                        levels = c("Pandemic", "Pre-pandemic no mob", "Non-pandemic")),
                       label = c("Urban", "Rural"))
data.segm <- data.frame(x=2,y=5,xend=5,yend=5,
                        setting = factor("Non-pandemic", 
                                         levels = c("Pandemic", "Pre-pandemic no mob", "Non-pandemic")))

(ur_fig <- ur_plot_data3 %>% 
    left_join(urb_rur_codes) %>% 
    
    filter(setting != "Pre-pandemic no mob") %>% 
    ggplot(aes(x = ur_code, y = mean_contact)) + 
    geom_jitter(aes(col = ur_code), alpha = 0.5) +
    geom_boxplot(fill = NA, outlier.shape = NA) +
    facet_wrap(~setting, nrow = 2) +
    labs(y = "Mean contact", x = "NCHS Urban-Rural Class") +
    theme_bw() + 
    scale_color_met_d(name = "Derain", direction = -1) +
    theme(legend.position = "none",
          axis.text = element_text(size = 14),
          axis.title = element_text(size = 16),
          strip.text = element_text(size = 16)) +
    geom_text(data = ann_text,
              aes(x = x, y = y, label = label), size = 6) +
    geom_segment(data = data.segm, aes(x=x,y=y,yend=yend,xend=xend),
                 arrow = arrow(length = unit(0.03, "npc"), ends = "both")))
ggsave("figures/24-01-02/ur.pdf", height = 4, width = 6)

maps <- ggarrange(pmap, bmap, ncol = 1, labels = "AUTO", font.label=list(size=20))
ggarrange(maps, ur_fig, ncol = 2, widths = c(4, 2), labels = c("", "C"), font.label=list(size=20))
ggsave("figures/fig2.pdf", height = 8, width = 12)
