packages <- c("tidyverse", "corrr", "here")
purrr::map(packages, library, character.only = TRUE)

source("R/corstarsfunction.R")
# source("R/corr_matrix_sig.R")

# read sim variables
vars <- read_csv(here("data/200309_comms_efa_vars.csv")) %>% 
  filter(!team %in% c("17080712_1", "17080810_1"))  # remove outliers: collisions("17080810_1"), distance("17081510_2")



# corrs bw nasa vars for driver
vars %>% 
  select(effort:temporal_demand, collisions_overall, speed_overall, distance_overall) %>% 
  corstarssigcheck()

# corrs bw nasa vars for UAV operator
vars %>% 
  select(effort_drone:temporal_demand_drone, collisions_overall, speed_overall, distance_overall) %>% 
  corstarssigcheck()

# corrs bw driver's rating of UAV and performance
vars %>% 
  select(effort_other:temporal_demand_other, collisions_overall, speed_overall, distance_overall) %>% 
  corstarssigcheck()

# corrs bw UAV's rating of driver and performance
vars %>% 
  select(effort_other_drone:temporal_demand_other_drone, collisions_overall, speed_overall, distance_overall) %>% 
  corstarssigcheck()

vars %>% 
  select(effort:temporal_demand, effort_other:temporal_demand_other, 
         effort_drone:temporal_demand_drone, effort_other_drone:temporal_demand_other_drone
         # collisions_overall, speed_overall, distance_overall, 
         # terrible_codriver, inconsistent_codriver, helpful_exchange,
         # confidence, bias, discrimination,
         # confidence_drone, bias_drone, discrimination_drone
         ) %>% 
  corstarssigcheck() %>% 
  rownames_to_column() %>% 
  write_csv("output/nasa_corrs2.csv")

ord <- data.frame(name = factor(c("performance", "effort", "mental_demand", "physical_demand", "temporal_demand", "frustration",
                           "performance_drone", "effort_drone", "mental_demand_drone", "physical_demand_drone", "temporal_demand_drone", "frustration_drone",
                           "performance_other", "effort_other", "mental_demand_other", "physical_demand_other", "temporal_demand_other", "frustration_other",
                           "performance_other_drone", "effort_other_drone", "mental_demand_other_drone", "physical_demand_other_drone", "temporal_demand_other_drone", 
                           "frustration_other_drone", "collisions_overall", "speed_overall", "distance_overall", "terrible_codriver", "inconsistent_codriver", "helpful_exchange",
                           "confidence", "bias", "discrimination", "confidence_drone", "bias_drone", "discrimination_drone"),
                           levels = c("performance", "effort", "mental_demand", "physical_demand", "temporal_demand", "frustration",
                                        "performance_drone", "effort_drone", "mental_demand_drone", "physical_demand_drone", "temporal_demand_drone", "frustration_drone",
                                        "performance_other", "effort_other", "mental_demand_other", "physical_demand_other", "temporal_demand_other", "frustration_other",
                                        "performance_other_drone", "effort_other_drone", "mental_demand_other_drone", "physical_demand_other_drone", "temporal_demand_other_drone", 
                                        "frustration_other_drone")))

x[order(ord$name),
  order(ord$name)]

c <- x[order(factor(ord$name, levels = c("performance", "effort", "mental_demand", "physical_demand", "temporal_demand", "frustration",
                                    "performance_drone", "effort_drone", "mental_demand_drone", "physical_demand_drone", "temporal_demand_drone", "frustration_drone",
                                    "performance_other", "effort_other", "mental_demand_other", "physical_demand_other", "temporal_demand_other", "frustration_other",
                                    "performance_other_drone", "effort_other_drone", "mental_demand_other_drone", "physical_demand_other_drone", "temporal_demand_other_drone", 
                                    "frustration_other_drone"))), 
    order(factor(ord$name, levels = c("performance", "effort", "mental_demand", "physical_demand", "temporal_demand", "frustration",
                                      "performance_drone", "effort_drone", "mental_demand_drone", "physical_demand_drone", "temporal_demand_drone", "frustration_drone",
                                      "performance_other", "effort_other", "mental_demand_other", "physical_demand_other", "temporal_demand_other", "frustration_other",
                                      "performance_other_drone", "effort_other_drone", "mental_demand_other_drone", "physical_demand_other_drone", "temporal_demand_other_drone", 
                                      "frustration_other_drone")))]

vars %>% 
  select(effort:temporal_demand, effort_drone:temporal_demand_drone, 
         collisions_overall, speed_overall, distance_overall) %>% 
  corstarssigcheck() %>% 
  rownames_to_column() %>%
  write_csv("output/nasa_self_perform.csv")

vars %>% 
  select(effort:temporal_demand, effort_other_drone:temporal_demand_other_drone, 
         collisions_overall, speed_overall, distance_overall) %>% 
  corstarssigcheck() %>% 
  rownames_to_column() %>%
  write_csv("output/nasa_perform.csv")

vars %>% 
  select(effort_other:temporal_demand_other, effort_other_drone:temporal_demand_other_drone,
         collisions_overall, speed_overall, distance_overall) %>% 
  corstarssigcheck() %>% 
  rownames_to_column() %>%
  write_csv("output/nasa_other.csv")

6*4
vars %>% 
  select(effort:temporal_demand, effort_drone:temporal_demand_drone,
         effort_other:temporal_demand_other, effort_other_drone:temporal_demand_other_drone) %>% 
  pivot_longer(effort:temporal_demand_other_drone, names_to = "var", values_to = "val") %>% 
  group_by(var) %>% 
  summarise(mean = mean(val),
            sd = sd(val)) %>% 
  write_csv("output/nasa_desc_role.csv")


vars %>% 
  select(effort:temporal_demand, effort_drone:temporal_demand_drone,
         effort_other:temporal_demand_other, effort_other_drone:temporal_demand_other_drone) %>% 
  pivot_longer(effort:temporal_demand_other_drone, names_to = "var", values_to = "val") %>% 
  mutate(var = str_remove(var, "_drone")) %>% 
  group_by(var) %>% 
  summarise(mean = mean(val),
            sd = sd(val)) %>% 
  write_csv("output/nasa_desc_overall.csv")


names(vars)

nested <- vars %>% 
  select(team, congruent_errors:neuroticism, effort:temporal_demand, effort_other:temporal_demand_other,
         congruent_errors_drone:neuroticism_drone, effort_drone:temporal_demand_drone,
         effort_other_drone:temporal_demand_other_drone) %>% 
  pivot_longer(-team, names_to = "var", values_to = "val") %>% 
  mutate(role = ifelse(str_detect(var, "drone"), "drone", "driver"),
         var = str_remove(var, "_drone")) %>% 
  pivot_wider(names_from = role, values_from = val) %>% 
  nest(data = c(team, driver, drone))

c <- nested %>% 
  mutate(fit = map(data, ~ t.test(.x$driver, .x$drone, var.equal = T)),
         tidy_fit = map(fit, broom::tidy)) %>% 
  unnest(tidy_fit) %>% 
  mutate(p.value = round(p.value, 3)) %>% 
  select(-data, -fit, -conf.low, -conf.high, -alternative) %>% 
  filter(p.value > .05)







vars %>% 
  select(effort:temporal_demand, 
         # effort_drone:temporal_demand_drone, 
         # effort_other:temporal_demand_other, 
         effort_other_drone:temporal_demand_other_drone) %>% 
  correlate() %>% focus(effort_other_drone:temporal_demand_other_drone) %>% 
  select(rowname, performance_other_drone, effort_other_drone, mental_demand_other_drone,
         physical_demand_other_drone, temporal_demand_other_drone, frustration_other_drone) %>% 
  mutate(rowname = factor(rowname, 
                          levels = c("performance","effort","mental_demand","physical_demand","temporal_demand","frustration"))) %>% 
  arrange(rowname) %>% 
  write_csv("output/driver_self_uav_other.csv")


vars %>% 
  select(effort_drone:temporal_demand_drone,
         effort_other:temporal_demand_other) %>% 
  correlate() %>% focus(effort_other:temporal_demand_other) %>% 
  select(rowname, performance_other, effort_other, mental_demand_other,
         physical_demand_other, temporal_demand_other, frustration_other) %>% 
  mutate(rowname = factor(rowname, 
                          levels = c("performance_drone","effort_drone","mental_demand_drone",
                                     "physical_demand_drone","temporal_demand_drone","frustration_drone"))) %>% 
  arrange(rowname) %>% 
  write_csv("output/uav_self_driver_other.csv")


vars %>% 
  select(
    effort:temporal_demand,
    # effort_drone:temporal_demand_drone,
    effort_other:temporal_demand_other
    # effort_other_drone:temporal_demand_other_drone
  ) %>% 
  correlate() %>% focus(effort_other:temporal_demand_other) %>% 
  select(rowname, performance_other, effort_other, mental_demand_other,
         physical_demand_other, temporal_demand_other, frustration_other) %>% 
  mutate(rowname = factor(rowname, 
                          levels = c("performance","effort","mental_demand",
                                     "physical_demand","temporal_demand","frustration"))) %>% 
  arrange(rowname) %>% 
  write_csv("output/driver_self_driver_other.csv")


vars %>% 
  select(effort_drone:temporal_demand_drone,
    effort_other_drone:temporal_demand_other_drone) %>% 
  correlate() %>% focus(effort_other_drone:temporal_demand_other_drone) %>% 
  select(rowname, performance_other_drone, effort_other_drone, mental_demand_other_drone,
         physical_demand_other_drone, temporal_demand_other_drone, frustration_other_drone) %>% 
  mutate(rowname = factor(rowname, 
                          levels = c("performance_drone","effort_drone","mental_demand_drone",
                                     "physical_demand_drone","temporal_demand_drone","frustration_drone"))) %>% 
  arrange(rowname) %>% 
  write_csv("output/uav_self_uav_other.csv")





vars %>% 
  select(effort:temporal_demand,
         effort_drone:temporal_demand_drone,
         effort_other:temporal_demand_other,
         effort_other_drone:temporal_demand_other_drone,
         collisions_overall, speed_overall, distance_overall,
         terrible_codriver, inconsistent_codriver, helpful_exchange) %>% 
  correlate() %>% focus(collisions_overall, speed_overall, distance_overall,
                        terrible_codriver, inconsistent_codriver, helpful_exchange) %>% 
  mutate(rowname = factor(rowname, 
                          levels = c("performance","effort","mental_demand",
                                     "physical_demand","temporal_demand","frustration",
                                     "performance_drone","effort_drone","mental_demand_drone",
                                     "physical_demand_drone","temporal_demand_drone","frustration_drone",
                                     "performance_other","effort_other","mental_demand_other",
                                     "physical_demand_other","temporal_demand_other","frustration_other",
                                     "performance_other_drone","effort_other_drone","mental_demand_other_drone",
                                     "physical_demand_other_drone","temporal_demand_other_drone","frustration_other_drone"))) %>% 
  arrange(rowname) %>% 
  write_csv("output/nasa_perf_comms.csv")



cor.test(vars$speed_fog_overall, vars$eng_fl_co_driver)


fit <- lm(collisions_overall ~ inconsistent_codriver + terrible_codriver + incongruent_errors + prop_female, data = vars)

summary(fit)




vars$aus_born_co_driver

vars %>% 
  select(collisions_overall, collisions_no_fog_overall, collisions_fog_overall,
         speed_overall, speed_no_fog_overall, speed_fog_overall,
         distance_overall, distance_no_fog_overall, distance_fog_overall,
         helpful_exchange, inconsistent_codriver, terrible_codriver,
         driving_years:neuroticism, age_driver, eng_fl_driver, prop_female) %>% 
  corstarssigcheck() %>% 
  rownames_to_column() %>% 
  write_csv("output/reg_corrs_driver.csv")

vars %>% 
  select(collisions_overall, collisions_no_fog_overall, collisions_fog_overall,
         speed_overall, speed_no_fog_overall, speed_fog_overall,
         distance_overall, distance_no_fog_overall, distance_fog_overall,
         helpful_exchange, inconsistent_codriver, terrible_codriver,
         driving_years_drone:neuroticism_drone, 
         age_co_driver, eng_fl_co_driver, prop_female) %>% 
  corstarssigcheck() %>% 
  rownames_to_column() %>% 
  write_csv("output/reg_corrs_uav.csv")





fit <- lm(scale(speed_overall) ~ scale(inconsistent_codriver) + scale(helpful_exchange) + scale(agreeableness) + 
            scale(neuroticism) + scale(switch_time_drone) + scale(prop_female), data = vars)
summary(fit)



vars %>% 
  ggplot(aes(y = helpful_exchange, x = agreeableness)) +
  geom_point() +
  geom_smooth(method = "lm", se = F) +
  labs(y = "Helpful Exchange", x = "Agreeableness (Driver)") +
  theme_classic()

ggsave("output/dhss_helpful_comm.png", width = 8, height = 6)


cor.test(vars$helpful_exchange, vars$speed_overall)

