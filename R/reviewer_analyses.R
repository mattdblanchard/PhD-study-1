packages <- c("tidyverse", "psych", "corrr")
# car # load package to diagnose multicollinearity
# mctest
# library(olsrr) # print semi-part correlations

lapply(packages, library, character.only = TRUE)

# function for correlation matrices with sig. stars
source("R/pca_functions.R")

# read data and remove teams with issues

d <- read_csv("data/210513_comms_efa_vars.csv")
  
# to examine teams back in dataset that were removed from N=54 because of no audio
# d3 <- read_csv("data/200309_comms_efa_vars.csv") %>% 
#   filter(!team %in% c("17080712_1", "17080810_1"))
# 
#  d %>% filter(!uid %in% d3$uid) %>% 
#    filter(str_detect(issue_note, "arrows")) %>% 
#    select(uid, issue_note)

# there are 4 collisions outliers which are candidates for removal. Need to examine more closely
# GGally::ggpairs(d %>% filter(collisions_overall < 550) %>%  select(collisions_overall, speed_overall))
# GGally::ggpairs(d %>% filter(!uid %in% c("17032109_1-g2", "17032915_2-d2")) %>%  select(collisions_overall, speed_overall))
# 
# d %>% mutate(z = (collisions_overall - mean(collisions_overall))/sd(collisions_overall)) %>% 
#   filter(collisions_overall > 500) %>% select(uid, collisions_overall, speed_overall, distance_overall, z)

# mean age
x <- d %>% 
  select(age_driver, age_co_driver) %>% 
  pivot_longer(everything(), names_to ="var", values_to = "val") %>% 
  summarise(m = mean(val),
            sd = sd(val))


# reliability estimates for sim metrics
nested <- d %>%
      ungroup() %>% 
      select(team, collisions_1:collisions_5, speed_1:speed_5,
             collisions_fog_1:collisions_fog_5, speed_fog_1:speed_fog_5,
             collisions_none_1:collisions_none_5, speed_none_1:speed_none_5) %>% 
  gather(var, val, -team) %>% 
  mutate(lap = str_extract(var, "_[1-5]"),
         lap = as.numeric(str_remove(lap, "_")),
         var = str_remove(var, "_[1-5]")) %>% 
  spread(lap, val) %>% 
  select(-team) %>% 
  nest(data = `1`:`5`)

nested %>% 
  mutate(fit = map(data, ~ alpha(.)$total$raw_alpha)) %>% 
  unnest(fit)

# descriptives for sim metrics
d %>% 
  select(collisions_overall, speed_overall,
         collisions_fog_overall, speed_fog_overall,
         collisions_none_overall, speed_none_overall) %>% 
  summarise(across(everything(), list(mean = mean, sd = sd), .names = "{.col}_{.fn}")) %>% 
  pivot_longer(everything(), names_to = "var", values_to = "val")

# t-tests on collisions and speed during for and normal periods
t.test(d$distance_none_overall, d$distance_fog_overall, paired = T, var.equal = T)

cor.test(d$collisions_none_overall, d$distance_none_overall)
cor.test(d$collisions_fog_overall, d$distance_fog_overall)

t.test(d$collisions_fog_ratio, d$collisions_none_ratio, paired = T, var.equal = T)
t.test(d$speed_fog_overall, d$speed_none_overall, paired = T, var.equal = T)








# select which communication variables to analyse
comms_fac <- d %>% select(terrible_codriver_r_two, helpful_exchange_r_two) %>% names()

# select which sim-metrics to analyse
sim_metrics <- d %>% select(collisions_overall, speed_overall) %>% names()

# Hierarchical regression -------------------------------------------------
# uid == 17032915_2-d2 is a collisions_overall outlier
# The correlation between collisions_overall and helpful_exchange_r_two is 
# r = .245 with them in the dataset and r = .137 with them removed

# create dataset for regression
reg_d <- d %>% filter(!is.na(terrible_codriver_r_two)) # & collisions_overall < 500)


# function to create scatter plots for sig predictors
sig_plots <- function(dv) {
  sig <- broom::tidy(fit4) %>% filter(p.value < .05) %>% 
    mutate(term = str_remove(term, "scale\\("),
           term = str_remove(term, "\\)"))
  
  reg_d %>% 
    select(sig$term, all_of(dv)) %>% 
    pivot_longer(-dv, names_to = "var", values_to = "val") %>% 
    ggplot(aes(x = val, y = get(dv))) +
    geom_point() +
    stat_smooth(
      method = "lm",
      formula = y ~ x,
      se = F) +
    facet_wrap(~ var, scales = "free") +
    theme_classic()
  
}

# DRIVER
# driver correlations
reg_d %>% select(competence_factor_driver, ef_time_factor_driver, ef_errors_factor_driver,
             agreeableness, intellect, extraversion, neuroticism, conscientiousness) %>% 
  correlate() %>% fashion()

reg_d %>% select(sim_metrics, comms_fac, competence_factor_driver, ef_time_factor_driver, ef_errors_factor_driver,
                 agreeableness, intellect, extraversion, neuroticism, conscientiousness, prop_female) %>% 
  correlate() %>% focus(sim_metrics)

# collisions overall
fit1 <- lm(scale(collisions_overall) ~ scale(prop_female), reg_d)
fit2 <- lm(scale(collisions_overall) ~ scale(prop_female) + scale(ef_errors_factor_driver) + scale(competence_factor_driver), reg_d)
fit3 <- lm(scale(collisions_overall) ~ scale(prop_female) + scale(ef_errors_factor_driver) + scale(competence_factor_driver) + scale(neuroticism), reg_d)
fit4 <- lm(scale(collisions_overall) ~ scale(prop_female) + scale(ef_errors_factor_driver) + scale(competence_factor_driver) +
             + scale(neuroticism) + scale(terrible_codriver_r_two) + scale(helpful_exchange_r_two), reg_d)

summary(fit1)
summary(fit2)
summary(fit3)
summary(fit4)

# create scatter plots for sig predictors
sig_plots("collisions_overall")

# check assumptions of linear regression
plot(fit4)


# speed overall
fit1 <- lm(scale(speed_overall) ~ scale(prop_female), reg_d)
fit2 <- lm(scale(speed_overall) ~ scale(prop_female) + scale(ef_errors_factor_driver) + scale(competence_factor_driver), reg_d)
fit3 <- lm(scale(speed_overall) ~ scale(prop_female) + scale(ef_errors_factor_driver) + scale(competence_factor_driver) + scale(agreeableness), reg_d)
fit4 <- lm(scale(speed_overall) ~ scale(prop_female) + scale(ef_errors_factor_driver) + scale(competence_factor_driver) +
             + scale(agreeableness) + scale(terrible_codriver_r_two) + scale(helpful_exchange_r_two), reg_d)

summary(fit1)
summary(fit2)
summary(fit3)
summary(fit4)

# create scatter plots for sig predictors
sig_plots("speed_overall")

# check assumptions of linear regression
plot(fit4)


# DRONE
# codriver correlations
reg_d %>% select(competence_factor_drone, ef_time_factor_drone, ef_errors_factor_drone,
             agreeableness_drone, intellect_drone, extraversion_drone, neuroticism_drone, conscientiousness_drone) %>% 
  correlate() %>% fashion()

reg_d %>% select(sim_metrics, comms_fac, competence_factor_drone, ef_time_factor_drone, ef_errors_factor_drone,
             agreeableness_drone, intellect_drone, extraversion_drone, neuroticism_drone, conscientiousness_drone, prop_female) %>% 
  correlate() %>% focus(sim_metrics)

# collisions overall
fit1 <- lm(scale(collisions_overall) ~ scale(prop_female), reg_d)
fit2 <- lm(scale(collisions_overall) ~ scale(prop_female) + scale(ef_errors_factor_drone) + scale(competence_factor_drone), reg_d)
fit3 <- lm(scale(collisions_overall) ~ scale(prop_female) + scale(ef_errors_factor_drone) + scale(competence_factor_drone) + scale(neuroticism_drone), reg_d)
fit4 <- lm(scale(collisions_overall) ~ scale(prop_female) + scale(ef_errors_factor_drone) + scale(competence_factor_drone) +
             + scale(neuroticism_drone) + scale(terrible_codriver_r_two) + scale(helpful_exchange_r_two), reg_d)

summary(fit1)
summary(fit2)
summary(fit3)
summary(fit4)

# create scatter plots for sig predictors
sig_plots("collisions_overall")

# check assumptions of linear regression
plot(fit4)


# speed overall
fit1 <- lm(scale(speed_overall) ~ scale(prop_female), reg_d)
fit2 <- lm(scale(speed_overall) ~ scale(prop_female) + scale(ef_errors_factor_drone) + scale(competence_factor_drone), reg_d)
fit3 <- lm(scale(speed_overall) ~ scale(prop_female) + scale(ef_errors_factor_drone) + scale(competence_factor_drone) + scale(neuroticism_drone), reg_d)
fit4 <- lm(scale(speed_overall) ~ scale(prop_female) + scale(ef_errors_factor_drone) + scale(competence_factor_drone) +
             + scale(neuroticism_drone) + scale(terrible_codriver_r_two) + scale(helpful_exchange_r_two), reg_d)

summary(fit1)
summary(fit2)
summary(fit3)
summary(fit4)

# create scatter plots for sig predictors
sig_plots("speed_overall")

# check assumptions of linear regression
plot(fit4)







reg_vars <- d %>% select(prop_female, all_of(c(sim_metrics, comms_fac, drive_ivs, drone_ivs)))

length(reg_vars)

corstarsl(reg_vars)

nested <- reg_vars %>% 
  pivot_longer(collisions_overall:distance_overall, names_to = "var", values_to = "val") %>% 
  nest(data = -var)

# create formula using var names
fm1 <- as.formula(paste0("scale(.x$val)", " ~ ", paste0("scale(.x$", names(reg_vars), ")", collapse = " + ")))

nested %>% 
  mutate(fit = map(data, ~ lm(fm)),
         tidy_fit = map(fit, broom::tidy)) %>% 
  unnest(tidy_fit) %>% 
  filter(var == "collisions_overall") %>% 
  mutate(p.value = round(p.value, 3))


fit <- lm(scale(collisions_overall) ~ scale(prop_female) + scale(inconsistent_codriver) + scale(terrible_codriver) + scale(helpful_exchange) + 
            scale(ef_time_factor_driver) + scale(ef_errors_factor_driver) + scale(cognitive_factor_driver) + scale(agree_conscientious_factor_driver) + 
            scale(emot_stability_factor_driver), d)

summary(fit)


fit <- lm(scale(speed_overall) ~ scale(prop_female) + scale(inconsistent_codriver) + scale(terrible_codriver) + scale(helpful_exchange) + 
            scale(ef_time_factor_driver) + scale(ef_errors_factor_driver) + scale(cognitive_factor_driver) + scale(agree_conscientious_factor_driver) + 
            scale(emot_stability_factor_driver), d)

summary(fit)

fit <- lm(scale(distance_overall) ~ scale(prop_female) + scale(inconsistent_codriver) + scale(terrible_codriver) + scale(helpful_exchange) + 
            scale(ef_time_factor_driver) + scale(ef_errors_factor_driver) + scale(cognitive_factor_driver) + scale(agree_conscientious_factor_driver) + 
            scale(emot_stability_factor_driver), d)

summary(fit)

  













# play --------------------------------------------------------------------

# driver correlations
  
x <- d %>% filter(collisions_overall < 500)
  
  
x %>% select(gf_accuracy, confidence, wm_accuracy, ef_time_factor_driver, ef_errors_factor_driver,
             agreeableness, intellect, extraversion, neuroticism, conscientiousness, resilience) %>% 
  correlate() %>% fashion()



d %>% filter(!is.na(helpful_exchange_r_two)) %>% select(collisions_overall, speed_overall, comms_fac, competence_factor_driver, ef_time_factor_driver, ef_errors_factor_driver,
             agreeableness, intellect, extraversion, neuroticism, conscientiousness, resilience, prop_female) %>% 
  correlate() %>% focus(collisions_overall, speed_overall) %>% 
  bind_cols(x %>% select(collisions_overall, speed_overall, comms_fac, competence_factor_driver, ef_time_factor_driver, ef_errors_factor_driver,
                         agreeableness, intellect, extraversion, neuroticism, conscientiousness, resilience, prop_female) %>% 
              correlate() %>% focus(collisions_overall, speed_overall) %>% 
              rename(collisions_overallrev = collisions_overall, speed_overallrev = speed_overall) %>% 
              select(-term)) %>% 
  select(term, collisions_overall, collisions_overallrev, speed_overall, speed_overallrev) %>% 
  pivot_longer(collisions_overall:speed_overallrev, names_to = "var", values_to = "val") %>% 
  separate(var, into = c("metric", "data")) %>% 
  group_by(term, metric) %>% 
  mutate(diff = val[1] - val[2]) %>% 
  select(-val, -data) %>% 
  filter(diff > .05)



d %>% select(helpful_exchange_r_two, collisions_overall) %>% correlate()
x %>% select(helpful_exchange_r_two, collisions_overall) %>% correlate()
d %>% filter(!is.na(helpful_exchange_r_two)) %>% filter(collisions_overall < 500) %>% select(helpful_exchange_r_two, collisions_overall) %>% correlate()



# codriver correlations
d %>% select(gf_accuracy_drone, confidence_drone, wm_accuracy_drone, ef_time_factor_drone, ef_errors_factor_drone,
             agreeableness_drone, intellect_drone, extraversion_drone, neuroticism_drone, conscientiousness_drone, resilience_drone) %>% 
  correlate() %>% fashion()

d %>% select(sim_metrics, comms_fac, gf_accuracy_drone, confidence_drone, wm_accuracy_drone, ef_time_factor_drone, ef_errors_factor_drone,
             agreeableness_drone, intellect_drone, extraversion_drone, neuroticism_drone, conscientiousness_drone, resilience_drone, prop_female) %>% 
  correlate() %>% focus(sim_metrics)
