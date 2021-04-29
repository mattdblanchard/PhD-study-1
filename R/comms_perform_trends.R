packages <- c("tidyverse", "corrr", "here", "broom")
purrr::map(packages, library, character.only = TRUE)

# read sim variables
vars <- read_csv(here("data/200227_comms_efa_vars.csv")) %>% 
  filter(!team %in% c("17080712_1", "17080810_1"))  # remove outliers: collisions("17080810_1"), distance("17081510_2")


# plot relationship between comms and performance
vars %>% 
  select(terrible_codriver, inconsistent_codriver, helpful_exchange, collisions_overall, speed_overall, distance_overall) %>% 
  pivot_longer(terrible_codriver:helpful_exchange, names_to = "comms", values_to = "comms_val") %>% 
  pivot_longer(collisions_overall:distance_overall, names_to = "perf", values_to = "perf_val") %>% 
  ggplot(aes(x = comms_val, y = perf_val)) +
  geom_smooth(method = "loess") +
  facet_grid(perf ~ comms, scales = "free_y") +
  theme_classic()

# fit poly regression
library(broom)
nested <- vars %>% 
  select(terrible_codriver, inconsistent_codriver, helpful_exchange, collisions_overall, speed_overall, distance_overall) %>% 
  mutate_if(is.numeric, scale) %>%
  pivot_longer(terrible_codriver:helpful_exchange, names_to = "comm_var", values_to = "comm") %>% 
  pivot_longer(collisions_overall:distance_overall, names_to = "perf_var", values_to = "perform") %>% 
  nest(data = c(comm, perform))

nested %>% 
  mutate(fit = map(data, ~ lm(perform ~ poly(comm, degree = 3), data = .)),
         tidy_fit = map(fit, tidy)) %>%
  unnest(tidy_fit) %>% 
  select(-std.error) %>% 
  pivot_longer(-(comm_var:term), names_to = "var", values_to = "val") %>%
  mutate(poly = str_extract(term, "[0-9]+$"),
         poly = str_c("poly_", poly),
         poly = ifelse(is.na(poly), "intercept", poly)) %>% 
  filter(poly != "intercept") %>% 
  pivot_wider(names_from = var, values_from = val) %>% 
  select(-data, -fit, -term, -statistic, -p.value) %>% 
  pivot_wider(names_from = poly, values_from = estimate) %>% 
  write_csv("output/comm_perform_poly.csv")

# get r squared for each model
map(1:9, function(i) {
data.frame(comm = print(c$comm_var[[i]]),
           perform = print(c$perf_var[[i]]),
           r.sqr =   round(glance(c$fit[[i]])$r.squared,3),
           f = round(glance(c$fit[[i]])$statistic,3),
           p = round(glance(c$fit[[i]])$p.value,3))
})



