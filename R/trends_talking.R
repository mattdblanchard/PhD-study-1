x <- vars %>% 
  select(uid, collisions_1:collisions_5, matches("time_talking_[1-5]"), -matches("drone")) %>% 
  pivot_longer(-uid, names_to = "var", values_to = "val") %>% 
  mutate(var = str_replace(var, "collisions", "x_collisions")) %>% 
  separate(var, into = c("time", "var", "lap")) %>% 
  select(-time) %>% 
  mutate(lap = as.numeric(lap))
  # pivot_wider(names_from = lap, values_from = val)

x %>% 
  group_by(lap) %>% 
  summarise(talk = mean(val[var == "talking"]),
            col = mean(val[var == "collisions"])) %>% 
  gather(var, val, talk, col) %>% 
  ggplot(aes(x = lap, y = val, colour = var)) +
  geom_line()

# overall trend for time talking
x <- vars %>%
  select(uid, matches("time_talking_[1-5]"),
         -matches("drone")) %>%
  pivot_longer(-uid, names_to = "var", values_to = "val") %>%
  separate(var, into = c("time", "var", "lap")) %>%
  select(-time, -var) %>%
  mutate(lap = as.numeric(lap)) %>% 
  filter(lap != 1)

# x <- vars %>% 
#   select(uid, collisions_1:collisions_5, matches("time_talking_[1-5]"),
#          -matches("drone")) %>%
#   pivot_longer(-uid, names_to = "var", values_to = "val") %>% 
#   mutate(var = str_replace(var, "coll", "x_coll")) %>% 
#   separate(var, into = c("time", "var", "lap")) %>% 
#   select(-time) %>% 
#   mutate(lap = as.numeric(lap)) %>% 
#   pivot_wider(names_from = var, values_from = val)


fit <- lm(val ~ poly(lap, degree = 3), data = x)
summary(fit)




# prepare date for polynomial regression
# function to standardise data for regression
standardise <- function(x) {
  (x - mean(x, na.rm = TRUE)) / sd(x, na.rm = TRUE)
}

x <- vars %>%
  select(team, matches("time_talking_[1-5]")) %>% 
  pivot_longer(-team, names_to = "var", values_to = "val") %>% 
  mutate(role = ifelse(str_detect(var, "drone"), "drone", "driver"), 
         var = str_remove(var, "_drone")) %>%
  separate(var, into = c("time", "talk", "lap")) %>% 
  pivot_wider(names_from = role, values_from = val) %>% 
  mutate(total = rowSums(across(where(is.numeric)))) %>% 
  mutate_if(is.numeric, standardise) %>% 
  pivot_longer(driver:total, names_to = "role", values_to = "val") %>% 
  mutate(lap = as.numeric(lap)) %>% 
  select(-time, -talk)
  

# fit trends
x <- x %>%
  na.omit() %>% 
  nest(data = c(lap, val)) %>%
  mutate(fit = map(data, ~ lm(val ~ poly(lap, degree = 3), .)),
         tidy_fit = map(fit, tidy)) %>%
  unnest(tidy_fit) %>%
  select(-statistic, -std.error) %>%
  gather(var, val, -(team:term)) %>%
  mutate(polynomial = str_extract(term, "[0-9]+$"),
         polynomial = str_c("poly_", polynomial),
         polynomial = ifelse(is.na(polynomial), "intercept", polynomial)) %>%
  unite(var, var, polynomial, sep = "_") %>%
  unite(var, var, role) %>%
  select(team, var, val) %>%
  spread(var, val)


cor <- vars %>% 
  select(team, collisions_overall, collisions_fog_overall, collisions_none_overall,
         speed_overall, speed_fog_overall, speed_none_overall,
         distance_overall, distance_fog_overall, distance_none_overall) %>% 
  left_join(x, by = "team") %>% 
  select(-team, -matches("p.value"), -matches("intercept")) %>% 
  correlate() %>% focus(collisions_overall:distance_none_overall)

cor %>% 
  gather(var, val, -rowname) %>% 
  filter(val > .3)
