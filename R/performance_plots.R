# load packages
library(tidyverse)
library(here)

# read sim variables
vars <- read_csv(here("data/200309_comms_efa_vars.csv")) %>% 
  filter(!team %in% c("17080712_1", "17080810_1"))  # remove outliers: collisions("17080810_1"), distance("17081510_2")


# Overall -----------------------------------------------------------------
vars %>% select(team, collisions_1:collisions_5) %>% 
  gather(var, val, -team) %>% 
  separate(var, c("a", "lap")) %>% 
  group_by(lap) %>% 
  summarise(val = mean(val)) %>% 
  ggplot(aes(x = lap, y = val)) +
  geom_point() + 
  geom_line(group = 1) +
  theme_classic()


vars %>% select(team, speed_1:speed_5) %>% 
  gather(var, val, -team) %>% 
  separate(var, c("a", "lap")) %>% 
  group_by(lap) %>% 
  summarise(val = mean(val)) %>% 
  ggplot(aes(x = lap, y = val)) +
  geom_point() + 
  geom_line(group = 1) +
  theme_classic()

vars %>% select(team, distance_1_deviation:distance_5_deviation) %>% 
  gather(var, val, -team) %>% 
  separate(var, c("a", "lap", "b")) %>% 
  group_by(lap) %>% 
  summarise(val = mean(val)) %>% 
  ggplot(aes(x = lap, y = val)) +
  geom_point() + 
  geom_line(group = 1) +
  theme_classic()


# By event ----------------------------------------------------------------
vars %>% 
  select(team, collisions_no_fog_1:collisions_no_fog_5, collisions_fog_1:collisions_fog_5,
         distance_fog_1:distance_fog_5, distance_no_fog_1:distance_no_fog_5) %>% 
  gather(var, val, -team) %>% 
  mutate(var = str_replace(var, "no_fog", "nofog")) %>%
  separate(var, c("var", "event", "lap")) %>% 
  group_by(team, lap, event) %>% 
  summarise(ratio = val[var == "collisions"] / val[var == "distance"]) %>% 
  group_by(lap, event) %>% 
  summarise(val = mean(ratio, na.rm = T)) %>% 
  ggplot(aes(x = lap, y = val, colour = event)) +
  geom_point() + 
  geom_line(aes(group = event)) +
  theme_classic()

vars %>% 
  select(team, speed_no_fog_1:speed_no_fog_5, speed_fog_1:speed_fog_5) %>% 
  gather(var, val, -team) %>% 
  mutate(var = str_replace(var, "no_fog", "nofog")) %>%
  separate(var, c("a", "event", "lap")) %>% 
  group_by(lap, event) %>% 
  summarise(val = mean(val, na.rm = T)) %>% 
  ggplot(aes(x = lap, y = val, colour = event)) +
  geom_point() + 
  geom_line(aes(group = event)) +
  theme_classic()

vars %>% 
  select(team, distance_no_fog_1:distance_no_fog_5, distance_fog_1:distance_fog_5) %>% 
  gather(var, val, -team) %>% 
  mutate(var = str_replace(var, "no_fog", "nofog")) %>%
  separate(var, c("a", "event", "lap")) %>% 
  group_by(lap, event) %>% 
  summarise(val = mean(val, na.rm = T)) %>% 
  ggplot(aes(x = lap, y = val, colour = event)) +
  geom_point() + 
  geom_line(aes(group = event)) +
  theme_classic()

t <- vars %>% 
  filter(distance_overall_deviation < 100 & distance_overall_deviation > -100) %>% 
  select(team, distance_overall_deviation, distance_1_deviation:distance_5_deviation) %>% 
  select(team)


vars %>% 
  select(team, distance_none_1:distance_none_5, distance_fog_1:distance_fog_5) %>% 
  filter(team %in% t$team)
