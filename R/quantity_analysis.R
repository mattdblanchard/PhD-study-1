# quantity of comms and relationship with bias and performance

f <- vars %>% 
  select(team, time_talking_fog_1:time_talking_fog_5,
         time_talking_fog_1_drone:time_talking_fog_5_drone) %>% 
  gather(var, val, -team) %>% 
  mutate(lap = str_extract(var, "[1-5]"),
         var = str_remove(var, "_[1-5]"),
         var = ifelse(str_detect(var, "drone"), var, paste0(var, "_driver"))) %>%
  group_by(team, var) %>% 
  summarise(val = sum(val)) %>% 
  group_by(team) %>% 
  mutate(time_talking_fog_all = sum(val),
         time_talking_fog_ratio = val[var == "time_talking_fog_drone"] / val[var == "time_talking_fog_driver"]) %>% 
  spread(var, val)


none <- c("none", "animal", "block", "ice", "fall")

n <- vars %>% 
  select(team, contains("time_talking")) %>% 
  select(team, contains(none), -contains("overall")) %>% 
  gather(var, val, -team) %>% 
  mutate(var = ifelse(str_detect(var, "drone"), var, paste0(var, "_driver"))) %>% 
  separate(var, into = c("a", "b", "e", "lap", "role")) %>% 
  group_by(team, role) %>% 
  summarise(val = sum(val)) %>% 
  group_by(team) %>% 
  mutate(all = sum(val),
         ratio = val[role == "drone"] / val[role == "driver"]) %>% 
  spread(role, val) %>% 
  gather(var, val, -team) %>% 
  mutate(var = paste0("time_talking_none_", var)) %>% 
  spread(var, val)


o <- vars %>% 
  select(team, time_talking_1:time_talking_5, time_talking_1_drone:time_talking_5_drone) %>% 
  gather(var, val, -team) %>% 
  mutate(lap = str_extract(var, "[1-5]"),
         var = str_remove(var, "_[1-5]"),
         var = str_replace(var, "drone", "overall_drone"),
         var = ifelse(str_detect(var, "drone"), var, paste0(var, "_overall_driver"))) %>%
  group_by(team, var) %>% 
  summarise(val = sum(val)) %>% 
  group_by(team) %>% 
  mutate(time_talking_overall_all = sum(val),
         time_talking_overall_ratio = val[var == "time_talking_overall_drone"] / val[var == "time_talking_overall_driver"]) %>% 
  spread(var, val)


datalist <- list(vars %>% select(-time_talking_overall, -time_talking_overall_drone), o, n, f)

vars <- datalist %>% reduce(left_join, by = "team")  

tail(names(vars), 20)

name <- c(names(o)[-1], names(n)[-1], names(f)[-1])

c <- vars %>% select(collisions_overall, collisions_no_fog_overall, collisions_fog_overall, 
                     speed_overall, speed_no_fog_overall, speed_fog_overall,
                     distance_overall, distance_no_fog_overall, distance_fog_overall, name) %>% 
  correlate() %>% focus(collisions_overall:distance_fog_overall) %>% 
  gather(var, val, - rowname) %>% 
  mutate(val = ifelse(abs(val) < .20, NA, val)) %>% 
  spread(var, val)

