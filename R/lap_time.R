# This script collects the times for the start of each lap for each driver
# this is to match the comms coding with the lap

library(tidyverse)

users <- read_rds("data/rds/sim_data_clean.rds")
user_issues <-  read_rds("data/rds/sim_issues.rds")

options(stringsAsFactors = FALSE)

ids <- data.frame(uid = names(users)) %>% 
  filter(!str_detect(uid, "d1")) %>% 
  filter(!str_detect(uid, "g1"))

lap_time <- lapply(1:length(users), function(i) {
  # users$`i`$events %>%
  users[[i]]$events %>% 
    filter(event == "newlap") %>% 
    mutate(uid = users[[i]]$session$user_id)
})

lap_time <- bind_rows(lap_time) %>% 
  mutate(team = str_remove(uid, "-[dg]2")) %>% 
  select(team, lap, time) %>% 
  rename(lap_time = time)

# lap_time %>% 
#   group_by(uid) %>% 
#   summarise(n = n()) %>% 
#   filter(n < 5)

lap_time %>% write_csv("data/lap_times.csv")



# Get times for events ----------------------------------------------------
# start and end times for each event
event_time <- lapply(1:length(users), function(i) {
  users[[i]]$events %>% 
    filter(event %in% c("fog_start", "fog_end", "fall_start", "fall_end", 
                        "ice_start", "ice_end", "block_start", "block_end", 
                        "animal_start", "animal_end")) %>% 
    mutate(uid = users[[i]]$session$user_id)
})

event_time <- bind_rows(event_time) %>% 
  mutate(team = str_remove(uid, "-[dg]2")) %>% 
  select(team, lap, event, time) %>% 
  rename(event_time = time)  

event_time %>% write_csv("data/event_times.csv")
