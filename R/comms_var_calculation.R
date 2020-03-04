#Soft Coding Variable Calculation

#Setup Packages
library(tidyverse)
library(lubridate)

# load the data
files <- dir("data/comm_coding_files/MB_comm_review/")

# read comms coding files for each team
data <- lapply(files, function(i) {
  data <- read_csv(c(paste0("data/comm_coding_files/MB_comm_review/", i)), skip = 1, col_names = FALSE) %>% 
    mutate(Participant = i)
  })

# bind the rows to a single tibble, rename columns and 
# clean Participant values so they are consistent with who_coded.csv
data <- bind_rows(data) %>% 
  rename(event = X2, time = X3) %>% 
  select(Participant, event, time) %>% 
  mutate(Participant = str_replace(Participant, "_WIP.", "."),
         Participant = str_replace(Participant, "processed_coded", ""),
         Participant = str_replace(Participant, "_processed", ""),
         Participant = str_replace(Participant, ".mp4_coded.csv", ""),
         Participant = str_replace(Participant, ".m4v_coded.csv", ""),
         Participant = str_remove(Participant, "MB_"),
         Participant = str_remove(Participant, "_"))

# rename comms codes
# discovered that the category labels differ between coding tool versions 
# to make consistent change "harmful" to "unhelpful" and frustration
{
data$event[data$event == "Co-Driver Gives Information -Unhelpful [N]"] <- "Co-Driver Gives Information -Harmful [N]"
data$event[data$event == "Co-Driver Gives Instruction -Unhelpful [M]"] <- "Co-Driver Gives Instruction -Harmful [M]"
data$event[data$event == "Frustration [F]"] <- "Driver Frustration/Anxiety/Surprise [F]"
data$event[data$event == "Frustration/Anxiety/Surprise [F]"] <- "Driver Frustration/Anxiety/Surprise [F]"
}

# read in who_coded data to add team id
alloc <- read_csv("data/comm_coding_files/who_coded.csv") %>% 
  mutate(Participant = str_remove(Participant, "_processed.m4v"),
         Participant = str_remove(Participant, ".mp4"))

data <- data %>% 
  left_join(alloc %>% rename(team = Team) %>% 
              select(Participant, team, video, Coder, Comments), 
            by = "Participant")

# check that all Participants have team data
data %>% filter(is.na(team)) %>% select(Participant) %>% unique()

# data %>% write_csv("data/200221_comms_raw.csv")


# calculate variables
comms <- data %>% 
  group_by(team) %>% 
  summarise(co_info_help_overall = sum(event == "Co-Driver Gives Information -Helpful [J]"),
            co_info_harm_overall = sum(event == "Co-Driver Gives Information -Harmful [N]"),
            co_instruct_help_overall = sum(event == "Co-Driver Gives Instruction -Helpful [K]"),
            co_instruct_harm_overall = sum(event == "Co-Driver Gives Instruction -Harmful [M]"),
            co_total_help_overall = co_info_help_overall + co_instruct_help_overall,
            co_total_harm_overall = co_info_harm_overall + co_instruct_harm_overall,
            co_info_harm_ratio_overall = co_info_harm_overall/co_info_help_overall,
            co_instruct_harm_ratio_overall = co_instruct_harm_overall/co_instruct_help_overall,
            co_total_harm_ratio_overall = co_total_harm_overall/co_total_help_overall,
            co_redundant_overall = sum(event == "Co-Driver Gives Redundant Info/Instructions [I]"),
            co_question_overall = sum(event == "Co-Driver Asks Question [U]"),
            co_total_overall = sum(co_total_help_overall, co_total_harm_overall, co_redundant_overall, co_question_overall),
            co_redundant_ratio_overall = co_redundant_overall/co_total_overall,
            co_question_ratio_overall = co_question_overall/co_total_overall,
            drive_question_overall = sum(event == "Driver Asks for Help [E]"),
            drive_informs_overall = sum(event == "Driver Informs Co-Driver [R]"),
            drive_frust_overall = sum(event == "Driver Frustration/Anxiety/Surprise [F]"),
            drive_question_ratio_overall = drive_question_overall/drive_informs_overall,
            drive_frust_ratio_overall = drive_frust_overall/(drive_question_overall+drive_informs_overall),
            drive_total_overall = sum(drive_question_overall, drive_informs_overall),
            drive_ratio_overall = drive_total_overall/co_total_overall,
            co_ratio_overall = co_total_overall/drive_total_overall)

# convert Inf value to NA - Inf creates issues for analyses
comms$co_ratio_overall[comms$co_ratio_overall == Inf] <- NA

# team scale
scale <- read_csv("data/comm_coding_files/team_scale.csv") %>% 
  rename(team = id) %>% 
  filter(Finished == "True") %>% 
  mutate(team = tolower(team)) %>% 
  filter(!str_detect(team, "test")) %>% 
  filter(!team %in% c("aboyi", "1"))

# modify incorrectly entered ids
{
scale$team[scale$team == "17032216011421"] <- "17032215_2"
scale$team[scale$team == "2016-10-27 12-28-27-11"] <- "16102712_1"
scale$team[scale$team == "2016-11-02 14-33-40-17"] <- "16110214_1"
scale$team[scale$team == "2017-03-15/09-26-59-6"] <- "17031509_2"
scale$team[scale$team == "2017-03-15/15-28-04-7"] <- "17031515_2"
scale$team[scale$team == "2017-03-24 11-28-24"] <- "17032215_2"
scale$team[scale$team == "2017-03-28 11-22-46"] <- "17032411_1"
scale$team[scale$team == "2017-03-28/09-20-10-32"] <- "17032809_1"
scale$team[scale$team == "2017-03-31/11-21-18-46"] <- "17033111_1"
scale$team[scale$team == "2017081412160148"] <- "17081412_2"
scale$team[scale$team == "2017081510181249"] <- "17081510_2"
scale$team[scale$team == "2017081512264852"] <- "17081512_2"
}

# recode Q1-14 responses to numeric
{
  scale[scale == "Never"] <- 1
  scale[scale == "Rarely"] <- 2
  scale[scale == "Sometimes"] <- 3
  scale[scale == "Often"] <- 4
  scale[scale == "Always"] <- 5
  }

# calculate vars
# prepare data
scale <- scale %>% 
  gather(var, val, Q1:Q15) %>% 
  mutate(val = as.numeric(val),
         val = ifelse(var %in% c("Q3", "Q4", "Q9", "Q10"), 6 - val, val),
         factor = ifelse(var %in% c("Q1", "Q2", "Q3", "Q4"), "leadership",
                         ifelse(var %in% c("Q5", "Q6", "Q7", "Q8", "Q9", "Q10"), "teamwork",
                                ifelse(var %in% c("Q11", "Q12", "Q13", "Q14"), "sit.awareness",
                                       ifelse(var == "Q15", "overall", val)))),
         role = ifelse(var %in% c("Q1", "Q3", "Q11", "Q13"), "driver",
                       ifelse(var %in% c("Q2", "Q4", "Q12", "Q14"), "co_driver", NA)))


# identified 3 teams with multiple attempts
# take the first and remove others
# scale %>% 
#   group_by(team) %>% 
#   summarise(n = n()) %>% 
#   filter(n > 15)

scale <- scale %>% 
  filter(!ResponseId %in% c("R_2tx23uU6otUDuhT", "R_1KxQ79P4YOIIopo", "R_2QQZbzujRrlSAJZ", "R_3JrNL95piufohjm"))

# calculate scores for each team
teamwork <- scale %>%
  group_by(team, factor) %>% 
  summarise(score = sum(val)) %>% 
  spread(factor, score)

# calculate scores for driver and co-driver items separately
role <- scale %>%
  filter(!is.na(role)) %>% 
  group_by(team, factor, role) %>% 
  summarise(score = sum(val)) %>% 
  unite(factor, factor, role) %>% 
  spread(factor, score)

teamwork <- teamwork %>% left_join(role, by = "team")

# reliability for scale 
# overall (a = .84)
x <- scale %>%
  select(team, ResponseId, var, val) %>% 
  spread(var, val) %>% 
  select(-team, -ResponseId)

round(psych::alpha(x)$total$raw_alpha, 2)

# each factor separately
factors <- c("leadership", "teamwork", "sit.awareness")

lapply(factors, function(i) {
x <- scale %>%
  select(team, ResponseId, factor, var, val) %>% 
  filter(factor == i) %>% 
  spread(var, val) %>% 
  select(-team, -ResponseId, -factor)

round(psych::alpha(x)$total$raw_alpha, 2)
})


# join comms coding and teamwork scale data
coding <- comms %>% left_join(teamwork, by = "team")


# read sim data
# d <- read_csv("data/190205_master_data.csv")
d <- read_csv("data/181123_master_team_data.csv")

# add issue note for team with network issue
d <- d %>% 
  mutate(issue = ifelse(team %in% c("17080712_1", "17032209_2", "17032711_1"),
                        TRUE, issue),
         issue_note = ifelse(team %in% c("17080712_1", "17032209_2", "17032711_1"),
                             "major driver-drone networking error", issue_note),
         issue = ifelse(team %in% c("17032915_1", "17040515_1"),
                        TRUE, issue),
         issue_note = ifelse(team %in% c("17032915_1", "17040515_1"),
                             "minor driver-drone networking error", issue_note),
         issue = ifelse(team %in% c("17040311_1", "17032709_1", "17032209_1", "17032111_1", 
                                    "17032109_1", "17032011_1", "17032009_1", "17040609_2",
                                    "17031609_1", "17080810_1", "17081512_2"),
                        TRUE, issue),
         issue_note = ifelse(team %in% c("17040311_1", "17032709_1", "17032209_1", "17032111_1", 
                                         "17032109_1", "17032011_1", "17032009_1", "17040609_2"),
                             "no comms coding becuase could not hear audio in recording", issue_note),
         issue_note = ifelse(team %in% c("17031609_1"), "major driver-drone networking error - codriver could not read arrows and saw roughly 10% of the driver's traffic", issue_note),
         issue_note = ifelse(team %in% c("17080810_1"), "major driver-drone networking error - codriver saw no-go signs in place of all arrows and saw roughly 40% of the driver's traffic", issue_note),
         issue_note = ifelse(team %in% c("17081512_2"), "major driver-drone networking error - codriver saw no-go signs in place of all arrows and saw roughly 10% of the driver's traffic", issue_note))


# calculate more vars for collisions
tmp <- d %>%
  select(team, collisions_1:collisions_5) %>%
  gather(var, val, collisions_1:collisions_5) %>%
  separate(var, into = c("v1", "lap")) %>%
  select(team, lap, val) %>%
  mutate(
    lap = as.numeric(lap))

# identify the lap with the lowest number of collisions
min <- tmp %>%
  group_by(team) %>%
  filter(val == min(val)) %>%
  mutate(n= 1:n()) %>%
  filter(n == 1) %>%
  summarise(min_lap_overall = lap,
            min_value_overall = val)

# identify the lap with the highest number of collisions
max <- tmp %>%
  group_by(team) %>%
  filter(val == max(val)) %>%
  mutate(n= 1:n()) %>%
  filter(n == 1) %>%
  summarise(max_lap_overall = lap,
            max_value_overall = val)

# identify whether the lowest lap occurred after the highest lap (improvement)
tmp1 <- min %>%
  left_join(max, by = "team") %>%
  group_by(team) %>%
  mutate(
    min_lap_last = min_lap_overall > max_lap_overall)

# calculate min lap before the max lap
tmp2 <- tmp1 %>%
  left_join(tmp, by = "team") %>%
  filter(min_lap_last == TRUE) %>%
  group_by(team) %>%
  filter(lap <= max_lap_overall) %>%
  filter(val == min(val)) %>%
  mutate(n = n()) %>%
  filter(n == 1) %>%
  summarise(min_lap_before = lap,
            min_value_before = val)

# calculate min lap after the max lap
tmp3 <- tmp1 %>%
  left_join(tmp, by = "team") %>%
  filter(min_lap_last == FALSE) %>%
  group_by(team) %>%
  filter(lap >= max_lap_overall) %>%
  filter(val == min(val)) %>%
  mutate(n = n()) %>%
  filter(n == 1) %>%
  summarise(min_lap_after = lap,
            min_value_after = val)

# calculate max lap before the min lap
tmp4 <- tmp1 %>%
  left_join(tmp, by = "team") %>%
  filter(min_lap_last == FALSE) %>%
  group_by(team) %>%
  filter(lap <= min_lap_overall) %>%
  filter(val == max(val)) %>%
  mutate(n = n()) %>%
  filter(n == 1) %>%
  summarise(max_lap_before = lap,
            max_value_before = val)

# calculate max lap after the min lap
tmp5 <- tmp1 %>%
  left_join(tmp, by = "team") %>%
  filter(min_lap_last == TRUE) %>%
  group_by(team) %>%
  filter(lap >= min_lap_overall) %>%
  filter(val == max(val)) %>%
  mutate(n = 1:n()) %>%
  filter(n == 1) %>%
  summarise(max_lap_after = lap,
            max_value_after = val)

# combine dataframes
datalist <- list(tmp1, tmp2, tmp3, tmp4, tmp5)

x <- datalist %>% reduce(left_join, by = "team")

# calculate variables
x <- x %>%
  left_join(d %>% select(team, collisions_1, collisions_5), by = "team") %>%
  group_by(team) %>%
  summarise(
    collisions_overall_improve = collisions_1 - collisions_5,
    collisions_initial_improve = collisions_1 - min_value_overall,
    collisions_late_decline = min_value_overall - collisions_5,
    collisions_max_improve = ifelse(min_lap_last == TRUE, max_value_overall - min_value_overall,
                                    ifelse(min_lap_last == FALSE, max_value_before - min_value_overall, NA)), # should it be max_lap_overall - min_lap_overall_after? or should this be an extra variable?
    collisions_max_decline = ifelse(min_lap_last == FALSE, min_value_overall - max_value_overall,
                                    ifelse(min_lap_last == TRUE, min_value_overall - max_value_after, NA)),
    collisions_max_recovery = ifelse(min_lap_last == TRUE, max_value_overall - min_value_overall,
                                     ifelse(min_lap_last == FALSE, max_value_overall - min_value_after, NA))) %>%
  select(team, collisions_overall_improve:collisions_max_recovery)


d <- d %>% left_join(x, by = "team")

# calculate more vars for speed
tmp <- d %>% 
  select(team, speed_1:speed_5) %>% 
  gather(var, val, speed_1:speed_5) %>% 
  separate(var, into = c("v1", "lap")) %>% 
  select(team, lap, val) %>% 
  mutate(
    lap = as.numeric(lap))

# identify the lap with the lowest number of collisions
min <- tmp %>% 
  group_by(team) %>% 
  filter(val == min(val)) %>% 
  mutate(n= 1:n()) %>% 
  filter(n == 1) %>% 
  summarise(min_lap_overall = lap,
            min_value_overall = val)

# identify the lap with the highest number of collisions
max <- tmp %>% 
  group_by(team) %>% 
  filter(val == max(val)) %>% 
  mutate(n= 1:n()) %>% 
  filter(n == 1) %>% 
  summarise(max_lap_overall = lap,
            max_value_overall = val)

# identify whether the lowest lap occurred after the highest lap (improvement)
tmp1 <- min %>% 
  left_join(max, by = "team") %>% 
  group_by(team) %>% 
  mutate(
    min_lap_last = min_lap_overall > max_lap_overall)

# calculate min lap before the max lap
tmp2 <- tmp1 %>% 
  left_join(tmp, by = "team") %>%
  filter(min_lap_last == TRUE) %>% 
  group_by(team) %>% 
  filter(lap <= max_lap_overall) %>% 
  filter(val == min(val)) %>% 
  mutate(n = n()) %>% 
  filter(n == 1) %>% 
  summarise(min_lap_before = lap,
            min_value_before = val)

# calculate min lap after the max lap
tmp3 <- tmp1 %>% 
  left_join(tmp, by = "team") %>%
  filter(min_lap_last == FALSE) %>% 
  group_by(team) %>% 
  filter(lap >= max_lap_overall) %>% 
  filter(val == min(val)) %>% 
  mutate(n = n()) %>% 
  filter(n == 1) %>% 
  summarise(min_lap_after = lap,
            min_value_after = val)

# calculate max lap before the min lap
tmp4 <- tmp1 %>% 
  left_join(tmp, by = "team") %>%
  filter(min_lap_last == FALSE) %>% 
  group_by(team) %>% 
  filter(lap <= min_lap_overall) %>% 
  filter(val == max(val)) %>% 
  mutate(n = n()) %>% 
  filter(n == 1) %>% 
  summarise(max_lap_before = lap,
            max_value_before = val)

# calculate max lap after the min lap
tmp5 <- tmp1 %>% 
  left_join(tmp, by = "team") %>%
  filter(min_lap_last == TRUE) %>% 
  group_by(team) %>% 
  filter(lap >= min_lap_overall) %>% 
  filter(val == max(val)) %>% 
  mutate(n = 1:n()) %>% 
  filter(n == 1) %>% 
  summarise(max_lap_after = lap,
            max_value_after = val)

# combine dataframes
datalist <- list(tmp1, tmp2, tmp3, tmp4, tmp5)

x <- datalist %>% reduce(left_join, by = "team")

# calculate variables
x <- x %>% 
  left_join(d %>% select(team, speed_1, speed_5), by = "team") %>% 
  group_by(team) %>% 
  summarise(
    speed_overall_improve = speed_5 - speed_1,
    speed_initial_improve = max_value_overall - speed_1,
    speed_late_decline = speed_5 - max_value_overall,
    speed_max_improve = ifelse(min_lap_last == FALSE, max_value_overall - min_value_overall,
                               ifelse(min_lap_last == TRUE, max_value_after - min_value_overall, NA)),
    speed_max_decline = ifelse(min_lap_last == TRUE, min_value_overall - max_value_overall,
                               ifelse(min_lap_last == FALSE, min_value_overall - max_value_before, NA)), # should it be max_lap_overall - min_lap_overall_after? or should this be an extra variable?
    speed_max_recovery = ifelse(min_lap_last == FALSE, max_value_overall - min_value_overall,
                                ifelse(min_lap_last == TRUE, max_value_after - min_value_overall, NA))) %>% 
  select(team, speed_overall_improve:speed_max_recovery)

# join to main dataset
d <- d %>% left_join(x, by = "team")

# join comms vars with main dataset
coding <- coding %>% left_join(d, by = "team")

# match comms coding to laps and calculate trends for each driver for each variable
source("R/comm_trends.R")

vars <- coding %>% left_join(comms, by = "team")

# match comms coding to fog event overall and per lap
source("R/fog_comms.R")

vars <- vars %>% left_join(fog_vars, by = "team")

# calculate collisions, speed, and distance for no fog
# collisions
col_no_fog <- vars %>% 
  select(team, collisions_animal_1:collisions_animal_5, collisions_block_1:collisions_block_5,
         collisions_fall_1:collisions_fall_5, collisions_ice_1:collisions_ice_5,
         collisions_none_1:collisions_none_5) %>% 
  gather(var, val, -team) %>% 
  mutate(lap = str_extract(var, "_[1-5]"),
         lap = as.numeric(str_remove(lap, "_")),
         var = str_remove(var, "_[1-5]")) %>% 
  group_by(team, lap) %>% 
  summarise(collisions_no_fog = sum(val, na.rm = T)) %>% 
  gather(var, val, collisions_no_fog) %>% 
  unite(var, var, lap) %>% 
  spread(var, val) %>% 
  group_by(team) %>%
  mutate(collisions_no_fog_overall = sum(collisions_no_fog_1, collisions_no_fog_2, collisions_no_fog_3,
                                         collisions_no_fog_4, collisions_no_fog_5))

# speed
speed_no_fog <- vars %>% 
  select(team, speed_animal_1:speed_animal_5, speed_block_1:speed_block_5,
         speed_fall_1:speed_fall_5, speed_ice_1:speed_ice_5,
         speed_none_1:speed_none_5) %>% 
  gather(var, val, -team) %>% 
  mutate(lap = str_extract(var, "_[1-5]"),
         lap = as.numeric(str_remove(lap, "_")),
         var = str_remove(var, "_[1-5]")) %>% 
  group_by(team, lap) %>% 
  summarise(speed_no_fog = mean(val, na.rm = T)) %>% 
  gather(var, val, speed_no_fog) %>% 
  unite(var, var, lap) %>% 
  spread(var, val) %>% 
  group_by(team) %>%
  mutate(speed_no_fog_overall = (speed_no_fog_1 + speed_no_fog_2 + speed_no_fog_3 +
                                   speed_no_fog_4 + speed_no_fog_5)/5)

# distance
dist_no_fog <- vars %>% 
  select(team, distance_animal_1:distance_animal_5, distance_block_1:distance_block_5,
         distance_fall_1:distance_fall_5, distance_ice_1:distance_ice_5,
         distance_none_1:distance_none_5) %>% 
  gather(var, val, -team) %>% 
  mutate(lap = str_extract(var, "_[1-5]"),
         lap = as.numeric(str_remove(lap, "_")),
         var = str_remove(var, "_[1-5]")) %>% 
  group_by(team, lap) %>% 
  summarise(distance_no_fog = sum(val, na.rm = T)) %>% 
  gather(var, val, distance_no_fog) %>% 
  unite(var, var, lap) %>% 
  spread(var, val) %>% 
  group_by(team) %>%
  mutate(distance_no_fog_overall = sum(distance_no_fog_1, distance_no_fog_2, distance_no_fog_3,
                                         distance_no_fog_4, distance_no_fog_5))

datalist <- list(vars, col_no_fog, speed_no_fog, dist_no_fog)

vars <- datalist %>% reduce(left_join, by = "team")

# add age and sex to data
demo <- read_csv("data/demographics.csv") %>% 
  mutate(team = str_remove(uid, "-[d-g][1-2]"),
         sex = abs(gender - 2),
         aus_born = ifelse(aus_born == 1, 1, 0),
         eng_fl = ifelse(eng_fl == 1, 1, 0),
         role = ifelse(str_detect(uid, "-[d-g][2]"), "driver", "co_driver")) %>% 
  select(team, age, sex, aus_born, aus_years, eng_fl, dic_use, role) %>% 
  gather(var, val, age:dic_use) %>% 
  unite(var, var, role) %>% 
  spread(var, val) %>% 
  filter(!is.na(age_co_driver)) %>% 
  group_by(team) %>% 
  mutate(mean_age = (age_driver + age_co_driver)/2,
    prop_female = abs(1-(sex_driver + sex_co_driver)/2))

vars <- vars %>% left_join(demo, by = "team")

# last saved 27 Feb 2020
vars %>% write_csv("data/200303_comms_vars.csv")

# save data for SK's SPSS efa script
# data <- vars
# 
# data[is.na(data)] <- 99999
# table(is.na(data))
# table(data == 99999)
# 
# data %>% write_csv("data/comms_data_4_SK_efa.csv")




# Need to conduct EFA in SPSS before running final part of script ---------
d <- read_csv("data/200303_comms_vars.csv")

# add efa factors to data
# read factor scores for comms vars
efa <- read_csv("data/200221_comms_efa_spss_n54.csv") %>% 
  select(team, inconsistent_codriver, terrible_codriver, helpful_exchange)

# add comms factors to dataset
d <- d %>% left_join(efa, by = "team")

# last saved 27 Feb 2020
d %>% write_csv("data/200303_comms_efa_vars.csv")

