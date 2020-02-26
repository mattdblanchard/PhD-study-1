# load the data
files <- dir("data/comm_coding_files/Final_StrategyCodingFiles/")

# read strategy coding files for each team
# to get start times so comms data can be matched to laps
d <- lapply(files, function(i) {
  data <- read_csv(c(paste0("data/comm_coding_files/Final_StrategyCodingFiles/", i)), skip = 1, col_names = FALSE) %>% 
    mutate(Participant = i)
})

# bind the rows to a single tibble, rename columns and 
# clean Participant values so they are consistent with who_coded.csv
  
d[[29]] <- d[[29]] %>%
  rename(X6 = X5, X5 = X4, X4 = X3, X3 = X2, X2 = X1) %>%
  mutate(X1 = 1:n())

d <- bind_rows(d) %>% 
  rename(event = X3, time = X4) %>% 
  select(Participant, event, time) %>% 
  mutate(Participant = str_remove(Participant, "_processed.m4v_coded"),
         Participant = str_remove(Participant, "_reviewed_final"),
         Participant = str_remove(Participant, "_reviewed"),
         Participant = str_remove(Participant, "_Reviewed"),
         Participant = str_remove(Participant, "_ reviewed"),
         Participant = str_remove(Participant, "- REVIEWED VIV'S"),
         Participant = str_remove(Participant, ".csv"))

start <- d %>% 
  group_by(Participant) %>% 
  mutate(n = 1:n()) %>% 
  filter(n == 1) %>% 
  select(-n)

# manually add start times for missing participants
# did this by referring to the videos
options(stringsAsFactors = FALSE)
x <- data.frame(Participant = c("2016-10-11 14-34-09-7", "2017-03-15 15-28-04-7", "2017-03-22 09-16-41-19",
                           "2017-03-22 16-01-14-21", "2017-03-27 14-28-04-31", "2017-03-31 09-22-08-44",
                           "2017-04-03 09-24-49-4", "2017-08-08 10-45-08-44", "2017-08-09 10-12-54-46",
                           "2017-03-14 09-24-58", "2017-03-15 15-28-51", "2017-03-17 11-22-54",
                           "2017-03-27 11-22-21", "2017-03-29 15-21-05", "2017-04-03 14-20-02",
                           "2017-04-04 11-30-45", "2017-04-04 11-30-54", "2017-04-05 11-21-44",
                           "2017-04-06 09-26-33", "2017-04-11 12-28-49"),
           event = "Normal Driving Non-Instructed [J]",
           time = c(209, 401, 1201, 244, 53, 250, 68, 487, 340,
                    1041, 340, 375, 508, 291, 319, 998, 728, 376, 458, 331))

start <- bind_rows(start, x) %>% 
  rename(start_time = time) %>% 
  select(-event)

# manually checked some videos
# need to modify start times that do not match
start <- start %>% 
  mutate(
    start_time = ifelse(Participant == "2017-04-07 11-20-56-35", 242, 
                 ifelse(Participant == "2017-03-28 11-22-56-35", 631, 
                 ifelse(Participant == "2017-03-28 11-22-46-34", 683, 
                 ifelse(Participant == "2017-03-15 09-26-59-6", 832, 
                 ifelse(Participant == "2017-03-27 14-28-04-31", 1032,
                 ifelse(Participant == "2017-04-05 09-22-15-26", 376,
                 ifelse(Participant == "2017-08-15 10-18-12-49", 401,
                 ifelse(Participant == "2017-08-15 12-26-48-52", 185,
                 ifelse(Participant == "2017-03-24 09-24-02-22", 151,
                        start_time))))))))))



# NEED TO CHECK START TIMES FOR COMMENTED ---------------------------------
# 2016-10-11 14-34-09-7 == 209
# 2017-03-15 15-28-04-7 == 401
# 2017-03-22 09-16-41-19 == 1201
# 2017-03-22 16-01-14-21 == 244
# 2017-03-27 14-28-04-31 == 53
# 2017-03-31 09-22-08-44 == 250 (check 1st comm time)
# 2017-04-03 09-24-49-4 == 68
# 2017-08-08 10-45-08-44 == 487
# 2017-08-09 10-12-54-46 == 340 (check 1st comm time)
# 
# 
# co-driver
# 2017-03-14 09-24-58 == 1041
# 2017-03-15 15-28-51 == 340
# 2017-03-17 11-22-54 == 375
# 2017-03-27 11-22-21 == 508
# 2017-03-29 15-21-05 == 291
# 2017-04-03 14-20-02 == 319 (slightly before)
# 2017-04-04 11-30-45 == 998 (slightly before)
# 2017-04-04 11-30-54 == 728
# 2017-04-05 11-21-44 == 376
# 2017-04-06 09-26-33 == 458
# 2017-04-11 12-28-49 == 331
