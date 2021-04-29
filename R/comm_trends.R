library(broom)
library(tidyverse)

source("R/start_time.R")

# vars <- read_csv("data/191210_comms_vars.csv")
# filter(issue == FALSE) # remove teams with networking issue

# d <- read_csv("data/200221_comms_raw.csv")

# combine lap start times with comms coding
d <- data %>% 
  left_join(start, by = "Participant") %>% 
  group_by(Participant) %>% 
  mutate(time = time - start_time)

# check that first comms time is after drive start time
d %>%
  group_by(team) %>%
  mutate(n = 1:n()) %>%
  filter(n == 1) %>%
  select(team, Participant, time, start_time) %>%
  filter(time < 0)

# add laps to comms coding
lap_time <- read_csv("data/lap_times.csv") %>% 
  mutate(var = "lap") %>% 
  unite(lap, var, lap) %>% 
  spread(lap, lap_time)

d <- d %>% 
  left_join(lap_time, "team")
  
d <- d %>% 
  group_by(team) %>% 
  mutate(lap = ifelse(time < lap_2, 1,
               ifelse(time > lap_2 & time < lap_3, 2,
               ifelse(time > lap_3 & time < lap_4, 3,
               ifelse(time > lap_4 & time < lap_5, 4,
               ifelse(time > lap_5, 5, NA))))))

# calculate comms vars for each lap
comms <- d %>% 
  group_by(team, lap) %>% 
  summarise(co_info_help = sum(event == "Co-Driver Gives Information -Helpful [J]"),
            co_info_harm = sum(event == "Co-Driver Gives Information -Harmful [N]"),
            co_instruct_help = sum(event == "Co-Driver Gives Instruction -Helpful [K]"),
            co_instruct_harm = sum(event == "Co-Driver Gives Instruction -Harmful [M]"),
            co_total_help = co_info_help + co_instruct_help,
            co_total_harm = co_info_harm + co_instruct_harm,
            co_info_harm_ratio = co_info_harm/co_info_help,
            co_instruct_harm_ratio = co_instruct_harm/co_instruct_help,
            co_total_harm_ratio = co_total_harm/co_total_help,
            co_redundant = sum(event == "Co-Driver Gives Redundant Info/Instructions [I]"),
            co_question = sum(event == "Co-Driver Asks Question [U]"),
            co_total = sum(co_total_help, co_total_harm, co_redundant, co_question),
            co_redundant_ratio = co_redundant/co_total,
            co_question_ratio = co_question/co_total,
            drive_question = sum(event == "Driver Asks for Help [E]"),
            drive_informs = sum(event == "Driver Informs Co-Driver [R]"),
            drive_frust = sum(event == "Driver Frustration/Anxiety/Surprise [F]"),
            drive_question_ratio = drive_question/drive_informs,
            drive_frust_ratio = drive_frust/(drive_question+drive_informs),
            drive_total = sum(drive_question, drive_informs),
            drive_ratio = drive_total/co_total,
            co_ratio = co_total/drive_total)

# convert Inf to NA as standardise() won't work if any Inf present
comms[comms==Inf] <- NA

# remove Ps data for each var with more than 2 laps missing
name <- names(comms)[-c(1,2)]

comms %>% 
  select(team, lap, name[1]) %>%
  mutate(var = name[1]) %>% 
  rename(val = name[1]) %>% 
  group_by(team) %>% 
  mutate(n = sum(!is.na(val)),
         val = ifelse(n < 4, NA, val)) %>% 
  ungroup() %>% 
  spread(var, val) %>% 
  select(-n, -team, -lap)


x <- lapply(name, function(i) {
  comms %>% 
    select(team, lap, i) %>%
    mutate(var = i) %>% 
    rename(val = i) %>% 
    group_by(team) %>% 
    mutate(n = sum(!is.na(val)),
           val = ifelse(n < 4, NA, val)) %>% 
    ungroup() %>% 
    spread(var, val) %>% 
    select(-n, -team, -lap)
})

trends <- comms %>% 
  select(team, lap) %>% 
  bind_cols(x) %>%
  gather(var, val, co_info_help:co_ratio) %>%
  unite(var, var, lap, sep = "-") %>%
  spread(var, val)

# prepare comms vars for final df
comms <- comms %>% 
  gather(var, val, -team, -lap) %>%
  unite(var, var, lap, sep = "_") %>%
  spread(var, val)

# identify problems with the matching of times and laps
# investigate any drivers who do not have communication in each lap
# THESE 3 TEAMS JUST HAD LITTLE COMMS (17040414_2, 17040509_1, 17041112_1)
ids <- d %>% 
  group_by(team, lap) %>% 
  mutate(n = n()) %>% 
  select(team, lap, n) %>% 
  unique() %>% 
  arrange(team, lap) %>% 
  group_by(team) %>% 
  mutate(n_laps = n()) %>% 
  filter(n_laps < 5) %>% 
  select(team) %>% unique()

lapply(ids$team, function(i) {
  d %>% 
    filter(team == i) %>% 
    group_by(team, lap) %>% 
    mutate(n = n()) %>% 
    select(team, lap, n) %>% 
    unique() %>% 
    arrange(team, lap) %>% 
    group_by(team) %>% 
    mutate(n_laps = n())
})


d %>% 
  group_by(Participant, lap) %>% 
  mutate(n = n()) %>% 
  select(Participant, lap, n) %>% 
  unique() %>% 
  arrange(Participant, lap) %>% 
  ggplot(aes(x=lap, y=n)) +
  geom_col() +
  facet_wrap(~Participant, scales = "free_y") +
  theme_minimal()
  
# prepare date for polynomial regression
# function to standardise data for regression
standardise <- function(x) {
  (x - mean(x, na.rm = TRUE)) / sd(x, na.rm = TRUE)
}


poly <- trends %>%
  ungroup() %>% 
  mutate_if(is.numeric, standardise) %>% 
  gather(var, val, -team) %>%
  separate(var, into = c("var", "lap"), sep = "-") %>%
  mutate(lap = as.numeric(lap))

# fit trends
poly <- poly %>%
  na.omit() %>% 
  nest(data = c(lap, val)) %>%
  mutate(fit = map(data, ~ lm(val ~ poly(lap, degree = 3), .)),
         tidy_fit = map(fit, tidy)) %>%
  unnest(tidy_fit) %>%
  select(-statistic, -std.error) %>%
  gather(variable, value, -(team:term)) %>%
  mutate(polynomial = str_extract(term, "[0-9]+$"),
         polynomial = str_c("poly_", polynomial),
         polynomial = ifelse(is.na(polynomial), "intercept", polynomial)) %>%
  unite(var, var, polynomial, sep = "_") %>%
  unite(var, var, variable) %>%
  select(team, var, value) %>%
  spread(var, value)

# combine comms variables per lap and polynomial variables
comms <- comms %>% left_join(poly, by = "team")

