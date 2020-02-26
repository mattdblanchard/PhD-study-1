
# Match the comms data to the fog event -----------------------------------
fog <- read_csv("data/event_times.csv") %>%
  filter(event %in% c("fog_start", "fog_end")) %>%
  group_by(team, event, lap) %>%
  mutate(n = n(),
         cum_n = 1:n()) %>%
  spread(event, event_time) %>%
  select(team, lap, fog_start, fog_end, n, cum_n)

# which teams ids are in the comms code data?
ids <- d %>% ungroup() %>% select(team) %>% unique()

# compute comms events in fog and not in fog
fog_event <- map(ids$team, function(t) {
  l <- d %>% ungroup() %>% filter(team == t) %>% select(lap) %>% unique()
  
  map(l$lap, function(i) {
    
    laps <- i
    teams <- t
    assign(paste0("lap", laps), fog %>% ungroup() %>% filter(team == teams) %>% filter(lap == laps) %>% select(fog_start, fog_end))
    
    x <- fog %>% filter(team == teams, lap == laps) %>% ungroup() %>% select(n)
    
    if (is.na(x$n[1])) {
      d %>%
        filter(team == teams & lap == laps) %>%
        select(team, lap, event, time) %>%
        mutate(fog = FALSE)
      
    } else if (is.na(x$n[1])) {
      d.frame(team = teams,
                 lap = laps,
                 event = event,
                 time = time,
                 fog = FALSE)
      
    } else if (x$n[1] == 1) {
      d %>%
        filter(team == teams) %>%
        select(team, lap, event, time) %>%
        filter(lap == laps) %>%
        mutate(fog = ifelse(time > get(paste0("lap", laps))$fog_start[1] &
                              time < get(paste0("lap", laps))$fog_end[1], TRUE, FALSE))
    } else if (x$n[1] == 2) {
      d %>%
        filter(team == teams) %>%
        select(team, lap, event, time) %>%
        filter(lap == laps) %>%
        mutate(fog = ifelse(time > get(paste0("lap", laps))$fog_start[1] &
                              time < get(paste0("lap", laps))$fog_end[1], TRUE,
                     ifelse(time > get(paste0("lap", laps))$fog_start[2] &
                              time < get(paste0("lap", laps))$fog_end[2], TRUE, FALSE)))
    } else if (x$n[1] == 3) {
      d %>%
        filter(team == teams) %>%
        select(team, lap, event, time) %>%
        filter(lap == laps) %>%
        mutate(fog = ifelse(time > get(paste0("lap", laps))$fog_start[1] &
                              time < get(paste0("lap", laps))$fog_end[1], TRUE,
                     ifelse(time > get(paste0("lap", laps))$fog_start[2] &
                              time < get(paste0("lap", laps))$fog_end[2], TRUE,
                     ifelse(time > get(paste0("lap", laps))$fog_start[3] &
                              time < get(paste0("lap", laps))$fog_end[3], TRUE, FALSE))))
      
    } else if (x$n[1] == 4) {
      d %>%
        filter(team == teams) %>%
        select(team, lap, event, time) %>%
        filter(lap == laps) %>%
        mutate(fog = ifelse(time > get(paste0("lap", laps))$fog_start[1] &
                              time < get(paste0("lap", laps))$fog_end[1], TRUE,
                     ifelse(time > get(paste0("lap", laps))$fog_start[2] &
                              time < get(paste0("lap", laps))$fog_end[2], TRUE,
                     ifelse(time > get(paste0("lap", laps))$fog_start[3] &
                              time < get(paste0("lap", laps))$fog_end[3], TRUE, 
                     ifelse(time > get(paste0("lap", laps))$fog_start[4] &
                              time < get(paste0("lap", laps))$fog_end[4], TRUE, FALSE)))))
      
    } else {
      d %>%
        filter(team == teams) %>%
        select(team, lap, event, time) %>%
        filter(lap == laps) %>%
        mutate(fog = ifelse(time > get(paste0("lap", laps))$fog_start[1] &
                              time < get(paste0("lap", laps))$fog_end[1], TRUE,
                     ifelse(time > get(paste0("lap", laps))$fog_start[2] &
                              time < get(paste0("lap", laps))$fog_end[2], TRUE,
                     ifelse(time > get(paste0("lap", laps))$fog_start[3] &
                              time < get(paste0("lap", laps))$fog_end[3], TRUE, 
                     ifelse(time > get(paste0("lap", laps))$fog_start[4] &
                              time < get(paste0("lap", laps))$fog_end[4], TRUE,
                     ifelse(time > get(paste0("lap", laps))$fog_start[5] &
                              time < get(paste0("lap", laps))$fog_end[5], TRUE,
                     ifelse(time > get(paste0("lap", laps))$fog_start[6] &
                              time < get(paste0("lap", laps))$fog_end[6], TRUE,
                     ifelse(time > get(paste0("lap", laps))$fog_start[7] &
                              time < get(paste0("lap", laps))$fog_end[7], TRUE,
                     ifelse(time > get(paste0("lap", laps))$fog_start[8] &
                              time < get(paste0("lap", laps))$fog_end[8], TRUE,
                     ifelse(time > get(paste0("lap", laps))$fog_start[9] &
                              time < get(paste0("lap", laps))$fog_end[9], TRUE,
                     ifelse(time > get(paste0("lap", laps))$fog_start[10] &
                              time < get(paste0("lap", laps))$fog_end[10], TRUE,
                     ifelse(time > get(paste0("lap", laps))$fog_start[11] &
                              time < get(paste0("lap", laps))$fog_end[11], TRUE, FALSE))))))))))))
    }
  })
})

# bind rows in single df
x <- map(1:length(fog_event), function(i) {
  bind_rows(fog_event[[i]])
})

fog_event <- bind_rows(x)


# check the number of rows of comms events coded match in the original data and the fog data
fog_event %>% 
  group_by(team) %>% 
  summarise(n = n()) %>% 
  left_join(d %>% 
              group_by(team) %>% 
              summarise(n2 = n())) %>% 
  group_by(team) %>% 
  summarise(n = n - n2) %>% 
  filter(n != 0)

# calculate fog variables overall
fog_overall <- fog_event %>% 
  mutate(fog = ifelse(fog == TRUE, "fog",
               ifelse(fog == FALSE, "no_fog", NA))) %>% 
  group_by(team, fog) %>% 
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
            co_ratio = co_total/drive_total) %>% 
  gather(var, val, -team, -fog) %>% 
  unite(var, var, fog) %>% 
  spread(var, val)

# calculate fog variables per lap
fog_lap <- fog_event %>% 
  mutate(fog = ifelse(fog == TRUE, "fog",
               ifelse(fog == FALSE, "no_fog", NA))) %>% 
  group_by(team, fog, lap) %>% 
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
            co_ratio = co_total/drive_total) %>% 
  gather(var, val, -team, -fog, -lap) %>% 
  unite(var, var, fog, lap) %>% 
  spread(var, val)

fog_vars <- fog_overall %>% left_join(fog_lap, by = "team")
