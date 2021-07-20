# Reliability overall

# Connor_Davidson_Resilience
x <- surveys_s1$Connor_Davidson_Resilience %>% 
  filter(uid %in% all_id$uid) %>% 
  select(uid, item_num, response) %>% 
  spread(item_num, response) %>% 
  select(-uid)

res <- tibble(var = "resilience",
                  rel = round(psych::alpha(x)$total$raw_alpha,2))

# mini-IPIP
surveys_s1$Mini_IPIP %>% filter(uid %in% all_id$uid)

fac <- unique(surveys_s1$Mini_IPIP$facet)

ipip <- map(fac, function(i) {
  x <- surveys_s1$Mini_IPIP %>% 
    filter(uid %in% all_id$uid) %>% 
    mutate(response = ifelse(key == "-1", 6-response, response)) %>% 
    filter(facet == i) %>% 
    select(uid, item_num, response) %>%
    spread(item_num, response) %>%
    select(-uid)
  
  tibble(var = i,
         rel = round(psych::alpha(x)$total$raw_alpha,2))
})

ipip <- bind_rows(ipip)

# RAPM
# accuracy
x <- surveys_s1$Ravens_Advanced_Progressive_Matrices %>% 
  filter(uid %in% all_id$uid) %>% 
  select(uid, ApmItemNum, Stimulus_ACC) %>% 
  mutate(Stimulus_ACC = as.numeric(Stimulus_ACC)) %>% 
  spread(ApmItemNum, Stimulus_ACC) %>% 
  select(-uid)

fluid <- tibble(var = "gf_accuracy",
              rel = round(psych::alpha(x)$total$raw_alpha,2))

# confidence
x <- surveys_s1$Ravens_Advanced_Progressive_Matrices %>% 
  filter(uid %in% all_id$uid) %>% 
  select(uid, ApmItemNum, Confidence_RESP) %>% 
  spread(ApmItemNum, Confidence_RESP) %>% 
  select(-uid)

conf <- tibble(var = "confidence",
              rel = round(psych::alpha(x)$total$raw_alpha,2))

# bias
# calculate bias for the odd and even items separately
# correlate the two sets then ajust with the Spearman-Brown prophecy formula
items <- surveys_s1$Ravens_Advanced_Progressive_Matrices %>% select(ApmItemNum) %>% unique()

odd <- items[c(seq(1, 20, 2)),]
even <- items[c(seq(2, 20, 2)),]

odd <- surveys_s1$Ravens_Advanced_Progressive_Matrices %>% 
  filter(uid %in% all_id$uid) %>% 
  filter(ApmItemNum %in% odd$ApmItemNum) %>% 
  group_by(uid) %>% 
  summarise(acc = 100 * mean(Stimulus_ACC),
            conf = mean(Confidence_RESP),
            bias = conf - acc)

even <- surveys_s1$Ravens_Advanced_Progressive_Matrices %>% 
  filter(uid %in% all_id$uid) %>% 
  filter(ApmItemNum %in% even$ApmItemNum) %>% 
  group_by(uid) %>% 
  summarise(acc = 100 * mean(Stimulus_ACC),
            conf = mean(Confidence_RESP),
            bias = conf - acc)

cor <- cor.test(odd$bias, even$bias)

# Adjust with the Spearman-Brown prophecy formula = (2*r) / (1+r)  
bias <- tibble(var = "bias",
              rel = round((2*cor$estimate) / (1+cor$estimate),2))


# discrimination
# calculate for the odd and even items separately
# correlate the two sets then ajust with the Spearman-Brown prophecy formula
items <- surveys_s1$Ravens_Advanced_Progressive_Matrices %>% select(ApmItemNum) %>% unique()

odd <- items[c(seq(1, 20, 2)),]
even <- items[c(seq(2, 20, 2)),]

odd <- surveys_s1$Ravens_Advanced_Progressive_Matrices %>% 
  filter(uid %in% all_id$uid) %>% 
  filter(ApmItemNum %in% odd$ApmItemNum) %>% 
  group_by(uid, Stimulus_ACC) %>% 
  summarise(conf = mean(Confidence_RESP, na.rm = T)) %>% 
  gather(var, val, conf) %>% 
  unite(var, var, Stimulus_ACC) %>% 
  spread(var, val) %>% 
  group_by(uid) %>% 
  summarise(discrim = conf_TRUE - conf_FALSE)

even <- surveys_s1$Ravens_Advanced_Progressive_Matrices %>% 
  filter(uid %in% all_id$uid) %>% 
  filter(ApmItemNum %in% even$ApmItemNum) %>% 
  group_by(uid, Stimulus_ACC) %>% 
  summarise(conf = mean(Confidence_RESP, na.rm = T)) %>% 
  gather(var, val, conf) %>% 
  unite(var, var, Stimulus_ACC) %>% 
  spread(var, val) %>% 
  group_by(uid) %>% 
  summarise(discrim = conf_TRUE - conf_FALSE)

cor <- cor.test(odd$discrim, even$discrim)

# Adjust with the Spearman-Brown prophecy formula = (2*r) / (1+r)  
discrim <- tibble(var = "discrimination",
               rel = round((2*cor$estimate) / (1+cor$estimate),2))

# task switching ----------------------------------------------------------
# uneven number of repeat and switch trials for each participant so selected 
# a complete subset to calculate reliability: the largest number of trials 
# completed by all participants in each trial type

type <- unique(surveys_s1$Task_Switching$type)

# Accuracy
switch_errors <- map(type, function(i) {
  x <- surveys_s1$Task_Switching %>%
    filter(uid %in% all_id$uid) %>% 
    filter(type == i) %>%
    select(uid, Stimulus_ACC) %>%
    group_by(uid) %>%
    mutate(itemnum = row_number())
  
  if (i == "repeat") {
    x <- x %>% 
      filter(itemnum %in% c(1:26)) %>%
      select(uid, itemnum, Stimulus_ACC) %>%
      mutate(Stimulus_ACC = ifelse(Stimulus_ACC == TRUE, 1,
                                   ifelse(Stimulus_ACC == FALSE, 0, Stimulus_ACC))) %>%
      spread(itemnum, Stimulus_ACC) %>%
      ungroup() %>% 
      select(-uid)
  } 
  
  if (i == "switch") {
    x <- x %>% 
      filter(itemnum %in% c(1:24)) %>% 
      select(uid, itemnum, Stimulus_ACC) %>%
      mutate(Stimulus_ACC = ifelse(Stimulus_ACC == TRUE, 1,
                                   ifelse(Stimulus_ACC == FALSE, 0, Stimulus_ACC))) %>%
      spread(itemnum, Stimulus_ACC) %>%
      ungroup() %>% 
      select(-uid)
  }
  
  tibble(var = paste0(i, "_errors"),
                 rel = round(psych::alpha(x)$total$raw_alpha,2))

})

switch_errors <- bind_rows(switch_errors)

# time
switch_time <- map(type, function(i) {
  x <- surveys_s1$Task_Switching %>%
    filter(uid %in% all_id$uid) %>% 
    filter(type == i) %>%
    select(uid, Stimulus_RT) %>%
    group_by(uid) %>%
    mutate(itemnum = row_number())
  
  if (i == "repeat") {
    x <- x %>% 
      filter(itemnum %in% c(1:26)) %>%
      select(uid, itemnum, Stimulus_RT) %>%
      mutate(Stimulus_RT = ifelse(Stimulus_RT == TRUE, 1,
                                  ifelse(Stimulus_RT == FALSE, 0, Stimulus_RT))) %>%
      spread(itemnum, Stimulus_RT) %>%
      ungroup() %>% 
      select(-uid)
  } else {
    x <- x %>% 
      filter(itemnum %in% c(1:24)) %>% 
      select(uid, itemnum, Stimulus_RT) %>%
      mutate(Stimulus_RT = ifelse(Stimulus_RT == TRUE, 1,
                                  ifelse(Stimulus_RT == FALSE, 0, Stimulus_RT))) %>%
      spread(itemnum, Stimulus_RT) %>%
      ungroup() %>% 
      select(-uid)
  }
  
  tibble(var = paste0(i, "_time"),
         rel = round(psych::alpha(x)$total$raw_alpha,2))
})

switch_time <- bind_rows(switch_time)


# switch cost
# prepare data
x <- surveys_s1$Task_Switching %>%
  filter(uid %in% all_id$uid) %>% 
  select(uid, type, Stimulus_RT) %>%
  group_by(uid, type) %>%
  mutate(itemnum = row_number()) %>% 
  filter(itemnum %in% c(1:24))

# select odd and even items
odds <- seq(1,24,2)
evens <- seq(2,24,2)

# calculate reliability
odd <- x %>%
  filter(itemnum %in% odds) %>% 
  group_by(uid) %>% 
  summarise(cost1 = mean(Stimulus_RT[type=="switch"] - mean(Stimulus_RT[type=="repeat"])))

even <- x %>%
  filter(itemnum %in% evens) %>% 
  group_by(uid) %>% 
  summarise(cost2 = mean(Stimulus_RT[type=="switch"] - mean(Stimulus_RT[type=="repeat"])))

cor <- cor.test(odd$cost1, even$cost2)

# Adjust with the Spearman-Brown prophecy formula = (2*r) / (1+r) 
switch_cost <- tibble(var = "switch_cost",
       rel = round((2*cor$estimate) / (1+cor$estimate),2))


# running letters ---------------------------------------------------------
# Accuracy
x <- surveys_s1$Running_Letters %>%
  filter(uid %in% all_id$uid) %>% 
  select(uid, RUNNINGtest_TrialNr, Stimulus_ACC) %>%
  group_by(uid) %>%
  mutate(Stimulus_ACC = ifelse(Stimulus_ACC == TRUE, 1,
                               ifelse(Stimulus_ACC == FALSE, 0, Stimulus_ACC))) %>% 
  spread(RUNNINGtest_TrialNr, Stimulus_ACC) %>%
  ungroup() %>% 
  select(-uid)

wm_acc <- tibble(var = "wm_accuracy",
       rel = round(psych::alpha(x)$total$raw_alpha,2))


# flanker task ------------------------------------------------------------
# 80 congruent trials (1 person with 79)
# 20 incongruent trials
type <- unique(surveys_s1$Flanker$Congruency)

# Accuracy
flank_errors <- map(type, function(i) {
  x <- surveys_s1$Flanker %>%
    filter(uid %in% all_id$uid) %>% 
    filter(Congruency == i) %>%
    select(uid, FLANKERtest_TrialNr, Stimulus_ACC) %>%
    group_by(uid) %>% 
    mutate(row = 1:n(),
           Stimulus_ACC = ifelse(Stimulus_ACC == TRUE, 1,
                                 ifelse(Stimulus_ACC == FALSE, 0, Stimulus_ACC)),
           Stimulus_ACC = as.numeric(Stimulus_ACC)) %>% 
    select(-FLANKERtest_TrialNr) %>% 
    spread(row, Stimulus_ACC) %>%
    ungroup() %>% 
    select(-uid)
  
  tibble(var = paste0(i, "_errors"),
         rel = round(psych::alpha(x)$total$raw_alpha,2))
})

flank_errors <- bind_rows(flank_errors)

# time
flank_time <- map(type, function(i) {
  x <- surveys_s1$Flanker %>%
    filter(uid %in% all_id$uid) %>% 
    filter(Congruency == i) %>%
    select(uid, FLANKERtest_TrialNr, Stimulus_RT) %>%
    group_by(uid) %>% 
    mutate(row = 1:n()) %>% 
    select(-FLANKERtest_TrialNr) %>% 
    spread(row, Stimulus_RT) %>%
    ungroup() %>% 
    select(-uid)
  
  tibble(var = paste0(i, "_time"),
         rel = round(psych::alpha(x)$total$raw_alpha,2))
})

flank_time <- bind_rows(flank_time)


# switch cost
# prepare data
x <- surveys_s1$Flanker %>%
  filter(uid %in% all_id$uid) %>% 
  select(uid, Congruency, Stimulus_RT) %>%
  group_by(uid, Congruency) %>%
  mutate(itemnum = row_number())

# select odd and even items
odd_con <- seq(1,80,2)
even_con <- seq(2,80,2)

odd_in <- seq(1,20,2)
even_in <- seq(2,20,2)

# calculate reliability
con_o <- x %>% 
  filter(itemnum %in% odd_con) %>% 
  group_by(uid) %>% 
  summarise(con1 = mean(Stimulus_RT))

in_o <- x %>% 
  filter(itemnum %in% odd_in) %>% 
  group_by(uid) %>% 
  summarise(in1 = mean(Stimulus_RT))

con_e <- x %>% 
  filter(itemnum %in% even_con) %>% 
  group_by(uid) %>% 
  summarise(con2 = mean(Stimulus_RT))

in_e <- x %>% 
  filter(itemnum %in% even_in) %>% 
  group_by(uid) %>% 
  summarise(in2 = mean(Stimulus_RT))


datalist <- list(con_o, in_o, con_e, in_e)

x <- datalist %>% reduce(left_join, by = "uid") %>% 
  group_by(uid) %>% 
  summarise(cost1 = con2 - con1,
            cost2 = in2 - in1)

cor <- cor.test(x$cost1, x$cost2)

# Adjust with the Spearman-Brown prophecy formula = (2*r) / (1+r)  
flank_cost <- tibble(var = "inhibitory_cost",
       rel = round((2*cor$estimate) / (1+cor$estimate),2))



reliability_team <- bind_rows(res, ipip, fluid, conf, bias, discrim, switch_errors, switch_time, switch_cost, wm_acc, flank_errors, flank_time, flank_cost) %>% 
  rename(a_team = rel)
  # write_csv("output/reliability_team_overall.csv")


