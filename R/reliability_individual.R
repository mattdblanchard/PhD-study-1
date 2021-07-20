# Reliability individuals
surveys_s2 <- readRDS("data/s2_item_level_survey_data.rds")

surveys_s2 <- surveys_s2 %>% 
  map(mutate, uid = str_remove(uid, "_p")) %>% 
  map(mutate, uid = ifelse(uid == "18051410_1_g1", "18051410_1-g1", 
                    ifelse(uid == "18051410_1_g2", "18051410_1-g2", 
                    ifelse(uid == "18051410_2_d2", "18051410_2-d2",
                    ifelse(uid == "18082410_2_d2", "18082410_2-d2",
                    ifelse(uid == "18081015_2-g1", "18081014_2-g1",
                    ifelse(uid == "18090315_1-g2_p", "18090314_1-g2_p", 
                    ifelse(uid == "18090314_1-g2", "18090314_1-g2_p", 
                    ifelse(uid == "18090411_2-d2", "18090410_2-d2", uid)))))))))

surveys_s2 <- surveys_s2 %>% 
  map(mutate,
  uid = ifelse(uid == "18052510_1-g1", "18052510_1-g2",
        ifelse(uid == "18052510_2-d1", "18052510_3-d2",
        ifelse(uid == "18052512_1-g1", "18052512_3-g2",
        ifelse(uid == "18052810_1-g1", "18052810_3-g2",
        ifelse(uid == "18052910_1-g1", "18052910_3-g2",
        ifelse(uid == "18052915_1-g1", "18052915_3-g2",
        ifelse(uid == "18053110_2-d1", "18053110_4-d2",
        ifelse(uid == "18060110_2-d1", "18060110_4-d2",
        ifelse(uid == "18060112_1-g1", "18060112_3-g2",
        ifelse(uid == "18060112_2-d1", "18060112_2-d2",
        ifelse(uid == "18060412_1-g1", "18060412_3-g2",
        ifelse(uid == "18060510_1-g1", "18060510_3-g2",
        ifelse(uid == "18060510_2-d1", "18060510_4-d2",
        ifelse(uid == "18060810_1-g1", "18060810_3-g2",
        ifelse(uid == "18061310_2-d1", "18061310_4-d2",
        ifelse(uid == "18080714_1-g1", "18080714_3-g2",
        ifelse(uid == "18080714_2-d1", "18080714_4-d2",
        ifelse(uid == "18080814_1-g1", "18080814_3-g2",
        ifelse(uid == "18080814_2-d1", "18080814_4-d2", uid))))))))))))))))))))
  
  
surveys_s2 <- surveys_s2 %>% 
  map(mutate,
      uid = ifelse(uid == "18081012_2-d1", "18081012_4-d2",
            ifelse(uid == "18081012_2-g1", "18081012_2-g2",
            ifelse(uid == "18081014_2-d1", "18081014_4-d2",
            ifelse(uid == "18081014_2-g1", "18081014_2-g2",
            ifelse(uid == "18081611_2-d1", "18081611_4-d2",
            ifelse(uid == "18081611_2-g1", "18081611_2-g2",
            ifelse(uid == "18082311_2-d1", "18082311_4-d2",
            ifelse(uid == "18082311_2-g1", "18082311_2-g2",
            ifelse(uid == "18082412_1-g1", "18082412_1-g2",
            ifelse(uid == "18083013_1-g2", "18083013_2-g2",
            ifelse(uid == "18083013_2-g1", "18083013_3-g2",
            ifelse(uid == "18083111_1-d1", "18083111_4-d2",
            ifelse(uid == "18090310_1-g1", "18090310_3-g2",
            ifelse(uid == "18090312_1-g1", "18090312_3-g2",
            ifelse(uid == "18090314_1-g1", "18090314_3-g2", uid))))))))))))))))
  
  surveys_s2 <- surveys_s2 %>% 
    map(mutate,
        uid = ifelse(uid == "18090412_1-g1", "18090412_1-g2",
              ifelse(uid == "18090414_1-g1", "18090414_3-g2",
              ifelse(uid == "18090512_2-g1", "18090512_2-g2",
              ifelse(uid == "18090610_1-g2", "18090610_2-g2",
              ifelse(uid == "18090610_2-g1", "18090610_3-g2",
              ifelse(uid == "18090712_2-g1", "18090712_2-g2",
              ifelse(uid == "18090714_1-g1", "18090714_3-g2",
              ifelse(uid == "18091010_1-g1", "18091010_3-g2",
              ifelse(uid == "18091012_1-g1", "18091012_3-g2",
              ifelse(uid == "18091014_2-g1", "18091014_2-g2",
              ifelse(uid == "18091110_1-g1", "18091110_1-g2",
              ifelse(uid == "18091212_1-g2", "18091212_2-g2",
              ifelse(uid == "18091212_2-g1", "18091212_3-g2",
              ifelse(uid == "18091310_2-g1", "18091310_2-g2",
              ifelse(uid == "18091310_1-g2", "18091310_3-g2", uid))))))))))))))))
  
  surveys_s2 <- surveys_s2 %>% 
    map(mutate,
        uid = ifelse(uid == "18090510_1-g2", "18090510_3-g2",
              ifelse(uid == "18091310_2-d2", "18091310_1-d2",
              ifelse(uid == "18090410_1-g1", "18090410_3-g2",
              ifelse(uid == "18051410_1-g1", "18051410_3-g2",
              ifelse(uid == "18051510_1-g1", "18051510_2-g2",
              ifelse(uid == "18052210_1-g1", "18052210_3-g2",
              ifelse(uid == "18052210_2-d1", "18052210_4-d2",
              ifelse(uid == "18052313_1-g1", "18052313_3-g2",
              ifelse(uid == "18061310_1-g1", "18061310_3-g2",
              ifelse(uid == "18081012_1-d2", "18081012_2-d2", uid)))))))))))
  
  
# full individual dataset (N=137) or subest (N=81)?
  if (dataset == "subset") {
    surveys_s2 <- surveys_s2 %>% map(filter, uid %in% ind_subset$uid)
  }
  
  
  ind1 <- read_csv("data/200603_s1_master_features_data.csv") %>% 
    filter(is.na(intellect_drone)) %>% 
    filter(events_missed_overall < 7) %>%
    select(uid)
  
  # study 2 individual drivers
  ind2 <- read_csv("data/190918_master_data_s2.csv") %>% 
    filter(is.na(issue_note) | !str_detect(issue_note, "driver did not complete")) %>% 
    filter(events_missed_overall < 7) %>% 
    select(uid)

  # Connor_Davidson_Resilience
x <- bind_rows(surveys_s1$Connor_Davidson_Resilience %>% 
                 filter(uid %in% ind1$uid) %>% 
                 select(uid, item_num, response),
               surveys_s2$resilience %>% 
                 filter(uid %in% ind2$uid) %>% 
                 select(uid, itemnum, Stimulus.RESP) %>% 
                 rename(item_num = itemnum, response = Stimulus.RESP)) %>% 
  spread(item_num, response) %>% 
  select(-uid)
  
res <- tibble(var = "resilience",
              rel = round(psych::alpha(x)$total$raw_alpha,2))
  
  
# mini-IPIP
x <- bind_rows(
    surveys_s2$ipip %>% 
      filter(uid %in% ind2$uid) %>% 
      select(uid, itemnum, factor, Stimulus.RESP, key) %>% 
      rename(item_num = itemnum, facet = factor, response = Stimulus.RESP),
    surveys_s1$Mini_IPIP %>% 
      filter(uid %in% ind1$uid) %>% 
      select(uid, item_num, facet, response, key))

fac <- unique(x$facet)

ipip <- map(fac, function(i) {
  x <- x %>% 
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
x <- bind_rows(
  surveys_s2$rapm %>% 
    filter(uid %in% ind2$uid) %>% 
    select(uid, ApmItemNum, Stimulus.ACC, Confidence.RESP) %>% 
    mutate(ApmItemNum = as.numeric(ApmItemNum)) %>% 
    rename(Stimulus_ACC = Stimulus.ACC, Confidence_RESP = Confidence.RESP),
  surveys_s1$Ravens_Advanced_Progressive_Matrices %>% 
    filter(uid %in% ind1$uid) %>% 
    select(uid, ApmItemNum, Stimulus_ACC, Confidence_RESP))

tmp <- x %>% 
  select(-Confidence_RESP) %>% 
  mutate(Stimulus_ACC = as.numeric(Stimulus_ACC)) %>% 
  spread(ApmItemNum, Stimulus_ACC) %>% 
  select(-uid)

fluid <- tibble(var = "gf_accuracy",
                rel = round(psych::alpha(tmp)$total$raw_alpha,2))

# confidence
tmp <- x %>% 
  select(-Stimulus_ACC) %>% 
  spread(ApmItemNum, Confidence_RESP) %>% 
  select(-uid)

conf <- tibble(var = "confidence",
               rel = round(psych::alpha(x)$total$raw_alpha,2))

# bias
# calculate bias for the odd and even items separately
# correlate the two sets then ajust with the Spearman-Brown prophecy formula
items <- x %>% select(ApmItemNum) %>% unique()

odd <- items[c(seq(1, 20, 2)),]
even <- items[c(seq(2, 20, 2)),]

odd <- x %>% 
  filter(ApmItemNum %in% odd$ApmItemNum) %>% 
  group_by(uid) %>% 
  summarise(acc = 100 * mean(Stimulus_ACC),
            conf = mean(Confidence_RESP),
            bias = conf - acc)

even <- x %>% 
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
items <- x %>% select(ApmItemNum) %>% unique()

odd <- items[c(seq(1, 20, 2)),]
even <- items[c(seq(2, 20, 2)),]

odd <- x %>% 
  filter(ApmItemNum %in% odd$ApmItemNum) %>% 
  group_by(uid, Stimulus_ACC) %>% 
  summarise(conf = mean(Confidence_RESP, na.rm = T)) %>% 
  gather(var, val, conf) %>% 
  unite(var, var, Stimulus_ACC) %>% 
  spread(var, val) %>% 
  group_by(uid) %>% 
  summarise(discrim = conf_TRUE - conf_FALSE)

even <- x %>% 
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

x <- bind_rows(
  surveys_s2$switch %>% 
    filter(uid %in% ind2$uid) %>% 
    select(uid, trial_type, Stimulus.ACC, Stimulus.RT) %>% 
    rename(Stimulus_ACC = Stimulus.ACC, type = trial_type,
           Stimulus_RT = Stimulus.RT),
  surveys_s1$Task_Switching %>% 
    filter(uid %in% ind1$uid) %>% 
    select(uid, type, Stimulus_ACC, Stimulus_RT)) %>% 
  filter(!is.na(type))

type <- unique(x$type)

# Accuracy
switch_errors <- map(type, function(i) {
  x <- x %>% 
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
  x <- x %>% 
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
x <- x %>% 
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
trial_num <- surveys_s1$Running_Letters %>%
  select(RUNNINGtest_TrialNr, Stimulus_CRESP) %>% 
  unique() %>% 
  rename(Stimulus.CRESP = Stimulus_CRESP)

x <- bind_rows(
  surveys_s2$run %>% 
    filter(uid %in% ind2$uid & !is.na(Stimulus.CRESP)) %>% 
    left_join(trial_num, by = "Stimulus.CRESP") %>% 
    select(uid, RUNNINGtest_TrialNr, Stimulus.ACC) %>% 
    rename(Stimulus_ACC = Stimulus.ACC),
  surveys_s1$Running_Letters %>% 
    filter(uid %in% ind1$uid) %>% 
    select(uid, RUNNINGtest_TrialNr, Stimulus_ACC))

x <- x %>% 
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
x <- bind_rows(
  surveys_s2$flanker %>% 
    filter(uid %in% ind2$uid) %>% 
    group_by(uid, Congruency) %>%
    arrange(Stimulus.OnsetTime) %>%
    mutate(FLANKERtest_TrialNr = 1:n()) %>%
    select(uid, Congruency, FLANKERtest_TrialNr, Stimulus.ACC, Stimulus.RT) %>% 
    rename(Stimulus_ACC = Stimulus.ACC, Stimulus_RT = Stimulus.RT),
  surveys_s1$Flanker %>% 
    filter(uid %in% ind1$uid) %>% 
    group_by(uid, Congruency) %>%
    arrange(FLANKERtest_TrialNr) %>%
    mutate(FLANKERtest_TrialNr = 1:n()) %>%
    select(uid, Congruency, FLANKERtest_TrialNr, Stimulus_ACC, Stimulus_RT)) %>% 
  ungroup()

ids <- x %>% 
  group_by(uid, Congruency) %>% 
  mutate(n = n()) %>% 
  filter(n == 80 & Congruency == "congruent") %>% 
  ungroup() %>% 
  select(uid) %>% unique()

type <- unique(x$Congruency)

# Accuracy
flank_errors <- map(type, function(i) {
  x <- x %>% 
    filter(Congruency == i) %>%
    select(uid, FLANKERtest_TrialNr, Stimulus_ACC) %>%
    group_by(uid) %>%
    mutate(Stimulus_ACC = ifelse(Stimulus_ACC == TRUE, 1,
                                 ifelse(Stimulus_ACC == FALSE, 0, Stimulus_ACC)),
           Stimulus_ACC = as.numeric(Stimulus_ACC)) %>%
    spread(FLANKERtest_TrialNr, Stimulus_ACC) %>%
    ungroup() %>% 
    select(-uid)
  
  tibble(var = paste0(i, "_errors"),
         rel = round(psych::alpha(x)$total$raw_alpha,2))
})

flank_errors <- bind_rows(flank_errors)

# time
flank_time <- map(type, function(i) {
  x <- x %>% 
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
x <- x %>%
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



reliability_ind <- bind_rows(res, ipip, fluid, conf, bias, discrim, switch_errors, switch_time, switch_cost, wm_acc, flank_errors, flank_time, flank_cost) %>% 
  rename(a_ind = rel)
# write_csv("output/reliability_team_overall.csv")


