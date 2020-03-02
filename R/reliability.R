# read survey data
surveys <- readRDS(here("data/survey_items.rds")) %>% 
  map(filter, uid %in% vars$uid)

# Connor_Davidson_Resilience
x <- surveys$Connor_Davidson_Resilience %>% 
  select(uid, item_num, response) %>% 
  spread(item_num, response) %>% 
  select(-uid)

print(paste0("resilience = ", round(psych::alpha(x)$total$raw_alpha,2)))

# mini-IPIP
fac <- unique(surveys$Mini_IPIP$facet)

map(fac, function(i) {
  x <- surveys$Mini_IPIP %>% 
    mutate(response = ifelse(key == "-1", 6-response, response)) %>% 
    filter(facet == i) %>% 
    select(uid, item_num, response) %>%
    spread(item_num, response) %>%
    select(-uid)
  
  print(paste0(i, " = ", round(psych::alpha(x)$total$raw_alpha,2)))
})

# RAPM
# accuracy
x <- surveys$Ravens_Advanced_Progressive_Matrices %>% 
  select(uid, ApmItemNum, Stimulus_ACC) %>% 
  mutate(Stimulus_ACC = as.numeric(Stimulus_ACC)) %>% 
  spread(ApmItemNum, Stimulus_ACC) %>% 
  select(-uid)

print(paste0("RAPM accuracy = ", round(psych::alpha(x)$total$raw_alpha,2)))

# confidence
x <- surveys$Ravens_Advanced_Progressive_Matrices %>% 
  select(uid, ApmItemNum, Confidence_RESP) %>% 
  spread(ApmItemNum, Confidence_RESP) %>% 
  select(-uid)

print(paste0("RAPM confidence = ", round(psych::alpha(x)$total$raw_alpha,2)))

# bias
# calculate bias for the odd and even items separately
# correlate the two sets then ajust with the Spearman-Brown prophecy formula
items <- surveys$Ravens_Advanced_Progressive_Matrices %>% select(ApmItemNum) %>% unique()

odd <- items[c(seq(1, 20, 2)),]
even <- items[c(seq(2, 20, 2)),]

odd <- surveys$Ravens_Advanced_Progressive_Matrices %>% 
  filter(ApmItemNum %in% odd$ApmItemNum) %>% 
  group_by(uid) %>% 
  summarise(acc = 100 * mean(Stimulus_ACC),
            conf = mean(Confidence_RESP),
            bias = conf - acc)

even <- surveys$Ravens_Advanced_Progressive_Matrices %>% 
  filter(ApmItemNum %in% even$ApmItemNum) %>% 
  group_by(uid) %>% 
  summarise(acc = 100 * mean(Stimulus_ACC),
            conf = mean(Confidence_RESP),
            bias = conf - acc)

cor <- cor.test(odd$bias, even$bias)

# Adjust with the Spearman-Brown prophecy formula = (2*r) / (1+r)  
print(paste0("RAPM bias = ", round((2*cor$estimate) / (1+cor$estimate),2)))


# discrimination
# calculate for the odd and even items separately
# correlate the two sets then ajust with the Spearman-Brown prophecy formula
items <- surveys$Ravens_Advanced_Progressive_Matrices %>% select(ApmItemNum) %>% unique()

odd <- items[c(seq(1, 20, 2)),]
even <- items[c(seq(2, 20, 2)),]

odd <- surveys$Ravens_Advanced_Progressive_Matrices %>% 
  filter(ApmItemNum %in% odd$ApmItemNum) %>% 
  group_by(uid, Stimulus_ACC) %>% 
  summarise(conf = mean(Confidence_RESP, na.rm = T)) %>% 
  gather(var, val, conf) %>% 
  unite(var, var, Stimulus_ACC) %>% 
  spread(var, val) %>% 
  group_by(uid) %>% 
  summarise(discrim = conf_TRUE - conf_FALSE)

even <- surveys$Ravens_Advanced_Progressive_Matrices %>% 
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
print(paste0("RAPM discrimination = ", round((2*cor$estimate) / (1+cor$estimate),2)))


# function to calculate reliability for the RT cost variable
spear_adjust <- function(data, type1, type2) {
  odd <- data %>%
    filter(itemnum %in% odds) %>% 
    group_by(uid) %>% 
    summarise(cost1 = mean(Stimulus_RT[type==type1] - mean(Stimulus_RT[type==type2])))
  
  even <- data %>%
    filter(itemnum %in% evens) %>% 
    group_by(uid) %>% 
    summarise(cost2 = mean(Stimulus_RT[type==type1] - mean(Stimulus_RT[type==type2])))
  
  x <- odd %>% 
    left_join(even, by = "uid") %>% 
    ungroup(uid) %>%
    select(-uid)
  
  cor <- cor.test(x$cost1, x$cost2)
  
  # Adjust with the Spearman-Brown prophecy formula = (2*r) / (1+r)  
  (2*cor$estimate) / (1+cor$estimate)
}


# task switching ----------------------------------------------------------
# uneven number of repeat and switch trials for each participant so selected 
# a complete subset to calculate reliability: the largest number of trials 
# completed by all participants in each trial type

type <- unique(surveys$Task_Switching$type)

# Accuracy
map(type, function(i) {
  x <- surveys$Task_Switching %>%
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
  
  print(paste0(i, " errors = ", round(psych::alpha(x)$total$raw_alpha,2)))
})


# time
map(type, function(i) {
  x <- surveys$Task_Switching %>%
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
  
  print(paste0(i, " time = ", round(psych::alpha(x)$total$raw_alpha,2)))
})


# inhibitory cost
# prepare data
x <- surveys$Task_Switching %>%
  select(uid, type, Stimulus_RT) %>%
  group_by(uid, type) %>%
  mutate(itemnum = row_number()) %>% 
  filter(itemnum %in% c(1:24))

# select odd and even items
odds <- seq(1,24,2)
evens <- seq(2,24,2)

# calculate reliability
spear_adjust(x, "switch", "repeat")
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
print(paste0("inhibitory cost = ", round((2*cor$estimate) / (1+cor$estimate),2)))


# running letters ---------------------------------------------------------
# Accuracy
x <- surveys$Running_Letters %>%
  select(uid, RUNNINGtest_TrialNr, Stimulus_ACC) %>%
  group_by(uid) %>%
  mutate(Stimulus_ACC = ifelse(Stimulus_ACC == TRUE, 1,
                               ifelse(Stimulus_ACC == FALSE, 0, Stimulus_ACC))) %>% 
  spread(RUNNINGtest_TrialNr, Stimulus_ACC) %>%
  ungroup() %>% 
  select(-uid)

print(paste0("working memory accuracy = ", round(psych::alpha(x)$total$raw_alpha,2)))



# flanker task ------------------------------------------------------------
# 80 congruent trials (1 person with 79)
# 20 incongruent trials
type <- unique(surveys$Flanker$Congruency)

# Accuracy
map(type, function(i) {
  x <- surveys$Flanker %>%
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
  
  print(paste0(i, " errors = ", round(psych::alpha(x)$total$raw_alpha,2)))
})

# time
map(type, function(i) {
  x <- surveys$Flanker %>%
    filter(Congruency == i) %>%
    select(uid, FLANKERtest_TrialNr, Stimulus_RT) %>%
    group_by(uid) %>% 
    mutate(row = 1:n()) %>% 
    select(-FLANKERtest_TrialNr) %>% 
    spread(row, Stimulus_RT) %>%
    ungroup() %>% 
    select(-uid)
  
  print(paste0(i, " time = ", round(psych::alpha(x)$total$raw_alpha,2)))
})


# switch cost
# prepare data
x <- surveys$Flanker %>%
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
print(paste0("switch cost = ", round((2*cor$estimate) / (1+cor$estimate),2)))