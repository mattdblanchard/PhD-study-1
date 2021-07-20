packages <- c("tidyverse", "psych", "corrr", "broom", "ez", "cowplot")
# car # load package to diagnose multicollinearity
# mctest
# library(olsrr) # print semi-part correlations

lapply(packages, library, character.only = TRUE)

# function for correlation matrices with sig. stars
source("R/pca_functions.R")

# # read data and remove teams with issues
# d <- read_csv("data/210513_comms_efa_vars.csv") %>% 
#   group_by(team) %>% 
#   mutate(role = "team",
#          ef_time_factor_team = mean(c(ef_time_factor_driver, ef_time_factor_drone)),
#          competence_factor_team = mean(c(competence_factor_driver, competence_factor_drone)),
#          agreeableness_team = mean(c(agreeableness, agreeableness_drone)),
#          neuroticism_team = mean(c(neuroticism, neuroticism_drone)))

# to examine teams back in dataset that were removed from N=54 because of no audio
# d3 <- read_csv("data/200309_comms_efa_vars.csv") %>% 
#   filter(!team %in% c("17080712_1", "17080810_1"))
# 
#  d %>% filter(!uid %in% d3$uid) %>% 
#    filter(str_detect(issue_note, "arrows")) %>% 
#    select(uid, issue_note)

# there are 4 collisions outliers which are candidates for removal. Need to examine more closely
# GGally::ggpairs(d %>% filter(collisions_overall < 550) %>%  select(collisions_overall, speed_overall))
# GGally::ggpairs(d %>% filter(!uid %in% c("17032109_1-g2", "17032915_2-d2")) %>%  select(collisions_overall, speed_overall))
# 
# d %>% mutate(z = (collisions_overall - mean(collisions_overall))/sd(collisions_overall)) %>% 
#   filter(collisions_overall > 500) %>% select(uid, collisions_overall, speed_overall, distance_overall, z)

# either remove collisions from other events from collisions_overall
# OR
# add collisions from other events to collisions_none_overall

# read data -------------------------------------------

if (file.exists("data/210712_final_teams_data.csv")) {
  
  d_all <- read_csv("data/210712_final_teams_data.csv")
  
} else {
  
# load individual data and remove incomplete attempts
# study 1 individual drivers
ind1 <- read_csv("data/200603_s1_master_features_data.csv") %>% 
  filter(is.na(intellect_drone)) %>% 
  filter(events_missed_overall < 7) %>%
  group_by(uid) %>% 
  mutate(role = "individual") %>% 
  select(uid, role, age, gender,
         congruent_errors:wm_accuracy, gf_accuracy:neuroticism,
         collisions_overall, speed_overall, collisions_none_overall, 
         speed_none_overall, distance_none_overall, collisions_1:collisions_5, speed_1:speed_5,
         collisions_fog_1:collisions_fog_5, speed_fog_1:speed_fog_5,
         collisions_none_1:collisions_none_5, 
         collisions_animal_1:collisions_animal_5,collisions_block_1:collisions_block_5,collisions_ice_1:collisions_ice_5,collisions_fall_1:collisions_fall_5,
         speed_none_1:speed_none_5, distance_1:distance_5, distance_fog_1:distance_fog_5, distance_none_1:distance_none_5,
         distance_animal_1:distance_animal_5,distance_block_1:distance_block_5,distance_ice_1:distance_ice_5,
         distance_fall_1:distance_fall_5, time_taken_overall, time_taken_none_overall, timetaken_fog_overall,issue_note) %>% 
  rename(time_taken_fog_overall = timetaken_fog_overall)

# study 2 individual drivers
ind2 <- read_csv("data/190918_master_data_s2.csv") %>% 
  filter(is.na(issue_note) | !str_detect(issue_note, "driver did not complete")) %>% 
  filter(events_missed_overall < 7) %>% 
  mutate(role = "individual") %>% 
  select(uid, role, age, gender, congruent_errors:wm_accuracy, rapm.acc:rapm_discrimination,
         agreeableness:neuroticism, collisions_overall, speed_overall,
         collisions_none_overall, speed_none_overall, 
         distance_none_overall, collisions_1:collisions_5, speed_1:speed_5,
         collisions_fog_1:collisions_fog_5, speed_fog_1:speed_fog_5,
         collisions_none_1:collisions_none_5, 
         collisions_animal_1:collisions_animal_5,collisions_block_1:collisions_block_5,collisions_ice_1:collisions_ice_5,collisions_fall_1:collisions_fall_5,
         speed_none_1:speed_none_5, distance_1:distance_5, distance_fog_1:distance_fog_5, distance_none_1:distance_none_5,
         distance_animal_1:distance_animal_5,distance_block_1:distance_block_5,distance_ice_1:distance_ice_5,
         distance_fall_1:distance_fall_5, time_taken_overall, time_taken_none_overall, time_taken_fog_1:time_taken_fog_5,
         issue_note) %>% 
  group_by(uid) %>% 
  mutate(time_taken_fog_overall = sum(c(time_taken_fog_1,time_taken_fog_2,time_taken_fog_3,time_taken_fog_4,time_taken_fog_5))) %>% 
  rename(gf_accuracy = rapm.acc, confidence = rapm.conf,
         discrimination = rapm_discrimination, bias = rapm_bias)

# combine s1 and s2 individual data and
# calculate missing sim vars
ind <- bind_rows(ind1, ind2) %>% 
  group_by(uid) %>% 
  mutate(speed_fog_overall = mean(c(speed_fog_1,speed_fog_2,speed_fog_3,speed_fog_4,speed_fog_5), na.rm=T),
         collisions_fog_overall = sum(c(collisions_fog_1,collisions_fog_2,collisions_fog_3,collisions_fog_4,collisions_fog_5), na.rm=T),
         distance_fog_overall = sum(c(distance_fog_1,distance_fog_2,distance_fog_3,distance_fog_4,distance_fog_5), na.rm=T)) %>% 
  ungroup()

# load team data
team <- read_csv("data/210513_comms_efa_vars.csv") %>%
  # read_csv("data/210513_comms_vars.csv") %>% 
  group_by(team) %>% 
  mutate(role = "team",
         ef_time_factor_team = mean(c(ef_time_factor_driver, ef_time_factor_drone),na.rm=T),
         competence_factor_team = mean(c(competence_factor_driver, competence_factor_drone),na.rm=T),
         agreeableness_team = mean(c(agreeableness, agreeableness_drone),na.rm=T),
         neuroticism_team = mean(c(neuroticism, neuroticism_drone),na.rm=T),
         talk_turn_overall_drone = sum(c(co_info_help_overall,co_info_harm_overall,co_instruct_help_overall,co_instruct_harm_overall,co_redundant_overall,co_question_overall),na.rm=T),
         talk_turn_overall_driver = sum(c(drive_question_overall,drive_informs_overall),na.rm=T),
         talk_turn_overall_team = sum(c(co_info_help_overall,co_info_harm_overall,co_instruct_help_overall,co_instruct_harm_overall,co_redundant_overall,co_question_overall,drive_question_overall,drive_informs_overall),na.rm=T),
         talk_turn_none_drone = sum(c(co_info_help_no_fog,co_info_harm_no_fog,co_instruct_help_no_fog,co_instruct_harm_no_fog,co_redundant_no_fog,co_question_no_fog),na.rm=T),
         talk_turn_none_driver = sum(c(drive_question_no_fog,drive_informs_no_fog),na.rm=T),
         talk_turn_none_team = sum(c(co_info_help_no_fog,co_info_harm_no_fog,co_instruct_help_no_fog,co_instruct_harm_no_fog,co_redundant_no_fog,co_question_no_fog,drive_question_no_fog,drive_informs_no_fog),na.rm=T),
         talk_turn_fog_drone = sum(c(co_info_help_fog,co_info_harm_fog,co_instruct_help_fog,co_instruct_harm_fog,co_redundant_fog,co_question_fog),na.rm=T),
         talk_turn_fog_driver = sum(c(drive_question_fog,drive_informs_fog),na.rm=T),
         talk_turn_fog_team = sum(c(co_info_help_fog,co_info_harm_fog,co_instruct_help_fog,co_instruct_harm_fog,co_redundant_fog,co_question_fog,drive_question_fog,drive_informs_fog),na.rm=T),
         talk_turn_overall_ratio = talk_turn_overall_drone/talk_turn_overall_driver,
         talk_turn_overall_ratio = ifelse(talk_turn_overall_ratio == Inf, NA, talk_turn_overall_ratio),
         talk_time_fog_driver = sum(c(time_talking_fog_1,time_talking_fog_2,time_talking_fog_3,time_talking_fog_4,time_talking_fog_5),na.rm=T),
         talk_time_fog_drone = sum(c(time_talking_fog_1_drone,time_talking_fog_2_drone,time_talking_fog_3_drone,time_talking_fog_4_drone,time_talking_fog_5_drone),na.rm=T),
         talk_time_fog_team = talk_time_fog_driver + talk_time_fog_drone,
         talk_time_none_driver = sum(c(time_talking_none_1,time_talking_none_2,time_talking_none_3,time_talking_none_4,time_talking_none_5),na.rm=T),
         talk_time_none_drone = sum(c(time_talking_none_1_drone,time_talking_none_2_drone,time_talking_none_3_drone,time_talking_none_4_drone,time_talking_none_5_drone),na.rm=T),
         talk_time_none_team = talk_time_none_driver + talk_time_none_drone,
         talk_time_overall_driver = sum(c(time_talking_1,time_talking_2,time_talking_3,time_talking_4,time_talking_5),na.rm=T),
         talk_time_overall_drone = sum(c(time_talking_1_drone,time_talking_2_drone,time_talking_3_drone,time_talking_4_drone,time_talking_5_drone),na.rm=T),
         talk_time_overall_team = talk_time_overall_driver + talk_time_overall_drone,
         talk_time_overall_ratio = talk_time_overall_drone/talk_time_overall_driver,
         talk_time_overall_ratio = ifelse(talk_time_overall_ratio == Inf, NA, talk_time_overall_ratio)) %>% 
  ungroup() %>% 
  select(uid, role, age_driver, age_co_driver, prop_female,
         congruent_errors:wm_accuracy, gf_accuracy:discrimination, agreeableness:neuroticism,
         congruent_errors_drone:wm_accuracy_drone, gf_accuracy_drone:discrimination_drone,
         agreeableness_drone:neuroticism_drone, 
         competence_factor_driver,competence_factor_drone,ef_time_factor_driver,ef_time_factor_drone,
         ef_time_factor_team, competence_factor_team, agreeableness_team, neuroticism_team,
         collisions_overall, speed_overall, collisions_none_overall, 
         speed_none_overall, collisions_fog_overall, speed_fog_overall,
         distance_none_overall, distance_fog_overall, collisions_1:collisions_5, speed_1:speed_5,
         collisions_fog_1:collisions_fog_5, speed_fog_1:speed_fog_5,
         collisions_none_1:collisions_none_5, 
         collisions_animal_1:collisions_animal_5,collisions_block_1:collisions_block_5,collisions_ice_1:collisions_ice_5,collisions_fall_1:collisions_fall_5,
         speed_none_1:speed_none_5, distance_1:distance_5,
         distance_fog_1:distance_fog_5, distance_none_1:distance_none_5,
         distance_animal_1:distance_animal_5,distance_block_1:distance_block_5,distance_ice_1:distance_ice_5,
         distance_fall_1:distance_fall_5, terrible_codriver_r_two, helpful_exchange_r_two, 
         talk_turn_overall_drone:talk_time_overall_ratio,
         time_taken_overall, time_taken_none_overall, timetaken_fog_overall, issue_note) %>% 
  rename(time_taken_fog_overall = timetaken_fog_overall)

# create dataset of ind and teams
d_all <- bind_rows(team, ind) %>% 
  group_by(uid) %>% 
  mutate(
    collisions_overall = collisions_none_overall + collisions_fog_overall,
    # collisions_nonenew_1 = sum(c(collisions_none_1,collisions_animal_1,collisions_block_1,collisions_ice_1,collisions_fall_1), na.rm=T),
    # collisions_nonenew_2 = sum(c(collisions_none_2,collisions_animal_2,collisions_block_2,collisions_ice_2,collisions_fall_2), na.rm=T),
    # collisions_nonenew_3 = sum(c(collisions_none_3,collisions_animal_3,collisions_block_3,collisions_ice_3,collisions_fall_3), na.rm=T),
    # collisions_nonenew_4 = sum(c(collisions_none_4,collisions_animal_4,collisions_block_4,collisions_ice_4,collisions_fall_4), na.rm=T),
    # collisions_nonenew_5 = sum(c(collisions_none_5,collisions_animal_5,collisions_block_5,collisions_ice_5,collisions_fall_5), na.rm=T),
    # collisions_nonenew_overall = sum(c(collisions_nonenew_1,collisions_nonenew_2,collisions_nonenew_3,collisions_nonenew_4,collisions_nonenew_5)),
    # distance_nonenew_1 = sum(c(distance_none_1,distance_animal_1,distance_block_1,distance_ice_1,distance_fall_1), na.rm=T),
    # distance_nonenew_2 = sum(c(distance_none_2,distance_animal_2,distance_block_2,distance_ice_2,distance_fall_2), na.rm=T),
    # distance_nonenew_3 = sum(c(distance_none_3,distance_animal_3,distance_block_3,distance_ice_3,distance_fall_3), na.rm=T),
    # distance_nonenew_4 = sum(c(distance_none_4,distance_animal_4,distance_block_4,distance_ice_4,distance_fall_4), na.rm=T),
    # distance_nonenew_5 = sum(c(distance_none_5,distance_animal_5,distance_block_5,distance_ice_5,distance_fall_5), na.rm=T),
    # distance_nonenew_overall = sum(c(distance_nonenew_1,distance_nonenew_2,distance_nonenew_3,distance_nonenew_4,distance_nonenew_5), na.rm = T),
    collisions_fog_overall = sum(c(collisions_fog_1,collisions_fog_2,collisions_fog_3,collisions_fog_4,collisions_fog_5), na.rm = T),
    distance_fog_overall = sum(c(distance_fog_1,distance_fog_2,distance_fog_3,distance_fog_4,distance_fog_5), na.rm = T),
    collisions_none_overall = sum(c(collisions_none_1,collisions_none_2,collisions_none_3,collisions_none_4,collisions_none_5), na.rm = T),
    distance_none_overall = sum(c(distance_none_1,distance_none_2,distance_none_3,distance_none_4,distance_none_5), na.rm = T),
    distance_overall = sum(c(distance_1,distance_2,distance_3,distance_4,distance_5), na.rm = T),
    # collisionsratio_overall = collisions_overall/distance_overall,
    collisionsratio_fog_overall = collisions_fog_overall/distance_fog_overall,
    collisionsratio_none_overall = collisions_none_overall/distance_none_overall,
    speed_fog_overall = mean(c(speed_fog_1,speed_fog_2,speed_fog_3,speed_fog_4,speed_fog_5), na.rm = T),
    speed_1 = ifelse(is.na(speed_fog_1), speed_none_1, (speed_none_1*(distance_none_1/(distance_none_1+distance_fog_1))) + (speed_fog_1*(distance_fog_1/(distance_none_1+distance_fog_1)))), # adjust speed overall variables so they don't include the other events (e.g., black ice, animal)
    speed_2 = ifelse(is.na(speed_fog_2), speed_none_2, (speed_none_2*(distance_none_2/(distance_none_2+distance_fog_2))) + (speed_fog_2*(distance_fog_2/(distance_none_2+distance_fog_2)))), # adjust speed overall variables so they don't include the other events (e.g., black ice, animal)
    speed_3 = ifelse(is.na(speed_fog_3), speed_none_3, (speed_none_3*(distance_none_3/(distance_none_3+distance_fog_3))) + (speed_fog_3*(distance_fog_3/(distance_none_3+distance_fog_3)))), # adjust speed overall variables so they don't include the other events (e.g., black ice, animal)
    speed_4 = ifelse(is.na(speed_fog_4), speed_none_4, (speed_none_4*(distance_none_4/(distance_none_4+distance_fog_4))) + (speed_fog_4*(distance_fog_4/(distance_none_4+distance_fog_4)))), # adjust speed overall variables so they don't include the other events (e.g., black ice, animal)
    speed_5 = ifelse(is.na(speed_fog_5), speed_none_5, (speed_none_5*(distance_none_5/(distance_none_5+distance_fog_5))) + (speed_fog_5*(distance_fog_5/(distance_none_5+distance_fog_5)))), # adjust speed overall variables so they don't include the other events (e.g., black ice, animal)
    speed_overall = mean(c(speed_1,speed_2,speed_3,speed_4,speed_5)),
    ) %>% 
  ungroup()

# outlier analysis
outliers <- d_all %>% select(uid, role, collisions_none_overall, collisions_fog_overall,
                             speed_none_overall, speed_fog_overall)

outliers$mahal <- mahalanobis(outliers[-1:-2], colMeans(outliers[-1:-2]), cov(outliers[-1:-2]))

outliers$p.value <- pchisq(outliers$mahal, df=5, lower.tail=FALSE)

outliers <- outliers %>% filter(p.value < .001)

# remove outliers
d_all <- d_all %>% filter(!uid %in% outliers$uid)

# conduct PCA to extract comms factors during normal and fog periods
# load original comms vars
comm <- read_csv("data/210513_comms_efa_vars.csv") %>%
  filter(uid %in% d_all$uid[!is.na(d_all$terrible_codriver_r_two)])

# normal periods
pca <- comm %>% 
  select(co_question_no_fog, drive_question_no_fog, drive_informs_no_fog, co_info_help_no_fog, co_instruct_help_no_fog,
         co_instruct_harm_no_fog, co_info_harm_no_fog, drive_frust_no_fog, co_redundant_no_fog)

# 2-component PCA
n_comp <- 2
rotate_method <- "promax"
score_method <- "Bartlett"

fit <- principal(pca, rotate = rotate_method, nfactors = n_comp,
                 method = score_method, scores = TRUE)

# save component scores as dataframe
pca_scores <- data.frame(fit$scores) %>% 
  rename(helpful_exchange_none_r_two = RC1, terrible_codriver_none_r_two = RC2)

# add component scores to d
d_all <- d_all %>% 
  left_join(bind_cols(comm %>% select(uid), pca_scores), by = "uid")

# fog periods
pca <- comm %>% 
  select(co_question_fog, drive_question_fog, drive_informs_fog, co_info_help_fog, co_instruct_help_fog,
         co_instruct_harm_fog, co_info_harm_fog, drive_frust_fog, co_redundant_fog)

fit <- principal(pca, rotate = rotate_method, nfactors = n_comp,
                 method = score_method, scores = TRUE)

# save component scores as dataframe
pca_scores <- data.frame(fit$scores) %>% 
  rename(helpful_exchange_fog_r_two = RC1, terrible_codriver_fog_r_two = RC2)

# add component scores to d
d_all <- d_all %>% 
  left_join(bind_cols(comm %>% select(uid), pca_scores), by = "uid")

d_all %>% write_csv("data/210712_final_teams_data.csv")

}

# method ------------------------------------------------------------------

# participant info for method
# number of participants
sum(d_all$role=="individual") + (sum(d_all$role=="team")*2)

# demographics
d_all %>% 
  select(role, matches("age")) %>% 
  pivot_longer(-role, names_to = "var", values_to = "val") %>% 
  group_by(role) %>%
  summarise(age_mean = mean(val, na.rm = T),
            age_sd = sd(val, na.rm = T))

# num females in total
sum(d_all$prop_female*2, na.rm = T) + (sum(d_all$role=="individual")-sum(d_all$gender, na.rm = T))
# prop females in total
(sum(d_all$prop_female*2, na.rm = T) + (sum(d_all$role=="individual")-sum(d_all$gender, na.rm = T)))/((nrow(team)*2) + sum(d_all$role=="individual"))

# num females in teams
sum(d_all$prop_female*2, na.rm = T)
# prop females in teams
sum(d_all$prop_female*2, na.rm = T)/(sum(d_all$role=="team")*2)

# num females in individuals
(sum(d_all$role=="individual")-sum(d_all$gender, na.rm = T))
# prop females in individuals
(sum(d_all$role=="individual")-sum(d_all$gender, na.rm = T))/sum(d_all$role=="individual")


# results -----------------------------------------------------------------
# d_all %>% 
#   filter(!uid %in% outliers$uid) %>% 
#   select(uid, role, collisions_none_overall, collisions_fog_overall,
#          speed_none_overall, speed_fog_overall) %>% 
#   pivot_longer(-uid:-role, "var", "val") %>% 
#   mutate(e = str_extract(var, "none|fog"),
#          metric = str_extract(var, "collisions|speed")) %>% 
#   select(-var) %>% 
#   pivot_wider(names_from = metric, values_from = value) %>% 
#   ggplot(aes(x = collisions, y = speed)) +
#   geom_point() +
#   facet_wrap(role~e, scales = "free")


# figure 2 plot
plot <- d_all %>% 
  select(role, collisions_overall, collisions_none_overall, collisions_fog_overall, speed_overall, speed_none_overall, speed_fog_overall) %>% 
  pivot_longer(-role, names_to = "var", values_to = "val") %>% 
  mutate(cond = str_extract(var, "none|fog"),
         cond = ifelse(is.na(cond), "overall", cond),
         cond = factor(cond, levels = c("overall", "none", "fog"), labels = c("Overall", "Normal", "Fog")),
         var = str_remove(var, "_overall|_none_overall|_fog_overall"),
         var = factor(var, levels = c("collisions", "speed"), 
             labels = c("Collisions", "Speed")),
         role = factor(role, levels = c("individual", "team"), labels = c("Individuals", "Teams")))
        
         

dist_plot <- function(data) {
  data %>% 
    ggplot() +
    geom_histogram(aes(x=val), color="black", fill="white", bins = 20) +
    labs(y = "Frequency") +
    theme_classic() +
    theme(strip.background = element_blank(),
          strip.text = element_text(size=12,colour="black"),
          axis.text=element_text(size=12),
          axis.title=element_text(size=12),
          plot.title = element_text(hjust = 0.5, face = "bold", size = 12))
}


a <- plot %>% 
  filter(var == "Collisions", role == "Individuals", cond == "Overall") %>% 
  dist_plot() +
  scale_x_continuous(breaks = c(0,200,400,600), limits = c(0, 600)) +
  labs(x = "Collisions", y = "Individuals")

b <- plot %>% 
  filter(var == "Speed", role == "Individuals", cond == "Overall") %>% 
  dist_plot() +
  scale_x_continuous(breaks = seq(4,14,2), limits = c(4,15)) +
  labs(x = "Speed", y = "")

c <- plot %>% 
  filter(var == "Collisions", role == "Teams", cond == "Overall") %>% 
  dist_plot() +
  scale_x_continuous(breaks = c(0,200,400,600), limits = c(0, 600)) +
  labs(x = "Collisions", y = "Teams")

d <- plot %>% 
  filter(var == "Speed", role == "Teams", cond == "Overall") %>% 
  dist_plot() +
  scale_x_continuous(breaks = seq(4,14,2), limits = c(4,15)) +
  labs(x = "Speed", y = "")

cowplot::plot_grid(a,b,c,d, labels = c("A", "B", "C", "D"))

ggsave("output/figure2.png", width = 8, height = 7)

# plot for normal periods for supplemental materials
a <- plot %>% 
  filter(var == "Collisions", role == "Individuals", cond == "Normal") %>% 
  dist_plot() +
  scale_x_continuous(breaks = c(0,200,400), limits = c(0, 400)) +
  labs(x = "Collisions", y = "Individuals")

b <- plot %>% 
  filter(var == "Speed", role == "Individuals", cond == "Normal") %>% 
  dist_plot() +
  scale_x_continuous(breaks = seq(4,16,2), limits = c(4,16)) +
  labs(x = "Speed", y = "Individuals")

c <- plot %>% 
  filter(var == "Collisions", role == "Teams", cond == "Normal") %>% 
  dist_plot() +
  scale_x_continuous(breaks = c(0,200,400), limits = c(0, 400)) +
  labs(x = "Collisions", y = "Teams")

d <- plot %>% 
  filter(var == "Speed", role == "Teams", cond == "Normal") %>% 
  dist_plot() +
  scale_x_continuous(breaks = seq(4,16,2), limits = c(4,16)) +
  labs(x = "Speed", y = "")

plot_grid(a,b,c,d, labels = c("A", "B", "C", "D"))

ggsave("output/supp_a1_figure.png", width = 8, height = 7)

# plot for fog periods for supplemental materials
a <- plot %>% 
  filter(var == "Collisions", role == "Individuals", cond == "Fog") %>% 
  dist_plot() +
  scale_x_continuous(breaks = c(0,200), limits = c(0, 250)) +
  labs(x = "Collisions", y = "Individuals")

b <- plot %>% 
  filter(var == "Speed", role == "Individuals", cond == "Fog") %>% 
  dist_plot() +
  scale_x_continuous(breaks = seq(4,14,2), limits = c(4,15)) +
  labs(x = "Speed", y = "")

c <- plot %>% 
  filter(var == "Collisions", role == "Teams", cond == "Fog") %>% 
  dist_plot() +
  scale_x_continuous(breaks = c(0,200), limits = c(0, 250)) +
  labs(x = "Collisions", y = "Teams")

d <- plot %>% 
  filter(var == "Speed", role == "Teams", cond == "Fog") %>% 
  dist_plot() +
  scale_x_continuous(breaks = seq(4,14,2), limits = c(4,15)) +
  labs(x = "Speed", y = "")

plot_grid(a,b,c,d, labels = c("A", "B", "C", "D"))

ggsave("output/supp_a2_figure.png", width = 8, height = 7)


# descriptives for sim metrics
desc <- d_all %>% 
  select(role, collisions_overall, speed_overall, collisions_fog_overall, speed_fog_overall, collisions_none_overall, speed_none_overall) %>% 
  pivot_longer(-role, names_to = "var", values_to = "val") %>% 
  group_by(role, var) %>% 
  summarise(mean = round(mean(val,na.rm=T),2), 
            sd = round(sd(val,na.rm=T),2)) %>% 
  mutate(var = str_remove(var, "_overall"))

# reliability estimates for sim metrics
nested <- d_all %>%
  ungroup() %>% 
  select(uid, role, collisions_1:collisions_5, speed_1:speed_5,
         collisions_fog_1:collisions_fog_5, speed_fog_1:speed_fog_5,
         collisions_none_1:collisions_none_5, speed_none_1:speed_none_5) %>% 
  pivot_longer(-uid:-role, names_to = "var", values_to = "val") %>% 
  mutate(lap = str_extract(var, "[1-5]"),
         lap = as.numeric(lap, "_"),
         var = str_remove(var, "_[1-5]")) %>% 
  pivot_wider(names_from = lap, values_from = val) %>% 
  select(-uid) %>% 
  nest(data = `1`:`5`)

alpha <- nested %>% 
  mutate(ic = map(data, ~ alpha(.)$total$raw_alpha)) %>% 
  unnest(ic) %>% 
  select(-data)

# compare individuals and teams on sim metrics
# t-tests
nested <- d_all %>% 
  select(uid, role, collisions_overall, speed_overall,
         collisions_none_overall, speed_none_overall, collisions_fog_overall, speed_fog_overall) %>% 
  pivot_longer(-uid:-role, names_to = "var", values_to = "val") %>% 
  pivot_wider(names_from = role, values_from = val) %>% 
  nest(data = c(uid, team, individual))

test <- nested %>% 
  mutate(fit = map(data, ~ t.test(.x$individual, .x$team, paired = FALSE, var.equal = TRUE)),
         tidy_fit = map(fit, broom::tidy)) %>% 
  unnest(tidy_fit) %>% 
  select(-data, -fit, -method, -alternative) %>% 
  rename(t.value = statistic, df = parameter, mean.diff = estimate) %>% 
  select(var, df, t.value, p.value) %>% 
  mutate(var = str_remove(var, "_overall"))

# combine desc, alpha, and t-test into table for paper
desc %>% left_join(alpha, by = c("role", "var")) %>% 
  left_join(test, by = "var") %>% 
  pivot_longer(mean:p.value, names_to = "param", values_to = "val") %>% 
  mutate(val = round(val, 4)) %>% 
  unite(param, param, role) %>% 
  pivot_wider(names_from = param, values_from = val) %>% 
  mutate(e = str_extract(var, "fog|none"),
         var = str_remove(var, "_fog|_none"),
         e = ifelse(is.na(e), "overall", e),
         e = factor(e, levels = c("overall", "none", "fog")),
         var = factor(var, levels = c("collisions", "speed")),
         t.value_team = ifelse(p.value_team < .001, paste0(round(t.value_team,2), "***"), 
                        ifelse(p.value_team < .01 & p.value_team > .001, paste0(round(t.value_team,2), "**"),
                        ifelse(p.value_team < .05 & p.value_team > .01, paste0(round(t.value_team,2), "*"), round(t.value_team,2))))) %>% 
  arrange(var, e) %>%
  select(var, e, ic_individual, mean_individual, sd_individual, ic_team, mean_team, sd_team, df_team, t.value_team, p.value_team) %>% 
  rename(df = df_team, t_value = t.value_team, p_value = p.value_team) %>% 
  write_csv("output/desc_sim_metrics.csv")



# psych vars --------------------------------------------------------------
# reliability
# prep survey data
surveys_s1 <- readRDS("data/survey_items.rds")

id <- d_all %>% 
  filter(role == "team") %>% 
  mutate(team = str_remove(uid, "-.*")) %>% 
  select(team)

all_id <- surveys_s1$Demographics %>% 
  select(uid) %>% 
  mutate(team = str_remove(uid, "-.*")) %>% 
  filter(team %in% id$team) %>% 
  select(-team)

id_ind <- d_all %>% 
  filter(role == "individual")

# select random subset of 80 individuals
set.seed(77)
ind_subset <- d_all %>% 
  # filter(role == "individual") %>% 
  filter(uid %in% sample(id_ind$uid, nrow(id))) %>% 
  select(uid)

# full individual dataset (N=137) or subset (N=81)?
# dataset <- "subset"
dataset <- "full"

# overall teams
source("R/reliability_overall.R")

# driver
source("R/reliability_driver.R")

# codriver
source("R/reliability_codriver.R")

# individuals
source("R/reliability_individual.R")


# descriptives for psych vars
descriptives <- function(data) {
  data %>% 
    group_by(var) %>% 
    summarise(mean = round(mean(val, na.rm = T),2),
              sd = round(sd(val, na.rm = T),2))
  
}

# drivers and navigators separately
team_roles <- d_all %>% 
  select(uid, role, congruent_errors:neuroticism, congruent_errors_drone:neuroticism_drone) %>% 
  filter(role == "team") %>% 
  pivot_longer(-uid:-role, names_to = "var", values_to = "val") %>% 
  descriptives() %>% 
  mutate(level = ifelse(str_detect(var, "drone"), "navigator", "driver"),
         var = str_remove(var, "_drone")) %>% 
  pivot_longer(mean:sd, "param", "val") %>% 
  unite(param, level, param) %>% 
  pivot_wider(names_from = param, values_from = value)

# teams overall
team_overall <- d_all %>% 
  filter(role == "team") %>% 
  select(uid, congruent_errors:neuroticism, congruent_errors_drone:neuroticism_drone) %>% 
  pivot_longer(-uid, names_to = "var", values_to = "val") %>% 
  mutate(var = str_remove(var, "_drone")) %>%
  descriptives() %>% 
  rename(team_mean = mean, team_sd = sd)

# individuals
if (dataset == "subset") {
  d_ind <- d_all %>% 
    filter(uid %in% ind_subset$uid)
} else if (dataset == "full") {
  d_ind <- d_all %>% 
    filter(role == "individual")
}

ind <- d_ind %>% 
  filter(role == "individual") %>% 
  select(uid, congruent_errors:neuroticism) %>% 
  pivot_longer(-uid, names_to = "var", values_to = "val") %>% 
  mutate(var = str_remove(var, "_drone")) %>%
  descriptives() %>% 
  rename(ind_mean = mean, ind_sd = sd)

# t-tests comparing individuals and teams on each psych var
team_test <- d_all %>% 
  filter(role == "team") %>%
  select(uid, role, congruent_errors:neuroticism, congruent_errors_drone:neuroticism_drone) %>% 
  pivot_longer(-uid:-role, names_to = "var", values_to = "val") %>% 
  mutate(var = str_remove(var, "_drone")) %>% 
  group_by(uid, var) %>% 
  summarise(m = mean(val, na.rm = T),
            role = "team") %>% 
  pivot_wider(names_from = var, values_from = m)
  
ind_test <- d_ind %>% 
  select(uid, role, congruent_errors:neuroticism)

test <- bind_rows(team_test, ind_test) %>% 
  ungroup()


nested <- test %>%
  pivot_longer(-uid:-role, names_to = "var", values_to = "val") %>% 
  pivot_wider(names_from = role, values_from = val) %>% 
  nest(data = -var)

test_ind <- nested %>% 
  mutate(fit = map(data, ~t.test(.x$individual, .x$team, paired = F, var.equal = T)),
         tidy_fit = map(fit, broom::tidy)) %>% 
  unnest(tidy_fit) %>% 
  select(-data, -fit, -method, -alternative) %>% 
  rename(t_value_ind = statistic, df = parameter, mean.diff = estimate) %>% 
  mutate(p.value = round(p.value, 4),
         t_value_ind = ifelse(p.value < .001, paste0(round(t_value_ind,2), "***"), 
                          ifelse(p.value < .01 & p.value > .001, paste0(round(t_value_ind,2), "**"),
                                 ifelse(p.value < .05 & p.value > .01, paste0(round(t_value_ind,2), "*"), round(t_value_ind,2))))) %>% 
  select(var, t_value_ind)


# t-tests comparing driver and codriver on each psych var
nested <- d_all %>% 
  filter(role == "team") %>% 
  select(uid, congruent_errors:neuroticism, congruent_errors_drone:neuroticism_drone) %>% 
  pivot_longer(names_to = "var", values_to = "val", -uid) %>% 
  mutate(role = ifelse(str_detect(var, "drone"), "drone", "driver"),
         var = str_remove(var, "_drone")) %>% 
  pivot_wider(names_from = role, values_from = val) %>% 
  nest(data = -var)

test_team <- nested %>% 
  mutate(fit = map(data, ~t.test(.x$driver, .x$drone, paired = T, var.equal = T)),
         tidy_fit = map(fit, broom::tidy)) %>% 
  unnest(tidy_fit) %>% 
  select(-data, -fit, -method, -alternative) %>% 
  rename(t_value_team = statistic, df = parameter, mean.diff = estimate) %>% 
  mutate(p.value = round(p.value, 4),
         t_value_team = ifelse(p.value < .001, paste0(round(t_value_team,2), "***"), 
                          ifelse(p.value < .01 & p.value > .001, paste0(round(t_value_team,2), "**"),
                                 ifelse(p.value < .05 & p.value > .01, paste0(round(t_value_team,2), "*"), round(t_value_team,2))))) %>% 
  select(var, t_value_team)

datalist <- list(reliability_ind, ind, reliability_team, team_overall, test_ind, reliability_driver, team_roles, reliability_codrive, test_team)

desc_psych <- datalist %>% reduce(left_join, by = "var") %>% 
  filter(var != "resilience") %>% 
  select(var, a_ind, ind_mean, ind_sd, a_team, team_mean, team_sd, t_value_ind, a_driver, driver_mean, driver_sd, a_codrive, navigator_mean, navigator_sd, t_value_team) %>% 
  mutate(var = factor(var, levels = c("repeat_time", "switch_time", "switch_cost", "repeat_errors", "switch_errors", "congruent_time", "incongruent_time",
                                      "inhibitory_cost", "congruent_errors", "incongruent_errors", "wm_accuracy", "gf_accuracy", "confidence",
                                      "bias", "discrimination", "agreeableness", "conscientiousness", "extraversion", "intellect", 'neuroticism'))) %>% 
  arrange(var)

if (dataset == "subset") {
  desc_psych %>% 
    write_csv("output/desc_psych_vars_subset.csv")
} else if (dataset == "full") {
  desc_psych %>% 
    write_csv("output/desc_psych_vars_full.csv")
  }

# comms vars --------------------------------------------------------------
# given there are 27 teams with out comms vars we need to check that there are no differences 
# on the sim metrics or pysch vars between full teams sample and teams subset
# sim metrics

# create teams dataset for analyses
reg_d <- d_all %>%
  filter(role == "team" & !is.na(terrible_codriver_r_two))

sub <- reg_d %>% 
  select(uid, collisions_overall, collisions_none_overall, collisions_fog_overall,
         speed_overall, speed_none_overall, speed_fog_overall) %>% 
  pivot_longer(-uid, names_to = "var", values_to = "val") %>% 
  mutate(sample = "subset") %>% 
  ungroup()

full <- d_all %>%
  filter(role == "team") %>% 
  select(uid, collisions_overall, collisions_none_overall, collisions_fog_overall,
         speed_overall, speed_none_overall, speed_fog_overall) %>% 
  pivot_longer(-uid, names_to = "var", values_to = "val") %>% 
  mutate(sample = "full") %>% 
  ungroup()

remove <- d_all %>%
  filter(role == "team" & !uid %in% sub$uid) %>% 
  select(uid, collisions_overall, collisions_none_overall, collisions_fog_overall,
         speed_overall, speed_none_overall, speed_fog_overall) %>% 
  pivot_longer(-uid, names_to = "var", values_to = "val") %>% 
  mutate(sample = "remove") %>% 
  ungroup()

nested <- bind_rows(full, sub) %>% 
  pivot_wider(names_from = sample, values_from = val) %>% 
  select(-uid) %>% 
  group_by(var) %>% 
  nest(data = c("full", "subset"))

nested %>% 
  mutate(test = map(data, ~ t.test(.x$full, .x$subset, var.equal = T, paired = F)),
         tidy_test = map(test, broom::tidy)) %>% 
  unnest(tidy_test) %>% 
  filter(p.value < .05)

bind_rows(full, sub) %>% 
  ggplot(aes(x=val, fill = sample)) +
  geom_histogram() +
  facet_wrap(~var, scales = "free")

# psych
sub <- reg_d %>% 
  select(uid, congruent_errors:neuroticism, congruent_errors_drone:neuroticism_drone) %>% 
  pivot_longer(-uid, names_to = "var", values_to = "val") %>% 
  mutate(var = str_remove(var, "_drone")) %>% 
  group_by(uid, var) %>% 
  summarise(val = mean(val)) %>% 
  mutate(sample = "subset") %>% 
  ungroup()

full <- d_all %>%
  filter(role == "team") %>% 
  select(uid, congruent_errors:neuroticism, congruent_errors_drone:neuroticism_drone) %>% 
  pivot_longer(-uid, names_to = "var", values_to = "val") %>% 
  mutate(var = str_remove(var, "_drone")) %>% 
  group_by(uid, var) %>% 
  summarise(val = mean(val)) %>% 
  mutate(sample = "full") %>% 
  ungroup()

remove <- d_all %>%
  filter(role == "team" & !uid %in% sub$uid) %>% 
  select(uid, congruent_errors:neuroticism, congruent_errors_drone:neuroticism_drone) %>% 
  pivot_longer(-uid, names_to = "var", values_to = "val") %>% 
  mutate(var = str_remove(var, "_drone")) %>% 
  group_by(uid, var) %>% 
  summarise(val = mean(val)) %>% 
  mutate(sample = "remove") %>% 
  ungroup()

nested <- bind_rows(full, sub) %>% 
  pivot_wider(names_from = sample, values_from = val) %>% 
  select(-uid) %>% 
  group_by(var) %>% 
  nest(data = c("full", "subset"))

nested %>% 
  mutate(test = map(data, ~ t.test(.x$full, .x$subset, var.equal = T, paired = F)),
         tidy_test = map(test, broom::tidy)) %>% 
  unnest(tidy_test) %>% 
  filter(p.value < .05)

# # duration and talking turn variables
desc <- reg_d %>% 
  select(talk_time_overall_team, talk_time_overall_driver, talk_time_overall_drone,
         talk_time_none_team, talk_time_none_driver, talk_time_none_drone,
         talk_time_fog_team, talk_time_fog_driver, talk_time_fog_drone,
         talk_turn_overall_team, talk_turn_overall_driver, talk_turn_overall_drone,
         talk_turn_none_team, talk_turn_none_driver, talk_turn_none_drone,
         talk_turn_fog_team, talk_turn_fog_driver, talk_turn_fog_drone) %>%
  pivot_longer(everything(), "var", "val") %>%
  group_by(var) %>%
  summarise(mean = round(mean(value, na.rm = T),2),
            sd = round(sd(value, na.rm = T),2)) %>%
  mutate(role = str_extract(var, "team|driver|drone"),
         var = str_remove(var, "_team|_driver|_drone")) %>%
  pivot_longer(mean:sd, names_to = "param", values_to = "val") %>%
  unite(param, param, role) %>%
  pivot_wider(names_from = param, values_from = val)


# simple comms vars per lap for reliability
comm <- read_csv("data/210513_comms_efa_vars.csv") %>% 
  filter(uid %in% reg_d$uid) %>% 
  group_by(uid) %>% 
  mutate(
    talk_turn_overall_driver_1 = sum(c(drive_question_1,drive_informs_1),na.rm=T),
    talk_turn_overall_driver_2 = sum(c(drive_question_2,drive_informs_2),na.rm=T),
    talk_turn_overall_driver_3 = sum(c(drive_question_3,drive_informs_3),na.rm=T),
    talk_turn_overall_driver_4 = sum(c(drive_question_4,drive_informs_4),na.rm=T),
    talk_turn_overall_driver_5 = sum(c(drive_question_5,drive_informs_5),na.rm=T),
    talk_turn_none_driver_1 = sum(c(drive_question_no_fog_1,drive_informs_no_fog_1),na.rm=T),
    talk_turn_none_driver_2 = sum(c(drive_question_no_fog_2,drive_informs_no_fog_2),na.rm=T),
    talk_turn_none_driver_3 = sum(c(drive_question_no_fog_3,drive_informs_no_fog_3),na.rm=T),
    talk_turn_none_driver_4 = sum(c(drive_question_no_fog_4,drive_informs_no_fog_4),na.rm=T),
    talk_turn_none_driver_5 = sum(c(drive_question_no_fog_5,drive_informs_no_fog_5),na.rm=T),
    talk_turn_fog_driver_1 = sum(c(drive_question_fog_1,drive_informs_fog_1),na.rm=T),
    talk_turn_fog_driver_2 = sum(c(drive_question_fog_2,drive_informs_fog_2),na.rm=T),
    talk_turn_fog_driver_3 = sum(c(drive_question_fog_3,drive_informs_fog_3),na.rm=T),
    talk_turn_fog_driver_4 = sum(c(drive_question_fog_4,drive_informs_fog_4),na.rm=T),
    talk_turn_fog_driver_5 = sum(c(drive_question_fog_5,drive_informs_fog_5),na.rm=T),
    talk_turn_overall_drone_1 = sum(c(co_info_help_1, co_info_harm_1, co_instruct_help_1, co_instruct_harm_1, co_redundant_1, co_question_1),na.rm=T),
    talk_turn_overall_drone_2 = sum(c(co_info_help_2, co_info_harm_2, co_instruct_help_2, co_instruct_harm_2, co_redundant_2, co_question_2),na.rm=T),
    talk_turn_overall_drone_3 = sum(c(co_info_help_3, co_info_harm_3, co_instruct_help_3, co_instruct_harm_3, co_redundant_3, co_question_3),na.rm=T),
    talk_turn_overall_drone_4 = sum(c(co_info_help_4, co_info_harm_4, co_instruct_help_4, co_instruct_harm_4, co_redundant_4, co_question_4),na.rm=T),
    talk_turn_overall_drone_5 = sum(c(co_info_help_5, co_info_harm_5, co_instruct_help_5, co_instruct_harm_5, co_redundant_5, co_question_5),na.rm=T),
    talk_turn_none_drone_1 = sum(c(co_info_help_no_fog_1, co_info_harm_no_fog_1, co_instruct_help_no_fog_1, co_instruct_harm_no_fog_1, co_redundant_no_fog_1, co_question_no_fog_1),na.rm=T),
    talk_turn_none_drone_2 = sum(c(co_info_help_no_fog_2, co_info_harm_no_fog_2, co_instruct_help_no_fog_2, co_instruct_harm_no_fog_2, co_redundant_no_fog_2, co_question_no_fog_2),na.rm=T),
    talk_turn_none_drone_3 = sum(c(co_info_help_no_fog_3, co_info_harm_no_fog_3, co_instruct_help_no_fog_3, co_instruct_harm_no_fog_3, co_redundant_no_fog_3, co_question_no_fog_3),na.rm=T),
    talk_turn_none_drone_4 = sum(c(co_info_help_no_fog_4, co_info_harm_no_fog_4, co_instruct_help_no_fog_4, co_instruct_harm_no_fog_4, co_redundant_no_fog_4, co_question_no_fog_4),na.rm=T),
    talk_turn_none_drone_5 = sum(c(co_info_help_no_fog_5, co_info_harm_no_fog_5, co_instruct_help_no_fog_5, co_instruct_harm_no_fog_5, co_redundant_no_fog_5, co_question_no_fog_5),na.rm=T),
    talk_turn_fog_drone_1 = sum(c(co_info_help_fog_1, co_info_harm_fog_1, co_instruct_help_fog_1, co_instruct_harm_fog_1, co_redundant_fog_1, co_question_fog_1),na.rm=T),
    talk_turn_fog_drone_2 = sum(c(co_info_help_fog_2, co_info_harm_fog_2, co_instruct_help_fog_2, co_instruct_harm_fog_2, co_redundant_fog_2, co_question_fog_2),na.rm=T),
    talk_turn_fog_drone_3 = sum(c(co_info_help_fog_3, co_info_harm_fog_3, co_instruct_help_fog_3, co_instruct_harm_fog_3, co_redundant_fog_3, co_question_fog_3),na.rm=T),
    talk_turn_fog_drone_4 = sum(c(co_info_help_fog_4, co_info_harm_fog_4, co_instruct_help_fog_4, co_instruct_harm_fog_4, co_redundant_fog_4, co_question_fog_4),na.rm=T),
    talk_turn_fog_drone_5 = sum(c(co_info_help_fog_5, co_info_harm_fog_5, co_instruct_help_fog_5, co_instruct_harm_fog_5, co_redundant_fog_5, co_question_fog_5),na.rm=T),
    talk_turn_overall_team_1 = sum(talk_turn_overall_drone_1, talk_turn_overall_driver_1,na.rm=T),
    talk_turn_overall_team_2 = sum(talk_turn_overall_drone_2, talk_turn_overall_driver_2,na.rm=T),
    talk_turn_overall_team_3 = sum(talk_turn_overall_drone_3, talk_turn_overall_driver_3,na.rm=T),
    talk_turn_overall_team_4 = sum(talk_turn_overall_drone_4, talk_turn_overall_driver_4,na.rm=T),
    talk_turn_overall_team_5 = sum(talk_turn_overall_drone_5, talk_turn_overall_driver_5,na.rm=T),
    talk_turn_none_team_1 = sum(talk_turn_none_drone_1, talk_turn_none_driver_1,na.rm=T),
    talk_turn_none_team_2 = sum(talk_turn_none_drone_2, talk_turn_none_driver_2,na.rm=T),
    talk_turn_none_team_3 = sum(talk_turn_none_drone_3, talk_turn_none_driver_3,na.rm=T),
    talk_turn_none_team_4 = sum(talk_turn_none_drone_4, talk_turn_none_driver_4,na.rm=T),
    talk_turn_none_team_5 = sum(talk_turn_none_drone_5, talk_turn_none_driver_5,na.rm=T),
    talk_turn_fog_team_1 = sum(talk_turn_fog_drone_1, talk_turn_fog_driver_1,na.rm=T),
    talk_turn_fog_team_2 = sum(talk_turn_fog_drone_2, talk_turn_fog_driver_2,na.rm=T),
    talk_turn_fog_team_3 = sum(talk_turn_fog_drone_3, talk_turn_fog_driver_3,na.rm=T),
    talk_turn_fog_team_4 = sum(talk_turn_fog_drone_4, talk_turn_fog_driver_4,na.rm=T),
    talk_turn_fog_team_5 = sum(talk_turn_fog_drone_5, talk_turn_fog_driver_5,na.rm=T),
    time_talking_overall_team_1 = sum(time_talking_1, time_talking_1_drone,na.rm=T),
    time_talking_overall_team_2 = sum(time_talking_2, time_talking_2_drone,na.rm=T),
    time_talking_overall_team_3 = sum(time_talking_3, time_talking_3_drone,na.rm=T),
    time_talking_overall_team_4 = sum(time_talking_4, time_talking_4_drone,na.rm=T),
    time_talking_overall_team_5 = sum(time_talking_5, time_talking_5_drone,na.rm=T),
    time_talking_none_team_1 = sum(time_talking_none_1, time_talking_none_1_drone,na.rm=T),
    time_talking_none_team_2 = sum(time_talking_none_2, time_talking_none_2_drone,na.rm=T),
    time_talking_none_team_3 = sum(time_talking_none_3, time_talking_none_3_drone,na.rm=T),
    time_talking_none_team_4 = sum(time_talking_none_4, time_talking_none_4_drone,na.rm=T),
    time_talking_none_team_5 = sum(time_talking_none_5, time_talking_none_5_drone,na.rm=T),
    time_talking_fog_team_1 = sum(time_talking_fog_1, time_talking_fog_1_drone,na.rm=T),
    time_talking_fog_team_2 = sum(time_talking_fog_2, time_talking_fog_2_drone,na.rm=T),
    time_talking_fog_team_3 = sum(time_talking_fog_3, time_talking_fog_3_drone,na.rm=T),
    time_talking_fog_team_4 = sum(time_talking_fog_4, time_talking_fog_4_drone,na.rm=T),
    time_talking_fog_team_5 = sum(time_talking_fog_5, time_talking_fog_5_drone,na.rm=T)) %>% 
  ungroup() 
  
nested <- comm %>% select(uid, time_talking_1:time_talking_5, time_talking_none_1:time_talking_none_5, time_talking_fog_1:time_talking_fog_5,
                          time_talking_1_drone:time_talking_5_drone, time_talking_none_1_drone:time_talking_none_5_drone, 
                          time_talking_fog_1_drone:time_talking_fog_5_drone, talk_turn_overall_driver_1:time_talking_fog_team_5) %>% 
  pivot_longer(-uid, "var", "val") %>% 
  mutate(lap = str_extract(var, "[1-5]"),
         var = str_remove(var, "_[1-5]")) %>% 
  pivot_wider(names_from = lap, values_from = value) %>% 
  select(-uid) %>% 
  group_by(var) %>% 
  nest(data = c(`1`:`5`))

alpha <- nested %>% 
  mutate(a = map(data, ~ alpha(.)$total$raw_alpha)) %>% 
  unnest(a) %>% 
  select(-data) %>% 
  mutate(a = round(a,2),
         var = str_replace(var, "time_talking", "talk_time"),
         var = ifelse(var == "talk_time", "talk_time_overall", var),
         var = ifelse(var == "talk_time_drone", "talk_time_overall_drone", var),
         role = paste0("a_", str_extract(var, "team|driver|drone")),
         role = ifelse(role == "a_NA", "a_driver", role),
         var = str_remove(var, "_team|_driver|_drone")) %>% 
  pivot_wider(names_from = role, values_from = a)


nested <- reg_d %>% 
  select(uid, talk_time_overall_driver, talk_time_overall_drone,
         talk_time_none_driver, talk_time_none_drone,
         talk_time_fog_driver, talk_time_fog_drone,
         talk_turn_overall_driver, talk_turn_overall_drone, 
         talk_turn_none_driver, talk_turn_none_drone,
         talk_turn_fog_driver, talk_turn_fog_drone) %>% 
  pivot_longer(-uid, "var", "val") %>% 
  mutate(role = str_extract(var, "team|driver|drone"),
         var = str_remove(var, "_team|_driver|_drone")) %>% 
  pivot_wider(names_from = role, values_from = value) %>% 
  select(-uid) %>% 
  nest(data = c(driver, drone))

test <- nested %>% 
  mutate(test = map(data, ~t.test(.x$driver, .x$drone, var.equal = T, paired = F)),
         tidy_test = map(test, tidy)) %>% 
  unnest(tidy_test) %>% 
  mutate(statistic = ifelse(p.value < .001, paste0(round(statistic,2), "***"), 
                            ifelse(p.value < .01 & p.value > .001, paste0(round(statistic,2), "**"),
                                   ifelse(p.value < .05 & p.value > .01, paste0(round(statistic,2), "*"), round(statistic,2)))),
         p.value = round(p.value, 4)) %>% 
  select(var, statistic, p.value)

datalist <- list(desc, alpha, test)

datalist %>% reduce(left_join, by = "var") %>% 
  mutate(var = factor(var, levels = c("talk_time_overall", "talk_time_none", "talk_time_fog",
                                      "talk_turn_overall", "talk_turn_none", "talk_turn_fog"))) %>% 
  arrange(var) %>% 
  select(var, a_team, matches("team"), a_driver, matches("driver"), a_drone, matches("drone"), statistic, p.value) %>% 
  write_csv("output/desc_simple_comms.csv")

d_all %>% 
  select(talk_time_overall_driver, talk_turn_overall_driver, talk_time_overall_drone, talk_turn_overall_drone) %>% 
  pivot_longer(everything(), "var", "val") %>% 
  separate(var, into = c("a", "var", "c", "role")) %>% 
  ggplot() +
  geom_histogram(aes(x=value), color="black", fill="white", bins = 20) +
  labs(y = "Frequency") +
  facet_wrap(role ~ var, scales = "free") +
  theme_classic() +
  theme(strip.background = element_blank(),
        strip.text = element_text(size=12,colour="black"),
        axis.text=element_text(size=12),
        axis.title=element_text(size=12),
        plot.title = element_text(hjust = 0.5, face = "bold", size = 12))



# coded comms behaviours
nested <- comm %>% select(team, co_info_harm_1:co_info_harm_5, co_info_help_1:co_info_help_5, 
                          co_instruct_harm_1:co_instruct_harm_5, co_instruct_help_1:co_instruct_help_5,
                          co_redundant_1:co_redundant_5, co_question_1:co_question_5,
                          drive_question_1:drive_question_5, drive_informs_1:drive_informs_5, drive_frust_1:drive_frust_5,
                          co_info_harm_fog_1:co_info_harm_fog_5, co_info_help_fog_1:co_info_help_fog_5,
                          co_instruct_harm_fog_1:co_instruct_harm_fog_5, co_instruct_help_fog_1:co_instruct_help_fog_5,
                          co_redundant_fog_1:co_redundant_fog_5, co_question_fog_1:co_question_fog_5,
                          drive_question_fog_1:drive_question_fog_5, drive_informs_fog_1:drive_informs_fog_5, drive_frust_fog_1:drive_frust_fog_5,
                          co_info_harm_no_fog_1:co_info_harm_no_fog_5, co_info_help_no_fog_1:co_info_help_no_fog_5,
                          co_instruct_harm_no_fog_1:co_instruct_harm_no_fog_5, co_instruct_help_no_fog_1:co_instruct_help_no_fog_5,
                          co_redundant_no_fog_1:co_redundant_no_fog_5, co_question_no_fog_1:co_question_no_fog_5,
                          drive_question_no_fog_1:drive_question_no_fog_5, drive_informs_no_fog_1:drive_informs_no_fog_5, drive_frust_no_fog_1:drive_frust_no_fog_5) %>% 
  pivot_longer(-team, "var", "val") %>% 
  mutate(lap = str_extract(var, "[1-5]"),
         var = str_remove(var, "_[1-5]"),
         e = str_extract(var, "no_fog|fog"),
         e = ifelse(is.na(e), "overall", e),
         var = str_remove(var, "_no_fog|_fog"),
         e = ifelse(e == "no_fog", "none", e)) %>% 
  pivot_wider(names_from = lap, values_from = value) %>% 
  select(-team) %>% 
  group_by(var) %>% 
  nest(data = c(`1`:`5`))

alpha <- nested %>% 
  mutate(a = map(data, ~ alpha(.)$total$raw_alpha)) %>% 
  unnest(a) %>% 
  select(-data) %>% 
  mutate(a = round(a,2),
         e = paste0("a_", e)) %>% 
  pivot_wider(names_from = e, values_from = a)


x <- comm %>% 
  select(uid, co_info_harm_overall, co_info_help_overall, 
         co_instruct_harm_overall, co_instruct_help_overall,
         co_redundant_overall, co_question_overall,
         drive_question_overall, drive_informs_overall, drive_frust_overall,
         co_info_harm_fog, co_info_help_fog,
         co_instruct_harm_fog, co_instruct_help_fog,
         co_redundant_fog, co_question_fog,
         drive_question_fog, drive_informs_fog, drive_frust_fog,
         co_info_harm_no_fog, co_info_help_no_fog,
         co_instruct_harm_no_fog, co_instruct_help_no_fog,
         co_redundant_no_fog, co_question_no_fog,
         drive_question_no_fog, drive_informs_no_fog, drive_frust_no_fog) %>% 
  pivot_longer(-uid, "var", "val") %>% 
  mutate(e = str_extract(var, "no_fog|fog"),
         e = ifelse(is.na(e), "overall", e),
         var = str_remove(var, "_overall|_no_fog|_fog"),
         e = ifelse(e == "no_fog", "none", e))

nested <- x %>% 
  pivot_wider(names_from = e, values_from = value) %>% 
  select(-uid, -overall) %>% 
  nest(data = c(none, fog))

test <- nested %>% 
  mutate(test = map(data, ~t.test(.x$none, .x$fog, var.equal = T, paired = F)),
         tidy_test = map(test, tidy)) %>% 
  unnest(tidy_test) %>% 
  mutate(statistic = ifelse(p.value < .001, paste0(round(statistic,2), "***"), 
                            ifelse(p.value < .01 & p.value > .001, paste0(round(statistic,2), "**"),
                                   ifelse(p.value < .05 & p.value > .01, paste0(round(statistic,2), "*"), round(statistic,2)))),
         p.value = round(p.value, 4)) %>% 
  select(var, statistic, p.value)

desc <- x %>% 
  group_by(var, e) %>% 
  summarise(mean = mean(value, na.rm = T),
            sd = sd(value, na.rm = T)) %>% 
  pivot_longer(mean:sd, names_to = "param", values_to = "val") %>% 
  unite(param, param, e) %>% 
  pivot_wider(names_from = param, values_from = val)

datalist <- list(desc, alpha, test)

datalist %>% reduce(left_join, by = "var") %>% 
  mutate(var = factor(var, levels = c("drive_informs", "drive_question", "drive_frust",
                                      "co_info_help", "co_info_harm", "co_instruct_help",
                                      "co_instruct_harm", "co_question", "co_redundant"))) %>% 
  arrange(var) %>% 
  select(var, a_overall, matches("overall"), a_none, matches("none"), a_fog, matches("fog"), statistic, p.value) %>% 
  write_csv("output/desc_comm_patterns.csv")


# ind vs teams on sim metrics ---------------------------------------------
# compare fog and normal periods on sim metrics
nested <- d_all %>% 
  select(uid, role, collisionsratio_none_overall, speed_none_overall, collisionsratio_fog_overall, speed_fog_overall) %>% 
  pivot_longer(-uid:-role, names_to = "var", values_to = "val") %>% 
  mutate(e = str_extract(var, "none|fog"),
         # e = ifelse(is.na(e), "overall", e),
         metric = str_extract(var, "speed|collisions")) %>% 
  select(-var) %>% 
  pivot_wider(names_from = e, values_from = val) %>% 
  nest(data = c(uid, none, fog))

nested %>% 
  mutate(fit = map(data, ~ t.test(.x$none, .x$fog, paired = TRUE, var.equal = TRUE)),
         tidy_fit = map(fit, broom::tidy)) %>% 
  unnest(tidy_fit) %>% 
  select(-data, -fit, -method, -alternative) %>% 
  rename(t.value = statistic, df = parameter, mean.diff = estimate) %>% 
  mutate(p.value = round(p.value, 4),
         t.value = ifelse(p.value < .001, paste0(round(t.value,2), "***"), 
                          ifelse(p.value < .01 & p.value > .001, paste0(round(t.value,2), "**"),
                                 ifelse(p.value < .05 & p.value > .01, paste0(round(t.value,2), "*"), round(t.value,2))))) %>% 
  select(role, metric, df, t.value, p.value)

# mean and sd for distance during normal periods
mean(d_all$distance_none_overall)
sd(d_all$distance_none_overall)

# mean and sd for distance during fog periods
mean(d_all$distance_fog_overall)
sd(d_all$distance_fog_overall)

# t.test on distance during normal vs fog periods
t.test(d_all$distance_none_overall, d_all$distance_fog_overall, paired = TRUE, var.equal = TRUE)

# correlations bw distance and collisions
cor.test(d_all$distance_none_overall, d_all$collisions_none_overall)
cor.test(d_all$distance_fog_overall, d_all$collisions_fog_overall)

# correlations bw distance and speed
cor.test(d_all$distance_none_overall, d_all$speed_none_overall)
cor.test(d_all$distance_fog_overall, d_all$speed_fog_overall)



aov <- d_all %>% 
  select(uid, role, collisionsratio_none_overall, collisionsratio_fog_overall,
         speed_none_overall, speed_fog_overall) %>% 
  pivot_longer(collisionsratio_none_overall:speed_fog_overall, "var", "val") %>% 
  mutate(role = factor(role, levels = c("individual", "team"), labels = c("Individuals", "Teams")),
         metric = str_extract(var, "collisions|speed"),
         metric = factor(metric, levels = c("collisions", "speed"), labels = c("Collisions", "Speed")),
         e = str_extract(var, "none|fog"),
         e = factor(e, levels = c("none", "fog"), labels = c("Normal", "Fog"))
         ) %>%
  select(-var)

# mixed design ANOVA
# collisions
x <- aov %>% 
  filter(metric == "Collisions")

ezANOVA(data = x, dv = value, wid = uid, within = e, between = role, type = 2)

nested <- aov %>% 
  filter(metric == "Collisions") %>% 
  pivot_wider(names_from = role, values_from = value) %>% 
  select(-uid, -metric) %>% 
  group_by(e) %>% 
  nest(data = c(Individuals, Teams))

pvalues <- nested %>% 
  mutate(test = map(data, ~ t.test(.x$Individuals, .x$Teams, var.equal = T)),
         tidy_test = map(test, tidy)) %>% 
  unnest(tidy_test)

p.adjust(pvalues$p.value, method = "bonferroni", n = 2)

aov %>% 
  group_by(metric, role) %>% 
  summarise(mean = mean(value),
            sd = sd(value)) %>% 
  group_by(metric) %>% 
  mutate(mean[1]-mean[2])


# speed
x <- aov %>% 
  filter(metric == "Speed")

ezANOVA(data = x, dv = value, wid = uid, within = e, between = role, type = 2)

# plot
# prep data
x <- aov %>% 
  group_by(metric, role, e) %>% 
  summarise(mean = mean(value, na.rm = T),
            se = 1.96 * sd(value, na.rm = T)/sqrt(n()))

pd <- position_dodge(width = 0.2)

# function for plot
anova_plot <- function(data) {
  data %>%
    ggplot(aes(x = role, y = mean, group = e)) +
    geom_line(aes(linetype = e), position = pd) +
    geom_errorbar(aes(ymin = mean - se, ymax = mean + se),
                  width = .1, position = pd) +
    geom_point(size = 4, position = pd) +
    geom_point(size = 3, colour = "white", position = pd) +
    # facet_wrap(~ metric, scales = "free") +
    guides(linetype = guide_legend("Period")) +
    labs(x = "Grouping",
         y = "Mean") +
    theme_classic() +
    theme(strip.background = element_blank(),
          strip.text = element_text(size=12,colour="black"),
          axis.text=element_text(size=12),
          axis.title=element_text(size=12))
}

# make plots
a <- x %>% 
  filter(metric == "Collisions") %>% 
  anova_plot() +
  labs(title = "Collisions") +
  theme(plot.title = element_text(hjust = 0.5), 
        legend.position = "none")

b <- x %>% 
  filter(metric == "Speed") %>% 
  anova_plot() +
  labs(title = "Speed",
       y = "") +
  theme(plot.title = element_text(hjust = 0.5))

plot_grid(a, b, labels = c("A", "B"),
                    rel_widths = c(.465,.535),
                    rel_heights = 1)

# save plot
ggsave("output/figure3.png", width = 10, height = 6)


# EFA for comms vars ------------------------------------------------------
# overall -----------------------------------------------------------------
source("R/pca_functions.R")

# select original comms variables
pca <- comm %>% 
  select(co_question_overall, drive_question_overall, drive_informs_overall, co_info_help_overall, co_instruct_help_overall,
         co_instruct_harm_overall, co_info_harm_overall, drive_frust_overall, co_redundant_overall)

# correlations bw original communication variables
pca %>% 
  corstarsl() %>% 
  rownames_to_column() %>% 
  write_csv("output/comms_overall_corrs.csv")

# Kaiser-Meyer-Olkin Measure of Sampling Adequacy (KMO)
KMO(cor(pca, use = "pairwise.complete.obs"))

# Bartlett's test of spherecity
print("Bartletts test of spherecity")
print(data.frame(cortest.bartlett(cor(pca, use = "pairwise.complete.obs"), n = 53)))

# scree plot
scree(pca, factors = TRUE)

# conduct parallel analysis to confirm no. components
paran::paran(drop_na(pca), iterations = 5000, all = T, graph = T)

# 2-component PCA
n_comp <- 2
rotate_method <- "promax" # rotation with kaiser normalization
score_method <- "Bartlett"

fit <- principal(pca, rotate = rotate_method, nfactors = n_comp,
                 method = score_method, scores = TRUE)

# variance explained
var_table()

# pattern matrix
pattern_matrix()

# save component scores as dataframe
pca_scores <- data.frame(fit$scores) %>% 
  rename(helpful_exchange_r_two = RC1, terrible_codriver_r_two = RC2)

# add component scores to d
 d_all <- d_all %>% 
   select(-helpful_exchange_r_two, -terrible_codriver_r_two) %>% 
   left_join(bind_cols(comm %>% select(uid), pca_scores), by = "uid")

 # correlation bw comms factors
 cor.test(d_all$helpful_exchange_r_two, d_all$terrible_codriver_r_two)  
 
 

# normal periods ----------------------------------------------------------
 pca <- comm %>% 
   select(co_question_no_fog, drive_question_no_fog, drive_informs_no_fog, co_info_help_no_fog, co_instruct_help_no_fog,
          co_instruct_harm_no_fog, co_info_harm_no_fog, drive_frust_no_fog, co_redundant_no_fog)
 
 # correlations bw original communication variables
 pca %>% 
   corstarsl() %>% 
   rownames_to_column() %>% 
   write_csv("output/comms_none_corrs.csv")
 
 # Kaiser-Meyer-Olkin Measure of Sampling Adequacy (KMO)
 KMO(cor(pca, use = "pairwise.complete.obs"))
 
 # Bartlett's test of spherecity
 print("Bartletts test of spherecity")
 print(data.frame(cortest.bartlett(cor(pca, use = "pairwise.complete.obs"), n = 53)))
 
 # scree plot
 scree(pca, factors = TRUE)
 
 # conduct parallel analysis to confirm no. components
 paran::paran(drop_na(pca), iterations = 5000, all = T, graph = T)
 
 # 2-component PCA
 n_comp <- 2
 rotate_method <- "promax"
 score_method <- "Bartlett"
 
 fit <- principal(pca, rotate = rotate_method, nfactors = n_comp,
                  method = score_method, scores = TRUE)
 
 # variance explained
 var_table()
 
 # pattern matrix
 pattern_matrix()
 
 # save component scores as dataframe
 pca_scores <- data.frame(fit$scores) %>% 
   rename(helpful_exchange_none_r_two = RC1, terrible_codriver_none_r_two = RC2)
 
 # add component scores to d
 d_all <- d_all %>% 
   left_join(bind_cols(comm %>% select(uid), pca_scores), by = "uid")
 
 # correlation bw comms factors
 cor.test(d_all$helpful_exchange_r_two, d_all$terrible_codriver_r_two)  
 
 

# fog periods -------------------------------------------------------------
pca <- comm %>% 
   select(co_question_fog, drive_question_fog, drive_informs_fog, co_info_help_fog, co_instruct_help_fog,
          co_instruct_harm_fog, co_info_harm_fog, drive_frust_fog, co_redundant_fog)
 
 # correlations bw original communication variables
 pca %>% 
   corstarsl() %>% 
   rownames_to_column() %>% 
   write_csv("output/comms_fog_corrs.csv")
 
 # Kaiser-Meyer-Olkin Measure of Sampling Adequacy (KMO)
 KMO(cor(pca, use = "pairwise.complete.obs"))
 
 # Bartlett's test of spherecity
 print("Bartletts test of spherecity")
 print(data.frame(cortest.bartlett(cor(pca, use = "pairwise.complete.obs"), n = 54)))
 
 # scree plot
 scree(pca, factors = TRUE)
 
 # conduct parallel analysis to confirm no. components
 paran::paran(drop_na(pca), iterations = 5000, all = T, graph = T)
 
 # 2-component PCA
 n_comp <- 2
 rotate_method <- "promax"
 score_method <- "Bartlett"
 
 fit <- principal(pca, rotate = rotate_method, nfactors = n_comp,
                  method = score_method, scores = TRUE)
 
 # variance explained
 var_table()
 
 # pattern matrix
 pattern_matrix()
 
 # save component scores as dataframe
 pca_scores <- data.frame(fit$scores) %>% 
   rename(helpful_exchange_fog_r_two = RC1, terrible_codriver_fog_r_two = RC2)
 
 # add component scores to d
 d_all <- d_all %>% 
   left_join(bind_cols(comm %>% select(uid), pca_scores), by = "uid")
 
 # correlation bw comms factors
 cor.test(d_all$helpful_exchange_fog_r_two, d_all$terrible_codriver_fog_r_two)  
 
 

# Hierarchical regression -------------------------------------------------
# uid == 17032915_2-d2 is a collisions_overall outlier
# The correlation between collisions_overall and helpful_exchange_r_two is 
# r = .245 with them in the dataset and r = .137 with them removed
# & collisions_overall < 500
# create dataset for regression
# reg_d <- d_all %>% filter(!is.na(terrible_codriver_r_two))

# DV = Comms --------------------------------------------------------------
# select variables and prep data
nested <- reg_d %>% select(terrible_codriver_r_two, helpful_exchange_r_two, terrible_codriver_none_r_two, 
                 helpful_exchange_none_r_two, terrible_codriver_fog_r_two, helpful_exchange_fog_r_two,
                 talk_time_overall_team, talk_time_none_team, talk_time_fog_team,
                 talk_turn_overall_team, talk_turn_none_team, talk_turn_fog_team,
                 prop_female, ef_time_factor_driver, competence_factor_driver, agreeableness, neuroticism,
                 ef_time_factor_drone, competence_factor_drone, agreeableness_drone, neuroticism_drone,
                 ef_time_factor_team, competence_factor_team, agreeableness_team, neuroticism_team) %>% 
  pivot_longer(terrible_codriver_r_two:talk_turn_fog_team, "var", "val") %>% 
  nest(data = c(value, prop_female:neuroticism_team))

# fit regression models
nested %>% 
  mutate(fit = map(data, ~ lm(scale(.x$value) ~ scale(.x$prop_female) + scale(.x$ef_time_factor_driver) + scale(.x$competence_factor_driver) +
                                scale(.x$agreeableness)  + scale(.x$neuroticism) + 
                                scale(.x$ef_time_factor_drone) + scale(.x$competence_factor_drone) + 
                                scale(.x$agreeableness_drone) + scale(.x$neuroticism_drone))),
         tidy_fit = map(fit, tidy),
         tidy_model = map(fit, glance)) %>% 
  unnest(tidy_fit) %>% 
  filter(p.value < .05)


c <- nested %>% 
  mutate(fit = map(data, ~ lm(scale(.x$value) ~ scale(.x$prop_female) + scale(.x$ef_time_factor_team) + scale(.x$competence_factor_team) +
                                scale(.x$agreeableness_team) + scale(.x$neuroticism_team))),
         tidy_fit = map(fit, tidy),
         tidy_model = map(fit, glance)) %>% 
  unnest(tidy_fit)
  # filter(p.value < .05)

# DV = sim metrics --------------------------------------------------------
# select variables and prep data
nested <- reg_d %>% select(uid, collisions_overall, speed_overall, collisions_none_overall, speed_none_overall,
                           collisions_fog_overall, speed_fog_overall, terrible_codriver_r_two, helpful_exchange_r_two, 
                           terrible_codriver_none_r_two, helpful_exchange_none_r_two, terrible_codriver_fog_r_two, 
                           helpful_exchange_fog_r_two, prop_female, ef_time_factor_driver, competence_factor_driver, 
                           agreeableness, neuroticism, ef_time_factor_drone, competence_factor_drone, agreeableness_drone, neuroticism_drone,
                           ef_time_factor_team, competence_factor_team, agreeableness_team, neuroticism_team) %>% 
  # mutate_if(is.numeric, standardise) %>%
  pivot_longer(collisions_overall:helpful_exchange_fog_r_two, names_to = "var", values_to = "val") %>% 
  mutate(e = str_extract(var, "none|fog"),
         e = ifelse(is.na(e), "overall", e),
         metric = str_extract(var, "collisions|speed|helpful_exchange|terrible_codriver")) %>% 
  select(-var) %>% 
  pivot_wider(names_from = metric, values_from = val) %>% 
  pivot_longer(collisions:speed, names_to = "sim_metric", values_to = "sim_val") %>% 
  # pivot_longer(helpful_exchange:terrible_codriver, names_to = "talk_metric", values_to = "talk_val") %>% 
  select(-uid) %>% 
  mutate(e = factor(e, levels = c("overall", "none", "fog")),
         sim_metric = factor(sim_metric, levels = c("collisions", "speed"))) %>% 
  nest(data = c(sim_val, helpful_exchange, terrible_codriver, prop_female:neuroticism_team))

# fit regression models for drivers
# drivers
fit_drive <- nested %>% 
  mutate(block1 = map(data, ~ lm(scale(.x$sim_val) ~ scale(.x$prop_female))),
         block2 = map(data, ~ lm(scale(.x$sim_val) ~ scale(.x$prop_female) + scale(.x$ef_time_factor_driver) + scale(.x$competence_factor_driver))),
         block3 = map(data, ~ lm(scale(.x$sim_val) ~ scale(.x$prop_female) + scale(.x$ef_time_factor_driver) + scale(.x$competence_factor_driver) +
                                   scale(.x$agreeableness) + scale(.x$neuroticism))),
         block4 = map(data, ~ lm(scale(.x$sim_val) ~ scale(.x$prop_female) + scale(.x$ef_time_factor_driver) + scale(.x$competence_factor_driver) +
                                   scale(.x$agreeableness) + scale(.x$neuroticism) + scale(.x$helpful_exchange) + scale(.x$terrible_codriver))))

# navigators
fit_drone <- nested %>% 
  mutate(block1 = map(data, ~ lm(scale(.x$sim_val) ~ scale(.x$prop_female))),
         block2 = map(data, ~ lm(scale(.x$sim_val) ~ scale(.x$prop_female) + scale(.x$ef_time_factor_drone) + scale(.x$competence_factor_drone))),
         block3 = map(data, ~ lm(scale(.x$sim_val) ~ scale(.x$prop_female) + scale(.x$ef_time_factor_drone) + scale(.x$competence_factor_drone) +
                                   scale(.x$agreeableness_drone) + scale(.x$neuroticism_drone))),
         block4 = map(data, ~ lm(scale(.x$sim_val) ~ scale(.x$prop_female) + scale(.x$ef_time_factor_drone) + scale(.x$competence_factor_drone) +
                                   scale(.x$agreeableness_drone) + scale(.x$neuroticism_drone) + scale(.x$helpful_exchange) + scale(.x$terrible_codriver))))

# teams
fit_team <- nested %>% 
  mutate(block1 = map(data, ~ lm(scale(.x$sim_val) ~ scale(.x$prop_female))),
         block2 = map(data, ~ lm(scale(.x$sim_val) ~ scale(.x$prop_female) + scale(.x$ef_time_factor_team) + scale(.x$competence_factor_team))),
         block3 = map(data, ~ lm(scale(.x$sim_val) ~ scale(.x$prop_female) + scale(.x$ef_time_factor_team) + scale(.x$competence_factor_team) +
                                   scale(.x$agreeableness_team) + scale(.x$neuroticism_team))),
         block4 = map(data, ~ lm(scale(.x$sim_val) ~ scale(.x$prop_female) + scale(.x$ef_time_factor_team) + scale(.x$competence_factor_team) +
                                   scale(.x$agreeableness_team) + scale(.x$neuroticism_team) + scale(.x$helpful_exchange) + scale(.x$terrible_codriver))))

x <- map(list(fit_drive, fit_drone, fit_team), function(i) {
  
  i %>% 
    mutate(tidy_coef1 = map(block1, tidy),
           tidy_coef2 = map(block2, tidy),
           tidy_coef3 = map(block3, tidy),
           tidy_coef4 = map(block4, tidy),
           tidy_model1 = map(block1, glance),
           tidy_model2 = map(block2, glance),
           tidy_model3 = map(block3, glance),
           tidy_model4 = map(block4, glance)) %>% 
    select(-data:-block4) %>% 
    pivot_longer(tidy_coef1:tidy_model4, names_to = "models", values_to = "mod_vals") %>% 
    mutate(block = str_extract(models, "[1-4]"),
           test = str_extract(models, "coef|model")) %>% 
    select(-models) %>% 
    unnest(mod_vals) %>% 
    select(-adj.r.squared:-nobs, -std.error) %>% 
    filter(str_detect(term, "scale") | is.na(term)) %>% 
    mutate(term = ifelse(is.na(term), "model", term)) %>%
    group_by(e, sim_metric) %>% 
    mutate(delta = r.squared - lag(r.squared),
           delta = ifelse(is.na(delta), r.squared, delta),
           R = sqrt(r.squared),
           block = factor(block),
           test = factor(test, levels = c("model", "coef"))) %>% 
    ungroup() %>% 
    rename(beta = estimate) %>% 
    pivot_longer(c(R, r.squared, delta, beta, statistic), names_to = "var", values_to = "val") %>%
    mutate(val = round(val, 2)) %>% 
    pivot_wider(names_from = var, values_from = val) %>% 
    arrange(sim_metric, e, block, test) %>% 
    select(sim_metric, e, block, test, term, R, r.squared, delta, beta, statistic, p.value) %>% 
    mutate(delta = ifelse(p.value < .001, paste0(round(delta,2), "***"), 
                   ifelse(p.value < .01 & p.value > .001, paste0(round(delta,2), "**"),
                   ifelse(p.value < .05 & p.value > .01, paste0(round(delta,2), "*"), round(delta,2)))),
           delta = ifelse(test == "model", delta, NA),
           beta = ifelse(p.value < .001, paste0(round(beta,2), "***"), 
                  ifelse(p.value < .01 & p.value > .001, paste0(round(beta,2), "**"),
                  ifelse(p.value < .05 & p.value > .01, paste0(round(beta,2), "*"), round(beta,2)))),
           beta = ifelse(test == "coef", beta, NA),
           p.value = round(p.value, 5)) %>% 
    select(-test)
})

# save output
x[[1]] %>% write_csv("output/reg_sim_metrics_drivers.csv")
x[[2]] %>% write_csv("output/reg_sim_metrics_navigators.csv")
x[[3]] %>% write_csv("output/reg_sim_metrics_teams.csv")
 
x[[2]] %>% filter(block == 4 & str_detect(term, "helpful|terrible"))

# plot relationship between comms and performance
sim_plot <- function(data) {
  data %>% 
    ggplot(aes(x = talk_val, y = sim_val)) +
    geom_point() + 
    # Implementation of linear model
    stat_smooth(
      method = "lm",
      formula = y ~ x,
      se = F,
      colour = "dark grey",
      linetype = "solid") +
    theme_classic() +
    theme(strip.background = element_blank(),
          strip.text = element_text(size=12,colour="black"),
          axis.text=element_text(size=12),
          axis.title=element_text(size=12),
          plot.title = element_text(hjust = 0.5, face = "bold", size = 12))
  
}

plot <- d_all %>% 
  filter(!is.na(terrible_codriver_r_two) & role == "team") %>% 
  select(uid, collisions_overall, collisions_none_overall, collisions_fog_overall,
         speed_overall, speed_none_overall, speed_fog_overall,
         helpful_exchange_r_two, helpful_exchange_none_r_two, helpful_exchange_fog_r_two,
         terrible_codriver_r_two, terrible_codriver_none_r_two, terrible_codriver_fog_r_two) %>% 
  pivot_longer(collisions_overall:terrible_codriver_fog_r_two, names_to = "var", values_to = "val") %>% 
  mutate(e = str_extract(var, "none|fog"),
         e = ifelse(is.na(e), "overall", e),
         metric = str_extract(var, "collisions|speed|helpful_exchange|terrible_codriver")) %>% 
  select(-var) %>% 
  pivot_wider(names_from = metric, values_from = val) %>% 
  pivot_longer(collisions:speed, names_to = "sim_metric", values_to = "sim_val") %>% 
  pivot_longer(helpful_exchange:terrible_codriver, names_to = "talk_metric", values_to = "talk_val") %>% 
  mutate(e = factor(e, levels = c("overall", "none", "fog"), labels = c("Overall", "Normal", "Fog")),
         sim_metric = factor(sim_metric, levels = c("collisions", "speed"), labels = c("Collisions", "Speed")),
         talk_metric = factor(talk_metric, levels = c("helpful_exchange", "terrible_codriver"), labels = c("Helpful Exchange", "Harmful Navigator")))

a <- plot %>% 
  filter(talk_metric == "Harmful Navigator" & sim_metric == "Collisions" & e == "Overall") %>% 
  sim_plot() +
  scale_y_continuous(breaks = seq(0,200,100), limits = c(0, 250)) +
  scale_x_continuous(breaks = seq(-2,3,1), limits = c(-2, 3.1)) +
  labs(x = "Harmful Navigator", y = "Collisions",
       title = "Overall")

b <- plot %>% 
  filter(talk_metric == "Harmful Navigator" & sim_metric == "Collisions" & e == "Normal") %>% 
  sim_plot() +
  scale_y_continuous(breaks = seq(0,200,100), limits = c(0, 250)) +
  scale_x_continuous(breaks = seq(-2,3,1), limits = c(-2, 3.1)) +
  labs(x = "Harmful Navigator", y = "",
       title = "Normal Periods")

c <- plot %>% 
  filter(talk_metric == "Harmful Navigator" & sim_metric == "Collisions" & e == "Fog") %>% 
  sim_plot() +
  scale_y_continuous(breaks = seq(0,200,100), limits = c(0, 250)) +
  scale_x_continuous(breaks = seq(-2,3,1), limits = c(-2, 3.1)) +
  labs(x = "Harmful Navigator", y = "",
       title = "Fog Periods")

d <- plot %>% 
  filter(talk_metric == "Helpful Exchange" & sim_metric == "Speed" & e == "Overall") %>% 
  sim_plot() +
  scale_y_continuous(breaks = seq(4,14,2), limits = c(4, 14)) +
  scale_x_continuous(breaks = seq(-2,3,1), limits = c(-2, 3.1)) +
  labs(x = "Helpful Exchange", y = "Speed",
       title = "Overall")

e <- plot %>% 
  filter(talk_metric == "Helpful Exchange" & sim_metric == "Speed" & e == "Normal") %>% 
  sim_plot() +
  scale_y_continuous(breaks = seq(4,14,2), limits = c(4, 14)) +
  scale_x_continuous(breaks = seq(-2,3,1), limits = c(-2, 3.1)) +
  labs(x = "Helpful Exchange", y = "",
       title = "Normal Periods")

f <- plot %>% 
  filter(talk_metric == "Helpful Exchange" & sim_metric == "Speed" & e == "Fog") %>% 
  sim_plot() +
  scale_y_continuous(breaks = seq(4,14,2), limits = c(4, 14)) +
  scale_x_continuous(breaks = seq(-2,3,1), limits = c(-2, 3.1)) +
  labs(x = "Helpful Exchange", y = "",
       title = "Fog Periods")

plot_grid(a,b,c,d,e,f, ncol = 3, labels = c("A", "B", "C", "D", "E", "F"))

ggsave("output/figure4.png")


# baseline regressor model: simple comms vars -----------------------------
cor.test(reg_d$talk_turn_overall_team, reg_d$talk_time_overall_team)
cor.test(reg_d$talk_turn_none_team, reg_d$talk_time_none_team)
cor.test(reg_d$talk_turn_fog_team, reg_d$talk_time_fog_team)

cor.test(reg_d$talk_time_overall_team, reg_d$collisions_overall)
cor.test(reg_d$talk_turn_overall_team, reg_d$collisions_overall)

cor.test(reg_d$talk_time_overall_team, reg_d$speed_overall)
cor.test(reg_d$talk_turn_overall_team, reg_d$speed_overall)

# select variables and prep data
nested <- reg_d %>% 
    select(uid, collisions_overall, speed_overall, collisions_none_overall, speed_none_overall,
           collisions_fog_overall, speed_fog_overall, 
           talk_time_overall_team, talk_time_fog_team, talk_time_none_team,
           talk_turn_overall_team, talk_turn_fog_team, talk_turn_none_team,
           prop_female, ef_time_factor_driver, competence_factor_driver, 
           agreeableness, neuroticism, ef_time_factor_drone, competence_factor_drone, agreeableness_drone, neuroticism_drone,
           ef_time_factor_team, competence_factor_team, agreeableness_team, neuroticism_team) %>% 
    pivot_longer(collisions_overall:talk_turn_none_team, names_to = "var", values_to = "val") %>% 
    mutate(e = str_extract(var, "none|fog"),
           e = ifelse(is.na(e), "overall", e),
           metric = str_extract(var, "collisions|speed|time|turn")) %>% 
    select(-var) %>% 
    pivot_wider(names_from = metric, values_from = val) %>% 
    pivot_longer(collisions:speed, names_to = "sim_metric", values_to = "sim_val") %>% 
    pivot_longer(time:turn, names_to = "talk_metric", values_to = "talk_val") %>% 
    select(-uid) %>% 
    mutate(e = factor(e, levels = c("overall", "none", "fog")),
           sim_metric = factor(sim_metric, levels = c("collisions", "speed")),
           talk_metric = factor(talk_metric, levels = c("time", "turn"))) %>% 
  nest(data = c(sim_val, talk_val, prop_female:neuroticism_team))
    # filter(!(sim_metric == "collisions" & talk_metric == "time") &
    #          !(sim_metric == "speed" & talk_metric == "turn"))


# fit regression models for teams
fit_team <- nested %>% 
  mutate(block1 = map(data, ~ lm(scale(.x$sim_val) ~ scale(.x$prop_female))),
         block2 = map(data, ~ lm(scale(.x$sim_val) ~ scale(.x$prop_female) + scale(.x$ef_time_factor_team) + scale(.x$competence_factor_team))),
         block3 = map(data, ~ lm(scale(.x$sim_val) ~ scale(.x$prop_female) + scale(.x$ef_time_factor_team) + scale(.x$competence_factor_team) +
                                   scale(.x$agreeableness_team) + scale(.x$neuroticism_team))),
         block4 = map(data, ~ lm(scale(.x$sim_val) ~ scale(.x$prop_female) + scale(.x$ef_time_factor_team) + scale(.x$competence_factor_team) +
                                   scale(.x$agreeableness_team) + scale(.x$neuroticism_team) + scale(.x$talk_val))))

fit_team %>% 
  mutate(tidy_coef1 = map(block1, tidy),
         tidy_coef2 = map(block2, tidy),
         tidy_coef3 = map(block3, tidy),
         tidy_coef4 = map(block4, tidy),
         tidy_model1 = map(block1, glance),
         tidy_model2 = map(block2, glance),
         tidy_model3 = map(block3, glance),
         tidy_model4 = map(block4, glance)) %>% 
  select(-data:-block4) %>% 
  pivot_longer(tidy_coef1:tidy_model4, names_to = "models", values_to = "mod_vals") %>% 
  mutate(block = str_extract(models, "[1-4]"),
         test = str_extract(models, "coef|model")) %>% 
  select(-models) %>% 
  unnest(mod_vals) %>% 
  select(-adj.r.squared:-nobs, -std.error) %>% 
  filter(str_detect(term, "scale") | is.na(term)) %>% 
  mutate(term = ifelse(is.na(term), "model", term)) %>%
  group_by(e, sim_metric) %>% 
  mutate(delta = r.squared - lag(r.squared),
         delta = ifelse(is.na(delta), r.squared, delta),
         R = sqrt(r.squared),
         block = factor(block),
         test = factor(test, levels = c("model", "coef"))) %>% 
  ungroup() %>% 
  rename(beta = estimate) %>% 
  pivot_longer(c(R, r.squared, delta, beta, statistic), names_to = "var", values_to = "val") %>%
  mutate(val = round(val, 2)) %>% 
  pivot_wider(names_from = var, values_from = val) %>% 
  arrange(sim_metric, e, block, test) %>% 
  mutate(delta = ifelse(p.value < .001, paste0(round(delta,2), "***"), 
                        ifelse(p.value < .01 & p.value > .001, paste0(round(delta,2), "**"),
                               ifelse(p.value < .05 & p.value > .01, paste0(round(delta,2), "*"), round(delta,2)))),
         delta = ifelse(test == "model", delta, NA),
         beta = ifelse(p.value < .001, paste0(round(beta,2), "***"), 
                       ifelse(p.value < .01 & p.value > .001, paste0(round(beta,2), "**"),
                              ifelse(p.value < .05 & p.value > .01, paste0(round(beta,2), "*"), round(beta,2)))),
         beta = ifelse(test == "coef", beta, NA),
         p.value = round(p.value, 5)) %>% 
  select(sim_metric, talk_metric, e, block, term, R, r.squared, delta, beta, statistic, p.value) %>% 
  write_csv("output/reg_simple_comms.csv")
  



# polynomial regression ---------------------------------------------------
# library(cowplot)

# # align all plots vertically
# plots <- align_plots(a,b,c,d,e,
#                      align = 'v', axis = 'l')
# 
# # compile the bottom row of the plot grid with legend
# top_row <- plot_grid(
#   plots[[1]], 
#   plots[[2]], 
#   legend,
#   labels = c("A", "B"),
#   # rel_widths = c(.68, .32),
#   label_x = .02, 
#   hjust = 0,
#   label_size = 16,
#   nrow = 1
# )
# top_row
# ggsave("output/test.png")
# 
# # create plot grid
# fig <- plot_grid(plots[[1]], 
#                  bottom_row, 
#                  labels = c("Vignette dataset"),
#                  ncol = 1,
#                  label_x = .01, 
#                  hjust = 0, 
#                  label_size = 16)

# fit poly regression
# function to standardise data for regression
standardise <- function(x) {
  (x - mean(x, na.rm = TRUE)) / sd(x, na.rm = TRUE)
}

# nested <- d_all %>% 
#   select(collisions_fog_overall, collisions_none_overall, speed_fog_overall, speed_none_overall, 
#          helpful_exchange_none_r_two, helpful_exchange_fog_r_two, 
#          terrible_codriver_none_r_two, terrible_codriver_fog_r_two) %>% 
#   filter(!is.na(terrible_codriver_none_r_two)) %>% 
#   mutate_if(is.numeric, standardise) %>%
#   pivot_longer(helpful_exchange_none_r_two:terrible_codriver_fog_r_two, names_to = "comm_var", values_to = "comm") %>% 
#   pivot_longer(collisions_fog_overall:speed_none_overall, names_to = "perf_var", values_to = "perform") %>% 
#   nest(data = c(comm, perform))

nested <- d_all %>% 
  filter(!is.na(terrible_codriver_r_two) & role == "team") %>% 
  select(uid, collisions_overall, collisions_none_overall, collisions_fog_overall,
         speed_overall, speed_none_overall, speed_fog_overall,
         helpful_exchange_r_two, helpful_exchange_none_r_two, helpful_exchange_fog_r_two,
         terrible_codriver_r_two, terrible_codriver_none_r_two, terrible_codriver_fog_r_two,
         talk_time_overall_team, talk_time_none_team, talk_time_fog_team,
         talk_turn_overall_team, talk_turn_none_team, talk_turn_fog_team) %>% 
    mutate_if(is.numeric, standardise) %>%
  pivot_longer(collisions_overall:talk_turn_fog_team, names_to = "var", values_to = "val") %>% 
  mutate(e = str_extract(var, "none|fog"),
         e = ifelse(is.na(e), "overall", e),
         metric = str_extract(var, "collisions|speed|helpful_exchange|terrible_codriver|talk_time|talk_turn")) %>% 
  select(-var) %>% 
  pivot_wider(names_from = metric, values_from = val) %>% 
  pivot_longer(collisions:speed, names_to = "sim_metric", values_to = "sim_val") %>% 
  pivot_longer(helpful_exchange:talk_turn, names_to = "talk_metric", values_to = "talk_val") %>% 
  select(-uid) %>% 
  nest(data = c(talk_val, sim_val))


poly <- d_all %>% 
  filter(!is.na(terrible_codriver_r_two) & role == "team")

fit <- lm(scale(collisions_overall) ~ poly(scale(terrible_codriver_r_two), 3), data = poly)
summary(fit)

nested %>% 
  mutate(fit = map(data, ~ lm(sim_val ~ poly(talk_val, degree = 3), data = .)),
         tidy_fit = map(fit, tidy),
         model_fit = map(fit, glance)) %>%
  unnest(tidy_fit) %>% 
  select(-std.error) %>% 
  pivot_longer(estimate:p.value, names_to = "var", values_to = "val") %>%
  mutate(poly = str_extract(term, "[0-9]+$"),
         poly = str_c("poly_", poly),
         poly = ifelse(is.na(poly), "intercept", poly)) %>% 
  filter(poly != "intercept") %>% 
  pivot_wider(names_from = var, values_from = val) %>% 
  rename(t_coef = statistic, reg_coef = estimate, p_coef = p.value) %>% 
    unnest(model_fit) %>% 
  rename(f_model = statistic, p_model = p.value) %>% 
  mutate(reg_coef = ifelse(p_coef < .001, paste0(round(reg_coef,2), "***"), 
                               ifelse(p_coef < .01 & p_coef > .001, paste0(round(reg_coef,2), "**"),
                                      ifelse(p_coef < .05 & p_coef > .01, paste0(round(reg_coef,2), "*"), round(reg_coef,2))))) %>% 
  select(-t_coef, -p_coef, -adj.r.squared, -sigma, -logLik:-deviance, -nobs, -data, -fit, -term) %>% 
  pivot_wider(names_from = poly, values_from = reg_coef) %>% 
  select(talk_metric, sim_metric, e, f_model, p_model, df, df.residual, r.squared, poly_1:poly_3) %>% 
  mutate(talk_metric = factor(talk_metric, levels = c("helpful_exchange", "terrible_codriver", "talk_time", "talk_turn")),
         sim_metric = factor(sim_metric, levels = c("collisions", "speed")),
         e = factor(e, levels = c("overall", "none", "fog"))) %>% 
  arrange(talk_metric, sim_metric, e) %>%
  write_csv("output/comm_perform_poly.csv")


