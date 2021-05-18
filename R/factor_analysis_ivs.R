# # load packages
packages <- c("tidyverse", "psych", "knitr", "kableExtra")
lapply(packages, library, character.only = TRUE)

source("R/pca_functions.R")

# read data
d <- read_csv("data/200309_comms_efa_vars.csv") %>% 
  filter(!team %in% c("17080712_1", "17080810_1"))  # remove outliers

# impute missing values 
d <- d %>% 
  mutate(congruent_errors_drone = ifelse(is.na(congruent_errors_drone), round(mean(congruent_errors_drone, na.rm = T),0), congruent_errors_drone),
         congruent_time_drone = ifelse(is.na(congruent_time_drone), mean(congruent_time_drone, na.rm = T), congruent_time_drone),
         incongruent_errors_drone = ifelse(is.na(incongruent_errors_drone), round(mean(incongruent_errors_drone, na.rm = T),0), incongruent_errors_drone),
         incongruent_time_drone = ifelse(is.na(incongruent_time_drone), mean(incongruent_time_drone, na.rm = T), incongruent_time_drone),
         inhibitory_cost_drone = ifelse(is.na(inhibitory_cost_drone), mean(inhibitory_cost_drone, na.rm = T), inhibitory_cost_drone))

# sim metrics
# collisions_overall, speed_overall, distance_overall

# comms facs
# inconsistent_codriver, terrible_codriver, helpful_exchange

# select variables
# control vars: age_co_driver:sex_driver, prop_female, driving_years, gaming_time
x <- d %>% 
  select(congruent_errors:incongruent_time, repeat_errors:switch_time, wm_accuracy:confidence, agreeableness:neuroticism, 
         congruent_errors_drone:incongruent_time_drone, repeat_errors_drone:switch_time_drone,
         wm_accuracy_drone:confidence_drone, agreeableness_drone:neuroticism_drone)

# % missing values for each variable
x %>% summarise(across(everything(), ~ sum(is.na(.)))) %>% 
  pivot_longer(everything(), names_to = "var", values_to = "n") %>% 
  filter(n != 0)

# # save correlations for drivers and codrivers
# corstarsl(x %>% select(congruent_errors:neuroticism)) %>% 
#   rownames_to_column() %>% 
#   write_csv("output/driver_corrs_factor_reduction.csv")
# 
# corstarsl(x %>% select(congruent_errors_drone:neuroticism_drone)) %>% 
#   rownames_to_column() %>% 
#   write_csv("output/drone_corrs_factor_reduction.csv")


# DRIVER ------------------------------------------------------------------

# PCA for EF vars ---------------------------------------------------------
vars <- x %>% select(matches("congruent"), matches("switch"), matches("repeat"), 
                     -matches("drone"))

# Kaiser-Meyer-Olkin Measure of Sampling Adequacy (KMO)
KMO(cor(vars))

# Bartlett's test of spherecity
print("Bartletts test of spherecity")
print(data.frame(cortest.bartlett(cor(vars), n = 54)))

# scree plot
scree(vars, factors = TRUE)

# 1-component PCA
n_comp <- 2
rotate_method <- "promax" # rotation with kaiser normalization
score_method <- "Bartlett"

fit <- principal(vars, rotate = rotate_method, nfactors = n_comp,
                 method = score_method, scores = TRUE)

# variance explained
var_table()

# pattern matrix
pattern_matrix()

# save component scores as dataframe
pca_scores <- data.frame(fit$scores) %>% 
  rename(ef_time_factor_driver = RC1, ef_errors_factor_driver = RC2)

# add component scores to d
d <- d %>% bind_cols(pca_scores)


# PCA for cog and personality vars ---------------------------------------------------------
vars <- x %>% select(wm_accuracy, gf_accuracy, confidence)

# Kaiser-Meyer-Olkin Measure of Sampling Adequacy (KMO)
KMO(cor(vars))

# Bartlett's test of spherecity
print("Bartletts test of spherecity")
print(data.frame(cortest.bartlett(cor(vars), n = 54)))

# scree plot
scree(vars, factors = TRUE)

# 1-component PCA
n_comp <- 1
rotate_method <- "promax" # rotation with kaiser normalization
score_method <- "Bartlett"

fit <- principal(vars, rotate = rotate_method, nfactors = n_comp,
          method = score_method, scores = TRUE)

# variance explained
var_table()

# pattern matrix
pattern_matrix()

# save component scores as dataframe
pca_scores <- data.frame(fit$scores) %>% 
  rename(competence_factor_driver = PC1)

# add component scores to d
d <- d %>% bind_cols(pca_scores)


# DRONE ------------------------------------------------------------------

# PCA for EF vars ---------------------------------------------------------
vars <- x %>% 
  select(matches("congruent"), matches("switch"), matches("repeat")) %>% 
  select(matches("drone"))

# Kaiser-Meyer-Olkin Measure of Sampling Adequacy (KMO)
KMO(cor(vars))

# Bartlett's test of spherecity
print("Bartletts test of spherecity")
print(data.frame(cortest.bartlett(cor(vars), n = 54)))

# scree plot
scree(vars, factors = TRUE)

# scree plot suggests 3 components but 2 component model better fit. 
# Conduct parallel analysis to confirm # components
# fa.parallel(vars)
paran::paran(vars, iterations = 5000, all = T, graph = T)

# PCA
n_comp <- 2
rotate_method <- "promax" # rotation with kaiser normalization
score_method <- "Bartlett"

fit <- principal(vars, rotate = rotate_method, nfactors = n_comp,
                 method = score_method, scores = TRUE)

# variance explained
var_table()

# pattern matrix
pattern_matrix()

# save component scores as dataframe
pca_scores <- data.frame(fit$scores) %>% 
  rename(ef_time_factor_drone = RC1, ef_errors_factor_drone = RC2)

# add component scores to d
d <- d %>% bind_cols(pca_scores)


# PCA for cog and personality vars ---------------------------------------------------------
vars <- x %>% select(wm_accuracy_drone, gf_accuracy_drone, confidence_drone)

# Kaiser-Meyer-Olkin Measure of Sampling Adequacy (KMO)
KMO(cor(vars))

# Bartlett's test of spherecity
print("Bartletts test of spherecity")
print(data.frame(cortest.bartlett(cor(vars), n = 54)))

# scree plot
scree(vars, factors = TRUE)

# 1-component PCA
n_comp <- 1
rotate_method <- "promax" # rotation with kaiser normalization
score_method <- "Bartlett"

fit <- principal(vars, rotate = rotate_method, nfactors = n_comp,
                 method = score_method, scores = TRUE)

# variance explained
var_table()

# pattern matrix
pattern_matrix()

# save component scores as dataframe
pca_scores <- data.frame(fit$scores) %>% 
  rename(competence_factor_drone = PC1)

# add component scores to d
d <- d %>% bind_cols(pca_scores)


d %>% write_csv("data/210511_comms_efa_vars_and_iv_reduction.csv")




