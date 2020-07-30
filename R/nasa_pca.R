library(tidyverse)
library(knitr)
library(corrplot)
library(psych)
# library(car) # load package to diagnose multicollinearity
# library(GGally)
# # library(olsrr) # print semi-part correlations
# library(mctest)
source("R/corr_matrix_sig.R")

vars <- read_csv("data/200227_comms_efa_vars.csv") %>% 
  filter(!team %in% c("17080712_1", "17080810_1"))

# select original comms variables
# effort_drone:temporal_demand_other_drone
pca1 <- vars %>% select(effort:temporal_demand)
pca2 <- vars %>% select(effort_other:temporal_demand_other)
pca3 <- vars %>% select(effort_drone:temporal_demand_drone)
pca4 <- vars %>% select(effort_other_drone:temporal_demand_other_drone)

# pca <- vars %>% dplyr::select(vars)
pca <- c("pca1", "pca2", "pca3", "pca4")

pca_scores <- map(pca, function(i) {
# create the correlation matrix for PCA so we know how it was done (e.g., how missing values were treated)
cor_pca <- star_matrix(get(i))

# print correlations
kable(cor_pca)

# Visualise correlations to see if variables appear to cluster
corrplot(cor(get(i), use="complete.obs"), order = "hclust", tl.col='black', tl.cex=.75)


# Kaiser-Meyer-Olkin Measure of Sampling Adequacy (KMO)
print(KMO(cor(get(i), use="complete.obs")))

# Bartlett's test of spherecity
print("Bartletts test of spherecity")
print(data.frame(cortest.bartlett(cor(get(i), use="complete.obs"), n = 55)))

# 3-component PCA
n_comp <- 2
rotate_method <- "promax"
score_method <- "Bartlett"

fit <- principal(get(i), rotate = rotate_method, nfactors = n_comp, 
                 method = score_method, scores = TRUE, n.obs = 55)

# communalities
x <- data.frame(communalities = round(fit$communality, 2))

kable(x)


# variance explained
x <- data.frame(component = 1:nrow(x),
                eigen = fit$values,
                prop_var = c(fit$Vaccounted[2,c(1:n_comp)], rnorm(nrow(x)-n_comp, 0, 0)),
                cum_var = c(fit$Vaccounted[3,c(1:n_comp)], rnorm(nrow(x)-n_comp, 0, 0)),
                rotation_SS_load = c(fit$Vaccounted[1,c(1:n_comp)], rnorm(nrow(x)-n_comp, 0, 0))) %>% 
  round(2)

x[x == 0] <- ""

kable(x)

# Scree plot
p <- data.frame(component = 1:nrow(x),
                eigen = fit$values)

p %>% ggplot(aes(x = component, y = eigen)) +
  geom_line() + geom_point() +
  scale_x_continuous(breaks = 1:nrow(x)) +
  theme_minimal() +
  labs(title = "Scree plot") +
  theme(plot.title = element_text(hjust = 0.5))


# pattern matrix
load <- data.frame(var = rownames(fit$loadings),
                   PC1 = round(fit$loadings[1:nrow(x)], 2),
                   PC2 = round(fit$loadings[(1+nrow(x)):(nrow(x)*2)], 2)) %>% 
  mutate(PC1 = ifelse(PC1 < .3 & PC1 > -.3, "", PC1),
         PC2 = ifelse(PC2 < .3 & PC2 > -.3, "", PC2)) %>% 
  arrange(desc(PC1), desc(PC2))

kable(load)




if (i == "pca1") {
  # Component correlations matrix
  rownames(fit$r.scores) <- c("positive_nasa", "negative_nasa")
  colnames(fit$r.scores) <- c("positive_nasa", "negative_nasa")

  round(fit$r.scores,2)

  # save component scores as dataframe
  pca_scores <- data.frame(fit$scores) %>%
    rename(positive_nasa_driver = RC1, negative_nasa_driver = RC2)

} else if (i == "pca2") {
  # Component correlations matrix
  rownames(fit$r.scores) <- c("positive_nasa", "negative_nasa")
  colnames(fit$r.scores) <- c("positive_nasa", "negative_nasa")

  round(fit$r.scores,2)

  # save component scores as dataframe
  pca_scores <- data.frame(fit$scores) %>%
    rename(positive_nasa_driver_other = RC1, negative_nasa_driver_other = RC2)

} else if (i == "pca3") {
# Component correlations matrix
rownames(fit$r.scores) <- c("negative_nasa", "positive_nasa")
colnames(fit$r.scores) <- c("negative_nasa", "positive_nasa")

round(fit$r.scores,2)


# save component scores as dataframe
pca_scores <- data.frame(fit$scores) %>%
  rename(negative_nasa_drone = RC1, positive_nasa_drone = RC2)

} else if (i == "pca4") {
  # Component correlations matrix
  rownames(fit$r.scores) <- c("negative_nasa", "positive_nasa")
  colnames(fit$r.scores) <- c("negative_nasa", "positive_nasa")

  round(fit$r.scores,2)


  # save component scores as dataframe
  pca_scores <- data.frame(fit$scores) %>%
    rename(negative_nasa_drone_other = RC1, positive_nasa_drone_other = RC2)
  
  }
})


# append component scores to dataset
pca_scores <- bind_cols(pca_scores)

vars <- cbind(vars, pca_scores)



c <- star_matrix(vars %>% select(contains("nasa")))

# no relationship between between driver's subjective assessment of themself and the drone's assessment of the driver
vars %>% 
  select(positive_nasa_driver, negative_nasa_driver, 
         negative_nasa_drone_other, positive_nasa_drone_other) %>% 
  star_matrix()

# no relationship between between drone's subjective assessment of themself and the driver's assessment of the drone
vars %>% 
  select(positive_nasa_driver_other, negative_nasa_driver_other, 
         negative_nasa_drone, positive_nasa_drone) %>% 
  star_matrix()

vars %>% select(contains("conf"), contains("bias"), contains("disc"))

c <- vars %>% 
  select(contains("conf"), contains("bias"), contains("disc"), contains("nasa")) %>% 
  select(-contains("dist")) %>% 
  star_matrix()


# drone's ratings on self on negative scales positively related with collisions
# driver's ratings of the drone on positive and negative scales positively related with distance travelled
# drone's ratings of self on the negative scale positively related with distance travelled
# nothing related with speed
c <- vars %>% 
  select(collisions_overall, speed_overall, distance_overall,
         positive_nasa_driver, negative_nasa_driver, 
         negative_nasa_drone_other, positive_nasa_drone_other,
         positive_nasa_driver_other, negative_nasa_driver_other, 
         negative_nasa_drone, positive_nasa_drone) %>% 
  star_matrix()




# correlations between original nasa vars and sim performance metrics
# driver's ratings of self have no relation with performance
# drone's ratings of self on effort, frustration, temporal demand and mental demand positively relate with collisions
# drone's assessment of the driver's performance positively relate with speed
# driver's assessment of the drone's physical demand positively related with collisions
# driver's assessment of the drone's effort, frustration, and temporal demand positively related with distance travelled
# drone's rating of own effort positively related with distance travelled
c <- vars %>% 
  select(collisions_overall, speed_overall, distance_overall,
         effort:temporal_demand,
         effort_other:temporal_demand_other,
         effort_drone:temporal_demand_drone,
         effort_other_drone:temporal_demand_other_drone) %>% 
  star_matrix()

#' Takeaways
#' Drone evaluated own performance using collisions but evaluated driver's performance using speed
#' Driver's own ratings have no relation with performance but evaluated drone using mostly distance and collisions
#' Driver's saw drone's role as directing them through the urban environment but drone's saw their role as reducing errors (collisions)
#' Appears mental models diverged... clarifying roles between members before beginning the task may be beneficial.  