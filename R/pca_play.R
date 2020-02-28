library(tidyverse)
library(here)
library(corrr)
library(psych)
library(corrplot)
library(GPArotation)
library(knitr)

# Great post about running PCA with rotation in R
# https://stats.stackexchange.com/questions/59213/how-to-compute-varimax-rotated-principal-components-in-r

# read in the data and select the comms variables
vars <- read_csv(here("data/200221_comms_efa_vars.csv")) %>% 
  filter(!team %in% c("17080712_1", "17080810_1"))

# create vector of event types for loop (map)
type <- c("overall", "no fog", "fog")

# create empty dataframe to store component scores
pca_scores <- data.frame(a = NA)

pca_scores <- map(type, function(i) {

# set the event type (overall, no fog, or fog)
event <- i

if (event == "overall") {
  pca <- vars %>% 
    select(co_info_help_overall:drive_frust_overall, 
           -contains("ratio"), -co_total_help_overall, 
           -co_total_harm_overall, -co_total_overall)
} else if (event == "no fog") {
  pca <- vars %>% 
    select(co_info_help_no_fog, co_info_harm_no_fog, co_instruct_help_no_fog, 
           co_instruct_harm_no_fog, co_question_no_fog, co_redundant_no_fog,   
           drive_informs_no_fog, drive_question_no_fog, drive_frust_no_fog)
} else if (event == "fog") {
  pca <- vars %>% 
    select(co_info_help_fog, co_info_harm_fog, co_instruct_help_fog, 
           co_instruct_harm_fog, co_question_fog, co_redundant_fog,   
           drive_informs_fog, drive_question_fog, drive_frust_fog)
}

# descriptive statistics
x <- describe(pca)

kable(x)

# Visualise correlations to see if variables cluster
corrplot(cor(pca, use="complete.obs"), order = "hclust", tl.col='black', tl.cex=.75)

# create the correlation matrix for PCA so we know how it was done (e.g., how missing values were treated)
cor_pca <- cor(pca, use="pairwise.complete.obs")
kable(round(cor_pca, 2))

# Kaiser-Meyer-Olkin Measure of Sampling Adequacy (KMO)
KMO(cor_pca)

# Bartlett's test of spherecity
cortest.bartlett(cor_pca, n = 55)

# 3-component PCA
n_comp <- 3
rotate_method <- "promax"
score_method <- "Bartlett"

fit <- principal(pca, rotate = rotate_method, nfactors = n_comp, 
                        method = score_method, scores = TRUE, n.obs = 55)

# communalities
x <- data.frame(communalities = fit$communality)

kable(x)

# variance explained
x <- data.frame(component = 1:9,
           eigen = fit$values,
           prop_var = c(fit$Vaccounted[2,c(1:3)], rep(0, 6)),
           cum_var = c(fit$Vaccounted[3,c(1:3)], rep(0, 6)),
           rotation_SS_load = c(fit$Vaccounted[1,c(1:3)], rep(0, 6))) %>% 
  round(2)

x[x == 0] <- ""

kable(x)


# Scree plot
p <- data.frame(component = 1:9,
           eigen = fit$values)

p %>% ggplot(aes(x = component, y = eigen)) +
  geom_line() + geom_point() +
  theme_minimal() +
  labs(title = "Scree plot") +
  theme(plot.title = element_text(hjust = 0.5))

# pattern matrix
load <- data.frame(var = rownames(fit$loadings),
                   PC1 = round(fit$loadings[1:9], 2),
                   PC2 = round(fit$loadings[10:18], 2),
                   PC3 = round(fit$loadings[19:27], 2)) %>% 
  mutate(PC1 = ifelse(PC1 < .3 & PC1 > -.3, "", PC1),
         PC2 = ifelse(PC2 < .3 & PC2 > -.3, "", PC2),
         PC3 = ifelse(PC3 < .3 & PC3 > -.3, "", PC3)) %>% 
  arrange(desc(PC1), desc(PC2), desc(PC3))
  
kable(load)

# Component correlations matrix
rownames(fit$r.scores) <- c("PC1", "PC2", "PC3")
colnames(fit$r.scores) <- c("PC1", "PC2", "PC3")

round(fit$r.scores,2)

# check that spss and r component scores are the same for overall components
if (event == "overall") {
cor1 <- cor.test(vars$inconsistent_codriver, fit$scores[,1])
cor2 <- cor.test(vars$terrible_codriver, fit$scores[,2])
cor3 <- cor.test(vars$helpful_exchange, fit$scores[,3])

data.frame(component = c("inconsistent_codriver", "terrible_codriver", "helpful_exchange"),
           r = round(c(cor1$estimate, cor2$estimate, cor3$estimate),4))

}

# append component scores to dataset
if (event == "overall") {
  # save component scores as dataframe
  scores <- data.frame(fit$scores) %>% 
    rename(inconsistent_codriver_overall_r = RC1, terrible_codriver_overall_r = RC2, helpful_exchange_overall_r = RC3)
  
  # # append component scores to dataset
  # vars <- cbind(vars, pca_scores)
  
} else if (event == "no fog") {
  scores <- data.frame(fit$scores) %>% 
    rename(inconsistent_codriver_no_fog_r = RC1, terrible_codriver_no_fog_r = RC2, helpful_exchange_no_fog_r = RC3)
  
  # # append component scores to dataset
  # vars <- cbind(vars, pca_scores)
  
} else if (event == "fog") {
  scores <- data.frame(fit$scores) %>% 
    rename(helpful_exchange_fog_r = RC1, inconsistent_codriver_fog_r = RC2, terrible_codriver_fog_r = RC3)
  
  # # append component scores to dataset
  # vars <- cbind(vars, pca_scores)
  
}

# save component scores
pca_scores <- cbind(pca_scores, scores) %>% select(-a)

})

# add component scores to vars dataset
vars <- vars %>% bind_cols(bind_cols(pca_scores))





