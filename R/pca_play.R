library(tidyverse)
library(here)
library(corrr)
library(psych)
library(corrplot)
library(GPArotation)

# Great post about running PCA with rotation in R
# https://stats.stackexchange.com/questions/59213/how-to-compute-varimax-rotated-principal-components-in-r

# read in the data and select the comms variables
vars <- read_csv(here("data/200221_comms_efa_vars.csv")) %>% 
  filter(!team %in% c("17080712_1", "17080810_1"))


pca <- vars %>% dplyr::select(co_info_help_overall:drive_frust_overall, 
                -contains("ratio"), -co_total_help_overall, 
                -co_total_harm_overall, -co_total_overall)

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
           prop_var = c(fit$Vaccounted[2,c(1:3)], rnorm(6, 0, 0)),
           cum_var = c(fit$Vaccounted[3,c(1:3)], rnorm(6, 0, 0)),
           rotation_SS_load = c(fit$Vaccounted[1,c(1:3)], rnorm(6, 0, 0))) %>% 
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


# check that spss and r component scores are the same
cor.test(vars$inconsistent_codriver, fit$scores[,1])
cor.test(vars$terrible_codriver, fit$scores[,2])
cor.test(vars$helpful_exchange, fit$scores[,3])
