

p <- vars %>% select(inconsistent_codriver, agreeableness, bias_drone) %>% 
  gather(var, val, agreeableness, bias_drone) %>% 
  mutate(var = factor(var, levels = c("agreeableness", "bias_drone"), 
                      labels = c("Agreeableness", "Bias UAV Operator"))) %>% 
  ggplot(aes(x = val, y = inconsistent_codriver)) +
  geom_point() +
  facet_wrap(~var, scales = "free_x") +
  geom_smooth(method='lm', formula = y~x, se=FALSE) +
  theme_classic() +
  theme(strip.background = element_blank(),
        strip.text = element_text(size = 12)) +
  # plot.title = element_text((face = "bold"))) +
  labs(x = "", y = "Inconsistent UAV Operator")

print(p)

ggsave("output/inconsistent.png")



p <- vars %>% select(helpful_exchange, agreeableness) %>% 
  ggplot(aes(x = agreeableness, y = helpful_exchange)) +
  geom_point() +
  geom_smooth(method='lm', formula = y~x, se=FALSE) +
  theme_classic() +
  theme(strip.background = element_blank(),
        strip.text = element_text(size = 12),
        plot.title = element_text(size = 12, hjust = 0.5)) +
  labs(title = "Agreeableness",
       x = "", y = "Helpful Exchange")

print(p)

ggsave("output/helpful.png")

vars %>% filter(collisions_overall > 500)


p <- vars %>% 
  filter(collisions_overall < 500) %>% 
  select(collisions_overall, terrible_codriver, incongruent_errors, prop_female) %>% 
  gather(var, val, -collisions_overall) %>%
  mutate(var = factor(var, levels = c("terrible_codriver", "incongruent_errors", "prop_female"),
                      labels = c("Terrible UAV Operator", "Incongruent Errors", "Proportion Female"))) %>%
  ggplot(aes(x = val, y = collisions_overall)) +
  geom_point() +
  facet_wrap(~var, scales = "free_x") +
  geom_smooth(method='lm', formula = y~x, se=FALSE) +
  theme_classic() +
  theme(strip.background = element_blank(),
        strip.text = element_text(size = 12),
        plot.title = element_text(size = 12, hjust = 0.5)) +
  labs(x = "", y = "Collisions")

print(p)

ggsave("output/collisions.png")


p <- vars %>% 
  select(speed_overall, helpful_exchange, switch_time_drone, prop_female) %>% 
  gather(var, val, -speed_overall) %>%
  mutate(var = factor(var, levels = c("helpful_exchange", "switch_time_drone", "prop_female"),
                      labels = c("Helpful Exchange", "Switch Time", "Proportion Female"))) %>%
  ggplot(aes(x = val, y = speed_overall)) +
  geom_point() +
  facet_wrap(~var, scales = "free_x") +
  geom_smooth(method='lm', formula = y~x, se=FALSE) +
  theme_classic() +
  theme(strip.background = element_blank(),
        strip.text = element_text(size = 12),
        plot.title = element_text(size = 12, hjust = 0.5)) +
  labs(x = "", y = "Speed")

print(p)

ggsave("output/speed.png")



p <- vars %>% 
  select(distance_overall, terrible_codriver, switch_cost) %>% 
  gather(var, val, -distance_overall) %>%
  mutate(var = factor(var, levels = c("terrible_codriver", "switch_cost"),
                      labels = c("Terrible UAV Operator", "Switch Cost"))) %>%
  ggplot(aes(x = val, y = distance_overall)) +
  geom_point() +
  facet_wrap(~var, scales = "free_x") +
  geom_smooth(method='lm', formula = y~x, se=FALSE) +
  theme_classic() +
  theme(strip.background = element_blank(),
        strip.text = element_text(size = 12),
        plot.title = element_text(size = 12, hjust = 0.5)) +
  labs(x = "", y = "Distance Travelled")

print(p)

ggsave("output/distance.png")
