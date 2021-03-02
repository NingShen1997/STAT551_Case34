library(tidyverse)
library(MASS)

# ==== Generate simulated data ====
set.seed(1)
n <- 600 ## simulated number of participants
dat <- data.frame(religion = factor(sample(c("Sikh","NonSikh"), n, replace = T)),
                  gender = factor(sample(c("Female","Male"), n, replace = T)),
                  accent = factor(sample(c("Present", "Absent"), n, replace = T)),
                  city = factor(sample(c("Richmond", "Surrey"), n, replace = T)))
dat$appoint_offer <- as.factor(sapply(0.5*(as.numeric(dat$religion)-1)+0.3*(as.numeric(dat$accent)-1), 
                       function(p){sample(c(0,1), 1, replace = T, prob = c(p, 1-p))}) +
                         sapply(0.5*(as.numeric(dat$religion)-1)+0.3*(as.numeric(dat$accent)-1), 
                                function(p){sample(c(0,1), 1, replace = T, prob = c(p, 1-p))}))

# ==== Exploratory analysis ====
# proportion of `appoint_offer` values within different religions
dat %>% 
  count(religion, appoint_offer) %>%
  group_by(religion) %>%
  mutate(freq = n / sum(n)) %>%
  ggplot(aes(religion, freq, fill = appoint_offer)) +
  geom_bar(stat = "identity",position = "stack", color="black", width = 0.5, alpha=0.8) +
  ylab("Proportion") + 
  ggsave("../Plots/Proportions_appoint_offer_religion.png", height = 3, width = 4.5)

# proportion of `appoint_offer` values within different genders
dat %>% 
  count(gender, appoint_offer) %>%
  group_by(gender) %>%
  mutate(freq = n / sum(n)) %>%
  ggplot(aes(gender, freq, fill = appoint_offer)) +
  geom_bar(stat = "identity",position = "stack", color="black", width = 0.5, alpha=0.8) +
  ylab("Proportion") +
  ggsave("../Plots/Proportions_appoint_offer_gender.png", height = 3, width = 4.5)

# proportion of `appoint_offer` values within different accents
dat %>% 
  count(accent, appoint_offer) %>%
  group_by(accent) %>%
  mutate(freq = n / sum(n)) %>%
  ggplot(aes(accent, freq, fill = appoint_offer)) +
  geom_bar(stat = "identity",position = "stack", color="black", width = 0.5, alpha=0.8) +
  ylab("Proportion") + 
  ggsave("../Plots/Proportions_appoint_offer_accent.png", height = 3, width = 4.5)

# proportion of `appoint_offer` values within different citys
dat %>% 
  count(city, appoint_offer) %>%
  group_by(city) %>%
  mutate(freq = n / sum(n)) %>%
  ggplot(aes(city, freq, fill = appoint_offer)) +
  geom_bar(stat = "identity",position = "stack", color="black", width = 0.5, alpha=0.8) +
  ylab("Proportion") + 
  ggsave("../Plots/Proportions_appoint_offer_city.png", height = 3, width = 4.5)

# ==== Apply ordinal regression ====
or.fit <- polr(appoint_offer ~ religion + gender + accent + city, data = dat, Hess=TRUE)

summary(or.fit)

round(coef(or.fit),3)





