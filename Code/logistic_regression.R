library(tidyverse)

# ==== Generate simulated data ====
set.seed(1)
n <- 600 ## simulated number of participants
dat <- data.frame(religion = factor(sample(c("Sikh","NonSikh"), n, replace = T)),
                  gender = factor(sample(c("Female","Male"), n, replace = T)),
                  accent = factor(sample(c("Present", "Absent"), n, replace = T)),
                  city = factor(sample(c("Richmond", "Surrey"), n, replace = T)))
dat$callback <- as.factor(sapply(0.3*(as.numeric(dat$religion)-1)+0.2*(as.numeric(dat$accent)-1), 
                                 function(p){sample(c(0,1), 1, replace = T, prob = c(p, 1-p))}))

# ==== Exploratory analysis ====
# proportion of `callback` values within different religions
dat %>% 
  count(religion, callback) %>%
  group_by(religion) %>%
  mutate(freq = n / sum(n)) %>%
  ggplot(aes(religion, freq, fill = callback)) +
  geom_bar(stat = "identity",position = "stack", color="black", width = 0.5, alpha=0.8) +
  ylab("Proportion") +
  ggsave("../Plots/Proportions_callback_religion.png", height = 3, width = 4.5)

# proportion of `callback` values within different genders
dat %>% 
  count(gender, callback) %>%
  group_by(gender) %>%
  mutate(freq = n / sum(n)) %>%
  ggplot(aes(gender, freq, fill = callback)) +
  geom_bar(stat = "identity",position = "stack", color="black", width = 0.5, alpha=0.8) +
  ylab("Proportion") +
  ggsave("../Plots/Proportions_callback_gender.png", height = 3, width = 4.5)

# proportion of `callback` values within different accents
dat %>% 
  count(accent, callback) %>%
  group_by(accent) %>%
  mutate(freq = n / sum(n)) %>%
  ggplot(aes(accent, freq, fill = callback)) +
  geom_bar(stat = "identity",position = "stack", color="black", width = 0.5, alpha=0.8) +
  ylab("Proportion") + 
  ggsave("../Plots/Proportions_callback_accent.png", height = 3, width = 4.5)

# proportion of `callback` values within different citys
dat %>% 
  count(city, callback) %>%
  group_by(city) %>%
  mutate(freq = n / sum(n)) %>%
  ggplot(aes(city, freq, fill = callback)) +
  geom_bar(stat = "identity",position = "stack", color="black", width = 0.5, alpha=0.8) +
  ylab("Proportion") +
  ggsave("../Plots/Proportions_callback_city.png", height = 3, width = 4.5)


# interaction plots between accent and gender
dat %>% # no interaction present
  count(accent, religion, callback) %>%
  group_by(accent, religion)  %>%
  mutate(freq = n / sum(n)) %>%
  filter(callback==1) %>%
  ggplot(aes(religion, freq, group = accent, color = accent, shape = accent)) +
  geom_point(size = 3) +
  geom_line(size = 1) +
  ylab("Proportion of `callback=1`") +
  ggsave("../Plots/Interaction_accent_religion.png", height = 3, width = 4.5)

# ==== Apply logistic regression ====
lr.fit <- glm(callback ~ religion + gender + accent + city, 
              data = dat, family=binomial("logit"))  ## Fit the parameters
summary(lr.fit)  ## Results summary
round(coef(lr.fit), 3)  ## Regression coefficients



# ====  simulate an interactive plot with interaction present ====

####  positive interaction bewteen accent and religion
set.seed(1000)
n <- 600 ## simulated number of participants
dat$callback <- as.factor(sapply(0.1*(as.numeric(dat$religion)-1) +
                                   0.1*(as.numeric(dat$accent)-1) + 
                                   0.4*(as.numeric(dat$religion)-1)*(as.numeric(dat$accent)-1) + 0.05,
                                 function(p){sample(c(0,1), 1, prob = c(p, 1-p))}))

dat %>% 
  count(accent, religion, callback) %>%
  group_by(accent, religion)  %>%
  mutate(freq = n / sum(n)) %>%
  filter(callback==1) %>%
  ggplot(aes(religion, freq, group = accent, color = accent, shape = accent)) +
  geom_point(size = 3) +
  geom_line(size = 1) +
  ylab("Proportion of `callback=1`") +
  ggsave("../Plots/Interaction_accent_religion_positive.png", height = 3, width = 4.5)

# proportion of `callback` values within different religions
dat %>% 
  count(religion, callback) %>%
  group_by(religion) %>%
  mutate(freq = n / sum(n)) %>%
  ggplot(aes(religion, freq, fill = callback)) +
  geom_bar(stat = "identity",position = "stack", color="black", width = 0.5, alpha=0.8) +
  ylab("Proportion") 

# proportion of `callback` values within different accents
dat %>% 
  count(accent, callback) %>%
  group_by(accent) %>%
  mutate(freq = n / sum(n)) %>%
  ggplot(aes(accent, freq, fill = callback)) +
  geom_bar(stat = "identity",position = "stack", color="black", width = 0.5, alpha=0.8) +
  ylab("Proportion") 


lr.fit <- glm(callback ~ religion + gender + accent + city + religion:accent, 
              data = dat, family=binomial("logit"))  ## Fit the parameters
summary(lr.fit)



####  negative interaction bewteen accenta and religion
set.seed(1000)
n <- 600 ## simulated number of participants
dat$callback <- as.factor(sapply(0.1*(as.numeric(dat$religion)-1) +
                                   0.1*(as.numeric(dat$accent)-1) - 
                                   0.4*(as.numeric(dat$religion)-1)*(as.numeric(dat$accent)-1) + 0.45,
                                 function(p){sample(c(0,1), 1, prob = c(p, 1-p))}))

dat %>% 
  count(accent, religion, callback) %>%
  group_by(accent, religion)  %>%
  mutate(freq = n / sum(n)) %>%
  filter(callback==1) %>%
  ggplot(aes(religion, freq, group = accent, color = accent, shape = accent)) +
  geom_point(size = 3) +
  geom_line(size = 1) +
  ylab("Proportion of `callback=1`") +
  ggsave("../Plots/Interaction_accent_religion_negative.png", height = 3, width = 4.5)

lr.fit <- glm(callback ~ religion + gender + accent + city + religion:accent, 
              data = dat, family=binomial("logit"))  ## Fit the parameters
summary(lr.fit)



# ==== sample size calculation ====
sample_size <- function(levels, p1, p2){ 
  alpha <- levels[1]
  power <- levels[2]
  2* (p1*(1-p1)+p2*(1-p2)) / ((p1-p2)^2) * (qnorm(alpha/2)+qnorm(1-power))^2 
}

power <- rep(seq(0.5,0.999,0.001), times = 3)
alpha <- rep(c(0.01,0.05,0.1), each = length(power/3))
N <- apply(data.frame(alpha, power), 1, sample_size, p1 = 0.6, p2 = 0.4)

res <- data.frame(alpha, power, N)
res %>%
  mutate(alpha = as.factor(alpha)) %>%
  ggplot(aes(N, power, color = alpha)) +
  geom_line() +
  xlab("Total Sample Size") +
  ylab("Statistical Power") +
  ggsave("../Plots/power_analysis.png", height = 4, width = 5)

