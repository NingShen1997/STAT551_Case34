library(tidyverse)

# ==== Generate simulated data ====
set.seed(1000)
n <- 600 ## simulated number of participants
dat <- data.frame(religion = factor(sample(c("Sikh","NonSikh"), n, replace = T)),
                  gender = factor(sample(c("Female","Male"), n, replace = T)),
                  accent = factor(sample(c("Present", "Absent"), n, replace = T)),
                  city = factor(sample(c("Richmond", "Surrey"), n, replace = T)))
dat$callback <- as.factor(sapply(0.3*(as.numeric(dat$religion)-1)+0.2*(as.numeric(dat$accent)-1)+0.1, 
                                 function(p){sample(c(0,1), 1, replace = T, prob = c(p, 1-p))}))


# ==== alternative regression on transformed callback rate ====
rate_callback <- dat %>% 
  group_by(religion, gender, accent, city) %>%
  summarise(rate = mean(as.numeric(as.character(callback))))

ar.fit <- lm(log(rate/(1-rate)) ~ religion + gender + accent + city, data = rate_callback)
summary(ar.fit)
