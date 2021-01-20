#loading libraries
library(tidyverse)
library(brms)
library(ggeffects)
library(here)

#### analysis on blinded data ####

#load blinded dataset
overall_data_blinded <- read_csv(here::here('overall_data_blinded.csv'),
                                 col_types = cols(date_withdrawn = col_datetime())) %>%
  mutate(random_has_coi = as.factor(random_has_coi),
         random_has_coi = fct_relevel(random_has_coi, c('FALSE', 'TRUE')),
         random_coi_shown = as.factor(random_coi_shown),
         random_coi_shown = fct_relevel(random_coi_shown, c('FALSE', 'TRUE'))) %>%
  mutate(pp_published = case_when(is.na(article_doi) ~ 'no',
                                  !is.na(article_doi) ~ 'yes'),
         pp_published = as.factor(pp_published),
         pp_published = fct_relevel(pp_published, c('no', 'yes')))


# set priors
priors <- c(set_prior("student_t(3, 0, 10)", "Intercept"),
            set_prior("student_t(3, 0, 2.5)", "b"),
            set_prior("cauchy(0, 2.5)", "sd"),
            set_prior("cauchy(0, 2.5)", "sigma"))

# basic model (not including provider level)
m1 <- brm(download ~ pp_published + random_coi_shown + random_has_coi + random_coi_shown * random_has_coi + (1|participant_id) + (1|guid),
          data = overall_data_blinded,
          family = bernoulli(link = 'logit'),
          warmup = 1500,
          iter = 3000,
          chains = 4,
          inits = '0',
          cores = 4,
          seed = 1)

# took over 24hrs to run and 'Error: vector memory exhausted (limit reached?)' when it actually tried to finish

# what is the distribution of number of pps viewed?
overall_data_blinded %>%
  group_by(participant_id) %>%
  tally() %>%
  ungroup() %>%
  rename(preprints_viewed = 'n') %>%
  group_by(preprints_viewed) %>%
  tally() %>%
  mutate(perc_sample = round(100 * n/sum(n),2))

# create dataset of only single pp viewers
single_viewers_blinded <- overall_data_blinded %>%
  group_by(participant_id) %>%
  mutate(pp_viewed = n()) %>%
  filter(pp_viewed == 1) %>%
  ungroup() %>%
  mutate(download_scaled = cum_downloads/100,
         download_scaled_mean = mean(download_scaled),
         download_centered = download_scaled - download_scaled_mean) %>%
  mutate(guid = as.factor(guid),
         pp_provider = as.factor(pp_provider))


# model without any random terms
m0a <- brm(download ~ pp_published + download_centered + random_coi_shown + random_has_coi + random_coi_shown * random_has_coi,
           data = single_viewers_blinded,
           family = bernoulli(link = 'logit'),
           warmup = 500,
           iter = 2000,
           chains = 2,
           inits = '0',
           cores = 4,
           seed = 5)

m0a_waic <- WAIC(m0a)

# model without random participant intercept, only using those who viewed only 1 pp & random intercept for guid
m1a <- brm(download ~ pp_published + download_centered + random_coi_shown + random_has_coi + random_coi_shown * random_has_coi + (1|guid),
           data = single_viewers_blinded,
           family = bernoulli(link = 'logit'),
           warmup = 500,
           iter = 2000,
           chains = 2,
           inits = '0',
           cores = 4,
           seed = 10,
           control=list(max_treedepth=13))

summary(m1a)
plot(m1a)
pairs(m1a)
m1a_waic <- WAIC(m1a)
pp_check(m1a)
pp_check(m1a, type = "stat", stat = 'median')

# include provider level intercepts (with guid nested within provider)
m2a <- brm(download ~ pp_published + download_centered + random_coi_shown + random_has_coi + random_coi_shown * random_has_coi + (1|pp_provider) + (1|guid),
           family = bernoulli(link = 'logit'),
           data = single_viewers_blinded,
           warmup = 1500,
           iter = 3000,
           chains = 2,
           inits = '0',
           cores = 4,
           seed = 20,
           control=list(max_treedepth=13))

summary(m2a)
plot(m2a)
pairs(m2a)
m2a_waic <- WAIC(m2a)
pp_check(m2a)
pp_check(m2a, type = "stat", stat = 'median')

# include guid level random close for affect of manipulation
m3a <- brm(download ~ pp_published + download_centered + random_coi_shown + random_has_coi + random_coi_shown * random_has_coi + 
             (1 | pp_provider) + (random_coi_shown | guid),
           data = single_viewers_blinded,
           family = bernoulli(link = 'logit'),
           warmup = 1500,
           iter = 3000,
           chains = 2,
           inits = '0',
           cores = 4,
           seed = 30)

summary(m3a)
plot(m3a)
pp_check(m3a)
m3a_waic <- WAIC(m3a)


# add in random slopes for interaction across providers
m4a <- brm(download ~ pp_published + download_centered + random_coi_shown + random_has_coi + random_coi_shown * random_has_coi + 
             (random_has_coi*random_coi_shown | pp_provider) + (random_coi_shown | guid),
           data = single_viewers_blinded,
           family = bernoulli(link = 'logit'),
           warmup = 1500,
           iter = 3000,
           chains = 2,
           inits = '0',
           cores = 4,
           seed = 40,
           control=list(max_treedepth=13))

summary(m4a)
plot(m4a)
pairs(m4a)
pp_check(m4a)
m4a_waic <- WAIC(m4a)


#### analyses on unblinded data ####

#load unblinded dataset
overall_data <- read_csv(here::here('overall_data.csv'),
                                 col_types = cols(date_withdrawn = col_datetime())) %>%
  mutate(has_coi = as.factor(has_coi),
         has_coi = fct_relevel(has_coi, c('FALSE', 'TRUE')),
         coi_shown = as.factor(coi_shown),
         coi_shown = fct_relevel(coi_shown, c('FALSE', 'TRUE'))) %>%
  mutate(pp_published = case_when(is.na(article_doi) ~ 'no',
                                  !is.na(article_doi) ~ 'yes'),
         pp_published = as.factor(pp_published),
         pp_published = fct_relevel(pp_published, c('no', 'yes')))

# create dataset of just those who viewed preprint
single_viewers <- overall_data %>%
  group_by(participant_id) %>%
  mutate(pp_viewed = n()) %>%
  filter(pp_viewed == 1) %>%
  ungroup() %>%
  mutate(download_scaled = cum_downloads/100,
         download_scaled_mean = mean(download_scaled),
         download_centered = download_scaled - download_scaled_mean) %>%
  mutate(guid = as.factor(guid),
         pp_provider = as.factor(pp_provider))

# initial model wiht only random intercept for pp
m1 <- brm(download ~ pp_published + download_centered + coi_shown + has_coi + coi_shown * has_coi + (1|guid),
           data = single_viewers,
           family = bernoulli(link = 'logit'),
           warmup = 500,
           iter = 2000,
           chains = 2,
           inits = '0',
           cores = 4,
           seed = 100)

summary(m1)
plot(m1)
pairs(m1)
m1_waic <- WAIC(m1)
pp_check(m1)
pp_check(m1, type = "stat", stat = 'median')
m1_predict <- ggpredict(m1, terms = c('coi_shown', 'has_coi'))
plot(m1_predict)

# model that adds random intercept for pp_provider
m2 <- brm(download ~ pp_published + download_centered + coi_shown + has_coi + coi_shown * has_coi + (1|guid) + (1|pp_provider),
          data = single_viewers,
          family = bernoulli(link = 'logit'),
          warmup = 1500,
          iter = 3000,
          chains = 2,
          inits = '0',
          cores = 4,
          seed = 200)

summary(m2)
plot(m2)
pairs(m2)
m2_waic <- WAIC(m2)
pp_check(m2)
pp_check(m2, type = "stat", stat = 'median')
m2_predict <- ggpredict(m2, terms = c('coi_shown', 'has_coi'))
plot(m2_predict)


# model that adds random slope for coi_shown to guid
m3 <- brm(download ~ pp_published + download_centered + coi_shown + has_coi + coi_shown * has_coi + (coi_shown|guid) + (1|pp_provider),
          data = single_viewers,
          family = bernoulli(link = 'logit'),
          warmup = 1500,
          iter = 3000,
          chains = 2,
          inits = '0',
          cores = 4,
          seed = 300,
          control=list(max_treedepth=11))

summary(m3)
plot(m3)
pairs(m3)
m3_waic <- WAIC(m3)
pp_check(m3)
pp_check(m3, type = "stat", stat = 'median')
m3_predict <- ggpredict(m3, terms = c('coi_shown', 'has_coi'))
plot(m3_predict)

# model that adds random slope for coi_shown*has_coi interaction to pp_provider
m4 <- brm(download ~ pp_published + download_centered + coi_shown + has_coi + coi_shown * has_coi + (coi_shown|guid) + (coi_shown * has_coi|pp_provider),
          data = single_viewers,
          family = bernoulli(link = 'logit'),
          warmup = 1500,
          iter = 3000,
          chains = 2,
          inits = '0',
          cores = 4,
          seed = 401,
          control=list(max_treedepth=12))

summary(m4)
plot(m4)
pairs(m4)
m3_waic <- WAIC(m4)
pp_check(m4)
pp_check(m4, type = "stat", stat = 'median')
