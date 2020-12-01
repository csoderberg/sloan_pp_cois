#loading libraries
library(tidyverse)
library(brms)
library(ggeffects)

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

