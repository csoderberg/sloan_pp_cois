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
