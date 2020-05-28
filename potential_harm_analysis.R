# loading libraries
library(tidyverse)
library(osfr)
library(here)

# read in datafiles
osf_retrieve_node('y87ah') %>%
  osf_ls_files(path = 'potential_harm_assesment') %>%
  osf_download()

complete_pps_data <- read_csv(here::here('counts_completed_pps.csv'))
pp_action_data <- read_csv(here::here('counts_workflowsteps_unfinished_pps.csv'))

# difference in percent of pps, by service, that were unfinished during - pre experiment
complete_pps_data %>%
  group_by(provider, timeframe) %>%
  summarize(perc_unfinished = sum(num_pps[pp_submit_finished == 'not_finished']/sum(num_pps)), total = sum(num_pps), unfinished = sum(num_pps[pp_submit_finished == 'not_finished'])) %>%
  group_by(provider) %>%
  summarize(perc_diff = perc_unfinished[timeframe == 'during_exp'] - perc_unfinished[timeframe == 'pre_exp'], raw_diff = unfinished[timeframe == 'during_exp'] - unfinished[timeframe == 'pre_exp'])

# difference in number of submissions to services before/after start of experiment
complete_pps_data %>%
  group_by(provider, timeframe) %>%
  summarize(submissions = sum(num_pps)) %>%
  pivot_wider(names_from = timeframe, values_from = submissions) %>%
  mutate(during_pre_diff = during_exp - pre_exp)
  
