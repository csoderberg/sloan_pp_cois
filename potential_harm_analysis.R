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

