# loading libraries
library(tidyverse)
library(osfr)
library(here)
library(lubridate)

Sys.setenv(TZ='UTC')

database_data <- read_csv(here::here('/raw_database_output.csv'))

# export csv of pp to include in study
database_data %>%
  select(guid, date_published) %>%
  distinct(guid, .keep_all = T) %>%
  write_csv('eligible_preprints.csv')
