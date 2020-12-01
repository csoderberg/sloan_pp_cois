# loading libraries
library(tidyverse)
library(here)

# read in data files

## view log files
views_sloan_ids <- read_csv(here::here('views_by_second_results.csv'))
views_user_ids <- read_csv(here::here('views_bysecond_nosloanid.csv'))
views_user_sloan_ids <- read_csv(here::here('viewer_user_guids.csv'))

# download log files
downloads_sloan_ids <- read_csv(here::here('downloads_by_second_results.csv'))
downloads_user_ids <- read_csv(here::here('downloads_bysecond_nosloanid.csv'))
downloads_user_sloan_ids <- read_csv(here::here('downloader_user_guids.csv'))

# database preprint info files
preprint_contributors <- read_csv(here::here('preprint_contributors.csv'))
preprint_info <- read_csv(here::here('/raw_database_output.csv'))

# overall downloads by second
downloads_by_second <- read_csv(here::here('overall_downloads_by_second_results.csv'))


# create clean file of earlierst view by each participant for each preprint
views_user_ids <- views_user_ids %>%
  filter(view_count > 0) %>%
  mutate(sloan_id = NA) %>%
  rename(view_date = 'date',
         user_guid = 'user_id') %>%
  select(guid, sloan_id, coi_shown, view_date, view_count, user_guid)


views_by_participants <- views_sloan_ids %>%
  filter(view_count > 0) %>%
  rename(view_date = 'date') %>%
  left_join(views_user_sloan_ids, by = 'sloan_id') %>%
  rename(user_guid = 'user_id') %>%
  rbind(views_user_ids) %>%
  mutate(participant_id = case_when(is.na(user_guid) ~ sloan_id,
                                    !is.na(user_guid) ~ user_guid)) %>%
  group_by(guid, participant_id, coi_shown) %>%
  summarize(earliest_view = min(view_date)) %>%
  ungroup()

# create clean file of earlierst download by each participant for each preprint
downloads_user_ids <- downloads_user_ids %>%
  filter(download_count > 0) %>%
  mutate(sloan_id = NA) %>%
  rename(download_date = 'date',
         user_guid = 'user_id') %>%
  select(guid, sloan_id, coi_shown, download_date, download_count, user_guid)

downloads_by_participants <- downloads_sloan_ids %>%
  filter(download_count > 0) %>%
  rename(download_date = 'date') %>%
  left_join(downloads_user_sloan_ids, by = 'sloan_id') %>%
  rename(user_guid = 'user_id') %>%
  rbind(downloads_user_ids) %>%
  mutate(participant_id = case_when(is.na(user_guid) ~ sloan_id,
                                    !is.na(user_guid) ~ user_guid)) %>%
  group_by(guid, participant_id, coi_shown) %>%
  summarize(earliest_download = min(download_date)) %>%
  ungroup()


# create one file with views and downloads
all_metrics <- full_join(views_by_participants, downloads_by_participants, by = c('guid', 'participant_id', 'coi_shown')) %>%
  mutate(download_after_view = case_when(earliest_download < earliest_view | is.na(earliest_view) ~ FALSE,
                                         earliest_download >= earliest_view ~ TRUE,
                                         TRUE ~ NA)) %>%
  mutate(download = case_when(is.na(earliest_download) ~ 0,
                              !is.na(earliest_download) ~ 1))

# exclude data based on exclusion rules
id_contributors <- all_metrics %>%
  filter(is.na(download_after_view) | download_after_view == TRUE) %>%  # only keep views that happened before or without downloads
  left_join(preprint_contributors, by = c('guid' = 'preprint_guid')) %>%
  filter(participant_id == user_guid) %>%
  mutate(contributor = 1) %>%
  distinct() %>%
  select(guid, participant_id, contributor)

retained_metrics <- all_metrics %>%
  filter(is.na(download_after_view) | download_after_view == TRUE) %>%  # only keep views that happened before or without downloads
  left_join(id_contributors, by = c('guid', 'participant_id')) %>% 
  filter(is.na(contributor)) %>% # get rid of views/downloads by contributors
  filter(participant_id != 'hsey5' & participant_id != 'cdi38' & participant_id != 'alh38' & participant_id != 'acfhe')  %>% # remove actions by study authors                     
  select(-contributor)


# how many people ended up in more than 1 visibility condition?
retained_metrics %>% 
  group_by(guid, participant_id) %>% 
  tally() %>% 
  filter(n > 1)

# only keep first view (and download that matches that condition, if it happened)
retained_metrics <- retained_metrics %>% 
  group_by(guid, participant_id) %>%
  filter(earliest_view == min(earliest_view))


# identify changed statements pps
statement_states <- preprint_info %>% 
  group_by(guid) %>% 
  tally()

changed_statements <- preprint_info %>% 
  filter(log_date < '2020-05-11 20:37:53' | is.na(log_date)) %>%
  left_join(statement_states, by = 'guid') %>% 
  filter(n > 1) %>%
  mutate(statement_changed = case_when(log_date > date_published ~ 'after_pub',
                                       log_date <= date_published ~ 'before_pub'),
         coi_statement_state = case_when(is.na(log_date) & log_action == 'has_coi_updated' ~ has_coi,
                                         !is.na(log_date) & log_action == 'has_coi_updated' ~ as.logical(log_value))) %>%
  dplyr::select(-c('n')) %>%
  distinct()


#number who changed coi statements before & after publishing
changed_statements %>%
  group_by(guid, statement_changed) %>%
  tally()

View(changed_statements %>%
       group_by(guid, statement_changed) %>%
       mutate(last_change = max(log_date)) %>%
       ungroup() %>%
       filter(log_date >= last_change) %>%
       group_by(guid) %>%
       mutate(num_changes = sum(!is.na(log_action))) %>%
       filter(num_changes > 1))


# only 4 preprint changed their coi after publishing and of those 3 have no views before the change, and the other has 3 views before the change, so gettign rid of duplicate rows
eligible_pps <- preprint_info %>% 
  mutate(max_log_date = date_published + days(14)) %>%
  filter(is.na(log_date) |
           log_date <= max_log_date) %>%
  mutate(statement_changed = case_when(log_date > date_published ~ 'after_pub',
                                       log_date <= date_published ~ 'before_pub'),
         coi_statement_state = case_when(is.na(log_date) ~ has_coi,
                                         !is.na(log_date) ~ log_value)) %>%
  group_by(guid, statement_changed) %>%
  mutate(last_change = max(log_date)) %>%
  ungroup() %>%
  filter(is.na(last_change) | 
           statement_changed == 'after_pub' | 
           (statement_changed == 'before_pub' & last_change == log_date)) %>%
  dplyr::select(-c('log_value', 'log_action', 'has_coi')) %>%
  rename(has_coi = 'coi_statement_state') %>%
  group_by(guid) %>%
  arrange(desc(log_date)) %>%
  slice(1L) %>%
  ungroup()

#create overall dataset
overall_data <- retained_metrics %>%
  full_join(eligible_pps, by = 'guid') %>%
  select(-c(pp_num)) %>%
  filter(!is.na(date_published) & !is.na(participant_id))


# create cumulative downloads for all view times
downloads_by_second <- downloads_by_second %>%
  full_join(overall_data %>% select(guid, earliest_view), 
            by = c('guid', 'date' = 'earliest_view')) %>%
  mutate(download_count = case_when(!is.na(download_count) ~ download_count,
                                    is.na(download_count) ~ 0)) %>%
  group_by(guid) %>%
  arrange(date) %>%
  mutate(cum_downloads = cumsum(download_count)) %>%
  select(guid, date, cum_downloads)

overall_data <- overall_data %>% 
  left_join(downloads_by_second, by = c('guid', 'earliest_view' = 'date'))

write_csv(overall_data, 'overall_data.csv')
