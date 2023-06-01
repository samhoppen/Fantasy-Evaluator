library(tidyverse)
library(dplyr)
library(nflfastR)
library(nflreadr)

rosters <- nflreadr::load_rosters(seasons = 2022) %>% 
  filter(!is.na(gsis_id)) %>%
  select(gsis_id, position = depth_chart_position) %>% 
  unique()

pbp <- nflfastR::load_pbp(2022)

participation <- nflreadr::load_participation(2022) %>% 
  select(-players_on_play) %>% 
  tidyr::separate_rows(offense_players, sep = ";") %>% 
  left_join(rosters,
            by = c("offense_players" = "gsis_id")) %>% 
  filter(!is.na(offense_formation))

new_participation <- participation %>% 
  # select(-c(offense_personnel, defenders_in_box, 
  #           defense_personnel, number_of_pass_rushers,
  #           defense_players, n_offense, n_defense)) %>% 
  arrange(nflverse_game_id, play_id, position) %>% 
  group_by(nflverse_game_id, play_id, position) %>% 
  mutate(pos_num = row_number()) %>% 
  ungroup() %>% 
  mutate(pos_id = paste0(position,"_",pos_num,"_id")) %>% 
  arrange(position, pos_num) %>% 
  select(-pos_num, -position) %>% 
  pivot_wider(names_from = pos_id, values_from = offense_players) %>% 
  arrange(nflverse_game_id, play_id)
