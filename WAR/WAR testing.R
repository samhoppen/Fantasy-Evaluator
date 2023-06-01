source('C:/Users/sphop/OneDrive/Betsperts/R/Fantasy-Evaluator/WAR/init_data.R')
source('C:/Users/sphop/OneDrive/Betsperts/R/Fantasy-Evaluator/WAR/meta_functions.R')
source('C:/Users/sphop/OneDrive/Betsperts/R/Fantasy-Evaluator/WAR/model_functions.R')
source('C:/Users/sphop/OneDrive/Betsperts/R/Fantasy-Evaluator/WAR/replacement_level_functions.R')
source('C:/Users/sphop/OneDrive/Betsperts/R/Fantasy-Evaluator/WAR/sim_functions.R')
source('C:/Users/sphop/OneDrive/Betsperts/R/Fantasy-Evaluator/WAR/split_stats_functions.R')
source('C:/Users/sphop/OneDrive/Betsperts/R/Fantasy-Evaluator/WAR/war_functions.R')

library(gt)
library(dplyr)
library(tidyverse)
library(RColorBrewer)
library(nflreadr)

#> Loading required package: magrittr
league_replacement_functions <- list("find_replacement_QB" = create_percentage_replacement_fn("Perc_Total_Plays", .5),
                                     #"find_replacement_QB" = create_league_replacement_fn(1, "QB", "Pass_Attempts"),
                                     "find_replacement_RB_rec" = create_league_replacement_fn(3, "RB", "Targets"), 
                                     "find_replacement_WR_rec" = create_league_replacement_fn(4, "WR", "Targets"),
                                     "find_replacement_TE_rec" = create_league_replacement_fn(2, "TE", "Targets"),
                                     "find_replacement_RB_rush" = create_league_replacement_fn(3, "RB",
                                                                                               "Rush_Attempts"),
                                     "find_replacement_WR_TE_rush" = create_league_replacement_fn(1, "WR",
                                                                                                  "Rush_Attempts",
                                                                                                  combine_wrte = 1))
# Create the expected points based modula formulas:
ep_model_formula_list <- list("air_formula" = as.formula(airEPA_Result ~ Home_Ind + Shotgun_Ind + No_Huddle_Ind + QBHit + 
                                                           Receiver_Position + PassLocation + Rush_EPA_Att +
                                                           (1|Passer_ID_Name) + (1|Receiver_ID_Name) + (1|DefensiveTeam)),
                              "yac_formula" = as.formula(yacEPA_Result ~ Home_Ind + Shotgun_Ind + No_Huddle_Ind + QBHit + 
                                                           AirYards*Receiver_Position + PassLocation + Rush_EPA_Att +
                                                           (1|Passer_ID_Name) + (1|Receiver_ID_Name) + (1|DefensiveTeam)),
                              "qb_rush_formula" = as.formula(EPA ~ Home_Ind + Shotgun_Ind + No_Huddle_Ind + Pass_EPA_Att +
                                                               (1|Rusher_ID_Name) + (1|DefensiveTeam)),
                              "main_rush_formula" = as.formula(EPA ~ Home_Ind + Shotgun_Ind + No_Huddle_Ind + 
                                                                 Rusher_Position + Pass_EPA_Att +
                                                                 (1|Team_Side_Gap) + (1|Rusher_ID_Name) + (1|DefensiveTeam)))

# Create the win probability based modula formulas:
wp_model_formula_list <- list("air_formula" = as.formula(airWPA_Result ~ Home_Ind + Shotgun_Ind + No_Huddle_Ind + QBHit + 
                                                           Receiver_Position + PassLocation + Rush_EPA_Att +
                                                           (1|Passer_ID_Name) + (1|Receiver_ID_Name) + (1|DefensiveTeam)),
                              "yac_formula" = as.formula(yacWPA_Result ~ Home_Ind + Shotgun_Ind + No_Huddle_Ind + QBHit + 
                                                           AirYards*Receiver_Position + PassLocation + Rush_EPA_Att +
                                                           (1|Passer_ID_Name) + (1|Receiver_ID_Name) + (1|DefensiveTeam)),
                              "qb_rush_formula" = as.formula(WPA ~ Home_Ind + Shotgun_Ind + No_Huddle_Ind + Pass_EPA_Att +
                                                               (1|Rusher_ID_Name) + (1|DefensiveTeam)),
                              "main_rush_formula" = as.formula(WPA ~ Home_Ind + Shotgun_Ind + No_Huddle_Ind + 
                                                                 Rusher_Position + Pass_EPA_Att +
                                                                 (1|Team_Side_Gap) + (1|Rusher_ID_Name) + (1|DefensiveTeam)))

# pbp <- nflfastR::load_pbp(2022)
nflreadr::.clear_cache()
season_results_wpa <- get_pbp_data(2021:2022) %>%
  dplyr::filter(week <= 18) %>%
  add_positions(2021:2022) %>%
  add_model_variables() %>%
  prepare_model_data() %>%
  add_position_tables() %>%
  join_position_statistics() %>%
  find_positional_replacement_level(league_replacement_functions) %>%
  estimate_player_value_added(wp_model_formula_list) %>%
  calculate_above_replacement() %>%
  convert_prob_to_wins()

.clear_cache()
season_results_epa <- get_pbp_data(2021:2022) %>% 
  filter(week <= 18) %>%
  add_positions(2021:2022) %>%
  add_model_variables() %>%
  prepare_model_data() %>%
  add_position_tables() %>%
  find_positional_replacement_level(league_replacement_functions) %>%
  estimate_player_value_added(ep_model_formula_list) %>%
  calculate_above_replacement() %>%
  convert_points_to_wins(calculate_points_per_win(2021:2022))

rosters <- nflreadr::load_rosters(2022) %>% 
  select(gsis_id, full_name, team, headshot_url) %>% 
  unique()

# qb_war <- season_results_wpa$QB_table %>% 
#   filter(Pass_Attempts >= 200) %>% 
#   select(c(Player_ID_Name, yac_WAR, rush_WAR, total_WAR)) %>% 
#   left_join(season_results_epa$QB_table %>% 
#               select(c(Player_ID_Name, yac_WAR, rush_WAR, total_WAR)),
#             by = c("Player_ID_Name")) %>% 
#   mutate(gsis_id = paste0("00-", gsub(".*-", "", Player_ID_Name))) %>% 
#   left_join(rosters) %>% 
#   mutate(yac_WAR = (yac_WAR.x+yac_WAR.y)/2,
#          rush_WAR = (rush_WAR.x+rush_WAR.y)/2,
#          total_WAR = (total_WAR.x+total_WAR.y)/2)

qb_war <- season_results_wpa$QB_table %>% 
  filter(Pass_Attempts >= 250) %>%
  select(c(Player_ID_Name, yac_WAR, rush_WAR, total_WAR)) %>% 
  mutate(gsis_id = paste0("00-", gsub(".*-", "", Player_ID_Name))) %>% 
  # left_join(rosters) %>% 
  arrange(desc(total_WAR))


qb_war <- season_results_epa$QB_table %>% 
  filter(Pass_Attempts >= 250) %>%
  select(c(Player_ID_Name, yac_WAR, rush_WAR, total_WAR)) %>% 
  mutate(gsis_id = paste0("00-", gsub(".*-", "", Player_ID_Name))) %>% 
  left_join(rosters) %>% 
  arrange(desc(total_WAR))

qb_table <- qb_war %>% 
  select(headshot_url, full_name, team, yac_WAR, rush_WAR, total_WAR) %>% 
  gt() %>% 
  tab_header(title = md(paste0("**Top QBs in Wins Above Replacement (WAR) in 2022**")),
             subtitle = paste0("Minimum 200 pass attempts | Reg szn only")) %>% 
  cols_align(align = "center",
             columns = c(team, yac_WAR, rush_WAR, total_WAR)) %>%
  cols_label(headshot_url = "Player",
             full_name = "",
             team = "Team",
             yac_WAR = "Pass WAR",
             rush_WAR = "Rush WAR",
             total_WAR = "Total WAR") %>% 
  fmt_number(columns = c(yac_WAR, rush_WAR, total_WAR), decimals = 2) %>% 
  cols_width(c(yac_WAR, rush_WAR, total_WAR) ~ px(110)) %>%
  # cols_width(c() ~ px(225)) %>% 
  # cols_hide(columns = c(wopr_rank)) %>% 
  tab_style(
    style = list(
      cell_borders(
        sides = "bottom",
        color = "black",
        weight = px(3)
      )
    ),
    locations = list(
      cells_column_labels(
        columns = gt::everything()
      )
    )
  ) %>% 
  tab_style(
    style = list(
      cell_borders(
        sides = "left",
        color = "black",
        weight = px(3)
      )
    ),
    locations = list(
      cells_body(
        columns = c(yac_WAR)
      )
    )
  ) %>%
  data_color(
    columns = c(yac_WAR),
    colors = scales::col_numeric(
      paletteer::paletteer_d(
        palette = "RColorBrewer::PRGn"
      ) %>% as.character(),
      domain = NULL
    ),
    alpha = 0.85
  ) %>%
  data_color(
    columns = c(rush_WAR),
    colors = scales::col_numeric(
      paletteer::paletteer_d(
        palette = "RColorBrewer::PRGn"
      ) %>% as.character(),
      domain = NULL
    ),
    alpha = 0.85
  ) %>%
  data_color(
    columns = c(total_WAR),
    colors = scales::col_numeric(
      paletteer::paletteer_d(
        palette = "RColorBrewer::PRGn"
      ) %>% as.character(),
      domain = NULL
    ),
    alpha = 0.85
  ) %>%
  text_transform(locations = cells_body(c(headshot_url)),
                 fn = function(x) web_image(url = paste0(x))) %>% 
  text_transform(locations = cells_body(c(team)),
                 fn = function(x) web_image(url = paste0('https://a.espncdn.com/i/teamlogos/nfl/500/',x,'.png')))%>% 
  tab_source_note(
    source_note = "Table: @SamHoppen | Data: @nflfastR"
  )

gt::gtsave(qb_table, file = file.path("C:/Users/sphop/OneDrive/Betsperts/R/Charts/QB WAR 2022.png"))



rb_war <- season_results_epa$RB_table %>% 
  filter(Receptions+Targets>= 50) %>% 
  select(c(Player_ID_Name, air_WAR, yac_WAR, rush_WAR, total_WAR)) %>% 
  left_join(season_results_epa$RB_table %>% 
              select(c(Player_ID_Name, air_WAR, yac_WAR, rush_WAR, total_WAR)),
            by = c("Player_ID_Name")) %>% 
  mutate(gsis_id = paste0("00-", gsub(".*-", "", Player_ID_Name))) %>% 
  left_join(rosters) %>% 
  mutate(air_WAR = (air_WAR.x+air_WAR.y)/2,
         yac_WAR = (yac_WAR.x+yac_WAR.y)/2,
         rec_WAR = air_WAR + yac_WAR,
         rush_WAR = (rush_WAR.x+rush_WAR.y)/2,
         total_WAR = (total_WAR.x+total_WAR.y)/2) %>% 
  select(headshot_url, full_name, team, rush_WAR, rec_WAR, total_WAR)

rb_war <- season_results_epa$RB_table %>% 
  filter(Receptions+Targets>= 50) %>% 
  select(c(Player_ID_Name, air_WAR, yac_WAR, rush_WAR, total_WAR)) %>% 
  # left_join(season_results_epa$RB_table %>% 
  #             select(c(Player_ID_Name, air_WAR, yac_WAR, rush_WAR, total_WAR)),
  #           by = c("Player_ID_Name")) %>% 
  mutate(gsis_id = paste0("00-", gsub(".*-", "", Player_ID_Name))) %>% 
  left_join(rosters) %>% 
  mutate(rec_WAR = air_WAR + yac_WAR) %>% 
  # mutate(air_WAR = (air_WAR.x+air_WAR.y)/2,
  #        yac_WAR = (yac_WAR.x+yac_WAR.y)/2,
  #        rec_WAR = air_WAR + yac_WAR,
  #        rush_WAR = (rush_WAR.x+rush_WAR.y)/2,
  #        total_WAR = (total_WAR.x+total_WAR.y)/2) %>% 
  select(headshot_url, full_name, team, rush_WAR, rec_WAR, total_WAR)


rb_table <- rb_war %>% 
  arrange(desc(total_WAR)) %>% 
  gt() %>% 
  tab_header(title = md(paste0("**Top RBs in Wins Above Replacement (WAR) in 2022**")),
             subtitle = paste0("Minimum 50 opportunities | Reg szn only")) %>% 
  cols_align(align = "center",
             columns = c(team, rec_WAR, rush_WAR, total_WAR)) %>%
  cols_label(headshot_url = "Player",
             full_name = "",
             team = "Team",
             rec_WAR = "Rec WAR",
             rush_WAR = "Rush WAR",
             total_WAR = "Total WAR") %>% 
  fmt_number(columns = c(rec_WAR, rush_WAR, total_WAR), decimals = 2) %>% 
  cols_width(c(rec_WAR, rush_WAR, total_WAR) ~ px(110)) %>%
  # cols_width(c() ~ px(225)) %>% 
  # cols_hide(columns = c(wopr_rank)) %>% 
  tab_style(
    style = list(
      cell_borders(
        sides = "bottom",
        color = "black",
        weight = px(3)
      )
    ),
    locations = list(
      cells_column_labels(
        columns = gt::everything()
      )
    )
  ) %>% 
  tab_style(
    style = list(
      cell_borders(
        sides = "left",
        color = "black",
        weight = px(3)
      )
    ),
    locations = list(
      cells_body(
        columns = c(rush_WAR)
      )
    )
  ) %>%
  data_color(
    columns = c(rec_WAR),
    colors = scales::col_numeric(
      paletteer::paletteer_d(
        palette = "RColorBrewer::PRGn"
      ) %>% as.character(),
      domain = NULL
    ),
    alpha = 0.85
  ) %>%
  data_color(
    columns = c(rush_WAR),
    colors = scales::col_numeric(
      paletteer::paletteer_d(
        palette = "RColorBrewer::PRGn"
      ) %>% as.character(),
      domain = NULL
    ),
    alpha = 0.85
  ) %>%
  data_color(
    columns = c(total_WAR),
    colors = scales::col_numeric(
      paletteer::paletteer_d(
        palette = "RColorBrewer::PRGn"
      ) %>% as.character(),
      domain = NULL
    ),
    alpha = 0.85
  ) %>%
  text_transform(locations = cells_body(c(headshot_url)),
                 fn = function(x) web_image(url = paste0(x))) %>% 
  text_transform(locations = cells_body(c(team)),
                 fn = function(x) web_image(url = paste0('https://a.espncdn.com/i/teamlogos/nfl/500/',x,'.png')))%>% 
  tab_source_note(
    source_note = "Table: @SamHoppen | Data: @nflfastR"
  )

gt::gtsave(rb_table, file = file.path("C:/Users/sphop/OneDrive/Betsperts/R/Charts/RB WAR 2022.png"))


wr_war <- season_results_epa$WR_table %>% 
  filter(Targets >= 75) %>% 
  select(c(Player_ID_Name, air_WAR, yac_WAR, total_WAR)) %>% 
  left_join(season_results_epa$WR_table %>% 
              select(c(Player_ID_Name, air_WAR, yac_WAR, total_WAR)),
            by = c("Player_ID_Name")) %>% 
  mutate(gsis_id = paste0("00-", gsub(".*-", "", Player_ID_Name))) %>% 
  left_join(rosters) %>% 
  mutate(air_WAR = (air_WAR.x+air_WAR.y)/2,
         yac_WAR = (yac_WAR.x+yac_WAR.y)/2,
         total_WAR = (total_WAR.x+total_WAR.y)/2) %>% 
  select(headshot_url, full_name, team, air_WAR, yac_WAR, total_WAR)

wr_war <- season_results_epa$WR_table %>% 
  filter(Targets >= 75) %>% 
  select(c(Player_ID_Name, air_WAR, yac_WAR, total_WAR)) %>% 
  # left_join(season_results_epa$WR_table %>% 
  #             select(c(Player_ID_Name, air_WAR, yac_WAR, total_WAR)),
  #           by = c("Player_ID_Name")) %>% 
  mutate(gsis_id = paste0("00-", gsub(".*-", "", Player_ID_Name))) %>% 
  left_join(rosters) %>% 
  # mutate(air_WAR = (air_WAR.x+air_WAR.y)/2,
  #        yac_WAR = (yac_WAR.x+yac_WAR.y)/2,
  #        total_WAR = (total_WAR.x+total_WAR.y)/2) %>% 
  select(headshot_url, full_name, team, air_WAR, yac_WAR, total_WAR)



wr_table <- wr_war %>% 
  arrange(desc(total_WAR)) %>% 
  gt() %>% 
  tab_header(title = md(paste0("**Top WRs in Wins Above Replacement (WAR) in 2022**")),
             subtitle = paste0("Minimum 75 targets | Reg szn only")) %>% 
  cols_align(align = "center",
             columns = c(team, air_WAR, yac_WAR, total_WAR)) %>%
  cols_label(headshot_url = "Player",
             full_name = "",
             team = "Team",
             air_WAR = "Air Yards WAR",
             yac_WAR = "YAC WAR",
             total_WAR = "Total WAR") %>% 
  fmt_number(columns = c(air_WAR, yac_WAR, total_WAR), decimals = 2) %>% 
  cols_width(c(air_WAR, yac_WAR, total_WAR) ~ px(110)) %>%
  # cols_width(c() ~ px(225)) %>% 
  # cols_hide(columns = c(wopr_rank)) %>% 
  tab_style(
    style = list(
      cell_borders(
        sides = "bottom",
        color = "black",
        weight = px(3)
      )
    ),
    locations = list(
      cells_column_labels(
        columns = gt::everything()
      )
    )
  ) %>% 
  tab_style(
    style = list(
      cell_borders(
        sides = "left",
        color = "black",
        weight = px(3)
      )
    ),
    locations = list(
      cells_body(
        columns = c(air_WAR)
      )
    )
  ) %>%
  data_color(
    columns = c(air_WAR),
    colors = scales::col_numeric(
      paletteer::paletteer_d(
        palette = "RColorBrewer::PRGn"
      ) %>% as.character(),
      domain = NULL
    ),
    alpha = 0.85
  ) %>%
  data_color(
    columns = c(yac_WAR),
    colors = scales::col_numeric(
      paletteer::paletteer_d(
        palette = "RColorBrewer::PRGn"
      ) %>% as.character(),
      domain = NULL
    ),
    alpha = 0.85
  ) %>%
  data_color(
    columns = c(total_WAR),
    colors = scales::col_numeric(
      paletteer::paletteer_d(
        palette = "RColorBrewer::PRGn"
      ) %>% as.character(),
      domain = NULL
    ),
    alpha = 0.85
  ) %>%
  text_transform(locations = cells_body(c(headshot_url)),
                 fn = function(x) web_image(url = paste0(x))) %>% 
  text_transform(locations = cells_body(c(team)),
                 fn = function(x) web_image(url = paste0('https://a.espncdn.com/i/teamlogos/nfl/500/',x,'.png')))%>% 
  tab_source_note(
    source_note = "Table: @SamHoppen | Data: @nflfastR"
  )

gt::gtsave(wr_table, file = file.path("C:/Users/sphop/OneDrive/Betsperts/R/Charts/WR WAR 2022.png"))


te_war <- season_results_epa$TE_table %>% 
  filter(Targets >= 40) %>% 
  select(c(Player_ID_Name, air_WAR, yac_WAR, total_WAR)) %>% 
  left_join(season_results_epa$TE_table %>% 
              select(c(Player_ID_Name, air_WAR, yac_WAR, total_WAR)),
            by = c("Player_ID_Name")) %>% 
  mutate(gsis_id = paste0("00-", gsub(".*-", "", Player_ID_Name))) %>% 
  left_join(rosters) %>% 
  mutate(air_WAR = (air_WAR.x+air_WAR.y)/2,
         yac_WAR = (yac_WAR.x+yac_WAR.y)/2,
         total_WAR = (total_WAR.x+total_WAR.y)/2) %>% 
  select(headshot_url, full_name, team, air_WAR, yac_WAR, total_WAR)

te_war <- season_results_epa$TE_table %>% 
  filter(Targets >= 40) %>% 
  select(c(Player_ID_Name, air_WAR, yac_WAR, total_WAR)) %>% 
  # left_join(season_results_epa$TE_table %>% 
  #             select(c(Player_ID_Name, air_WAR, yac_WAR, total_WAR)),
  #           by = c("Player_ID_Name")) %>% 
  mutate(gsis_id = paste0("00-", gsub(".*-", "", Player_ID_Name))) %>% 
  left_join(rosters) %>% 
  # mutate(air_WAR = (air_WAR.x+air_WAR.y)/2,
  #        yac_WAR = (yac_WAR.x+yac_WAR.y)/2,
  #        total_WAR = (total_WAR.x+total_WAR.y)/2) %>% 
  select(headshot_url, full_name, team, air_WAR, yac_WAR, total_WAR)


te_table <- te_war %>% 
  arrange(desc(total_WAR)) %>% 
  gt() %>% 
  tab_header(title = md(paste0("**Top TEs in Wins Above Replacement (WAR) in 2022**")),
             subtitle = paste0("Minimum 40 targets | Reg szn only")) %>% 
  cols_align(align = "center",
             columns = c(team, air_WAR, yac_WAR, total_WAR)) %>%
  cols_label(headshot_url = "Player",
             full_name = "",
             team = "Team",
             air_WAR = "Air Yards WAR",
             yac_WAR = "YAC WAR",
             total_WAR = "Total WAR") %>% 
  fmt_number(columns = c(air_WAR, yac_WAR, total_WAR), decimals = 2) %>% 
  cols_width(c(air_WAR, yac_WAR, total_WAR) ~ px(110)) %>%
  # cols_width(c() ~ px(225)) %>% 
  # cols_hide(columns = c(wopr_rank)) %>% 
  tab_style(
    style = list(
      cell_borders(
        sides = "bottom",
        color = "black",
        weight = px(3)
      )
    ),
    locations = list(
      cells_column_labels(
        columns = gt::everything()
      )
    )
  ) %>% 
  tab_style(
    style = list(
      cell_borders(
        sides = "left",
        color = "black",
        weight = px(3)
      )
    ),
    locations = list(
      cells_body(
        columns = c(air_WAR)
      )
    )
  ) %>%
  data_color(
    columns = c(air_WAR),
    colors = scales::col_numeric(
      paletteer::paletteer_d(
        palette = "RColorBrewer::PRGn"
      ) %>% as.character(),
      domain = NULL
    ),
    alpha = 0.85
  ) %>%
  data_color(
    columns = c(yac_WAR),
    colors = scales::col_numeric(
      paletteer::paletteer_d(
        palette = "RColorBrewer::PRGn"
      ) %>% as.character(),
      domain = NULL
    ),
    alpha = 0.85
  ) %>%
  data_color(
    columns = c(total_WAR),
    colors = scales::col_numeric(
      paletteer::paletteer_d(
        palette = "RColorBrewer::PRGn"
      ) %>% as.character(),
      domain = NULL
    ),
    alpha = 0.85
  ) %>%
  text_transform(locations = cells_body(c(headshot_url)),
                 fn = function(x) web_image(url = paste0(x))) %>% 
  text_transform(locations = cells_body(c(team)),
                 fn = function(x) web_image(url = paste0('https://a.espncdn.com/i/teamlogos/nfl/500/',x,'.png')))%>% 
  tab_source_note(
    source_note = "Table: @SamHoppen | Data: @nflfastR"
  )

gt::gtsave(te_table, file = file.path("C:/Users/sphop/OneDrive/Betsperts/R/Charts/TE WAR 2022.png"))
