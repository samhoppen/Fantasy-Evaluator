library(tidyverse)
library(dplyr)
library(nflfastR)
library(nflreadr)
library(RColorBrewer)
library(ggtext)
library(nflplotR)
library(gt)

rosters <- load_rosters(seasons = 2022) %>% 
  select(gsis_id, headshot_url, full_name) %>% 
  unique()

pbp <- nflfastR::load_pbp(2022) %>% 
  nflfastR::add_xpass()

ints <- pbp %>% 
  filter(pass == 1 | rush == 1) %>% 
  filter(play_type != "no_play") %>% 
  filter(!is.na(passer_id)) %>% 
  group_by(passer_id, posteam) %>% 
  summarize(dbs = n(),
            must_pass_3rd = sum(interception[down == 3 & xpass >= 0.75 & half_seconds_remaining >= 120], na.rm = T),
            must_pass_3rd_epa = round(sum(epa[interception == 1 & down == 3 & xpass >= 0.75], na.rm = T), 3),
            last_2_min = sum(interception[qtr == 2 & half_seconds_remaining <= 120], na.rm = T),
            last_2_min_epa = round(sum(epa[interception == 1 & qtr == 2 & half_seconds_remaining <= 120], na.rm = T), 3),
            long_2nd = sum(interception[down == 2 & ydstogo >= 15], na.rm = T),
            long_2nd_epa = round(sum(epa[interception == 1 & down == 2 & ydstogo >= 15]), 3),
            first_down = sum(interception[down == 1 & ydstogo == 10], na.rm = T),
            first_down_epa = round(sum(epa[interception == 1 & down == 1 & ydstogo == 10], na.rm = T), 3),
            total_ints = sum(interception, na.rm = T),
            total_ints_epa = round(sum(epa[interception == 1], na.rm = T), 3),
            rz_ints = sum(interception[yardline_100 <= 20], na.rm = T),
            rz_int_epa = round(sum(epa[interception == 1 & yardline_100 <= 20], na.rm = T), 3)) %>% 
  filter(dbs >= 300) %>% 
  # filter(total_ints >= 5) %>% 
  ungroup() %>% 
  left_join(rosters,
            by = c("passer_id" = "gsis_id")) %>% 
  select(c(headshot_url, full_name, posteam,
           total_ints, total_ints_epa, must_pass_3rd, must_pass_3rd_epa,
           last_2_min, last_2_min_epa, long_2nd, long_2nd_epa, first_down, first_down_epa,
           rz_ints, rz_int_epa)) %>% 
  arrange(desc(total_ints))
  


int_table <- ints %>% 
  gt() %>% 
  tab_header(title = md(paste0("**Breaking down when each quarterback's interceptions occurred**")),
             subtitle = paste0("Minimum 300 dropbacks | Postseason included")) %>% 
  cols_align(align = "center",
             columns = c(posteam, total_ints, total_ints_epa, must_pass_3rd, must_pass_3rd_epa,
                         last_2_min, last_2_min_epa, long_2nd, long_2nd_epa, first_down, first_down_epa,
                         rz_ints, rz_int_epa)) %>%
  cols_label(headshot_url = "Player",
             full_name = "",
             posteam = "Team",
             total_ints = "#",
             total_ints_epa = "EPA Lost",
             must_pass_3rd = "#",
             must_pass_3rd_epa = "EPA Lost",
             last_2_min = "#",
             last_2_min_epa = "EPA Lost",
             long_2nd = "#",
             long_2nd_epa = "EPA Lost",
             first_down = "#",
             first_down_epa = "EPA Lost",
             rz_ints = "#", 
             rz_int_epa = "EPA Lost") %>% 
  cols_width(c(total_ints, must_pass_3rd, 
               last_2_min, long_2nd, first_down, rz_ints) ~ px(25)) %>%
  cols_width(c(total_ints_epa, must_pass_3rd_epa,
               last_2_min_epa, long_2nd_epa, first_down_epa, rz_int_epa) ~ px(75)) %>%
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
        columns = c(must_pass_3rd)
      )
    )
  ) %>%
  # data_color(
  #   columns = c(total_ints),
  #   colors = scales::col_numeric(
  #     paletteer::paletteer_d(
  #       palette = "RColorBrewer::PRGn"
  #     ) %>% as.character(),
  #     domain = NULL
  #   ),
  #   alpha = 0.85
  # ) %>%
  data_color(
    columns = c(total_ints_epa),
    colors = scales::col_numeric(
      paletteer::paletteer_d(
        palette = "RColorBrewer::PRGn"
      ) %>% as.character(),
      domain = NULL
    ),
    alpha = 0.85
  ) %>%
  # data_color(
  #   columns = c(must_pass_3rd),
  #   colors = scales::col_numeric(
  #     paletteer::paletteer_d(
  #       palette = "RColorBrewer::PRGn"
  #     ) %>% as.character(),
  #     domain = NULL
  #   ),
  #   alpha = 0.85
  # ) %>%
  data_color(
    columns = c(must_pass_3rd_epa),
    colors = scales::col_numeric(
      paletteer::paletteer_d(
        palette = "RColorBrewer::PRGn"
      ) %>% as.character(),
      domain = NULL
    ),
    alpha = 0.85
  ) %>%
  # data_color(
  #   columns = c(last_2_min),
  #   colors = scales::col_numeric(
  #     paletteer::paletteer_d(
  #       palette = "RColorBrewer::PRGn"
  #     ) %>% as.character(),
  #     domain = NULL
  #   ),
  #   alpha = 0.85
  # ) %>%
  data_color(
    columns = c(last_2_min_epa),
    colors = scales::col_numeric(
      paletteer::paletteer_d(
        palette = "RColorBrewer::PRGn"
      ) %>% as.character(),
      domain = NULL
    ),
    alpha = 0.85
  ) %>%
  # data_color(
  #   columns = c(long_2nd),
  #   colors = scales::col_numeric(
  #     paletteer::paletteer_d(
  #       palette = "RColorBrewer::PRGn"
  #     ) %>% as.character(),
  #     domain = NULL
  #   ),
  #   alpha = 0.85
  # ) %>%
  data_color(
    columns = c(long_2nd_epa),
    colors = scales::col_numeric(
      paletteer::paletteer_d(
        palette = "RColorBrewer::PRGn"
      ) %>% as.character(),
      domain = NULL
    ),
    alpha = 0.85
  ) %>%
  # data_color(
  #   columns = c(first_down),
  #   colors = scales::col_numeric(
  #     paletteer::paletteer_d(
  #       palette = "RColorBrewer::PRGn"
  #     ) %>% as.character(),
  #     domain = NULL
  #   ),
  #   alpha = 0.85
  # ) %>%
  data_color(
    columns = c(first_down_epa),
    colors = scales::col_numeric(
      paletteer::paletteer_d(
        palette = "RColorBrewer::PRGn"
      ) %>% as.character(),
      domain = NULL
    ),
    alpha = 0.85
  ) %>%
  data_color(
    columns = c(rz_int_epa),
    colors = scales::col_numeric(
      paletteer::paletteer_d(
        palette = "RColorBrewer::PRGn"
      ) %>% as.character(),
      domain = NULL
    ),
    alpha = 0.85
  ) %>%
  tab_spanner(
    label = html("All<br>INTs"),
    columns = c(total_ints, total_ints_epa)
  ) %>%
  tab_spanner(
    label = html("3rd down,<br>xPass >75%"),
    columns = c(must_pass_3rd, must_pass_3rd_epa)
  ) %>%
  tab_spanner(
    label = html("Last 2 min<br>first half"),
    columns = c(last_2_min, last_2_min_epa)
  ) %>%
  tab_spanner(
    label = html("2nd &<br>Long (15+)"),
    columns = c(long_2nd, long_2nd_epa)
  ) %>%
  tab_spanner(
    label = html("1st<br>& 10"),
    columns = c(first_down, first_down_epa)
  ) %>%
  tab_spanner(
    label = html("Red<br>Zone"),
    columns = c(rz_ints, rz_int_epa)
  ) %>%
  text_transform(locations = cells_body(c(headshot_url)),
                 fn = function(x) web_image(url = paste0(x))) %>% 
  text_transform(locations = cells_body(c(posteam)),
                 fn = function(x) web_image(url = paste0('https://a.espncdn.com/i/teamlogos/nfl/500/',x,'.png')))%>% 
  tab_source_note(
    source_note = "Table: @SamHoppen | Data: @nflfastR"
  )

gt::gtsave(int_table, file = file.path("C:/Users/sphop/OneDrive/Betsperts/R/Charts/INT breakdown.png"))





# https://twitter.com/CowboysStats/status/1615034349404946437?s=20&t=-38NV4IFURbpcKdtOHopMw

# Last week, @CowboysStats shared a breakdown of Dak's interceptions by situation, which captured the fact that some of his interceptions really weren't that bad.

# I recreated some of those categories for each quarterback this season to compare his picks to others. Note: categories aren't mutually exclusive.

