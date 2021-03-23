scrape_projections <- function(position, week) {
  
  if (position == "qb") {
    naming <- c("", "PASS", "PASS", 'PASS', "PASS", "PASS", "RUSH", "RUSH", "RUSH", "", "")
  } else if (position == "rb") {
    naming <-  c("", "RUSH", "RUSH", "RUSH", "REC", "REC","REC", "", "")
  } else if (position == "wr") {
    
    naming <- c("", "REC", "REC","REC", "RUSH", "RUSH", "RUSH", "", "")
  } else {
    naming <- c("", "REC", "REC","REC", "", "")
  }
  
  # Be nice
  Sys.sleep(1)
  
  message(glue::glue("Scraping projected stats for {position} wk: {week}!"))
  
  url <- glue::glue("https://www.fantasypros.com/nfl/projections/{position}.php?week=draft")
  
  url %>%
    read_html() %>% 
    html_table() %>% 
    .[[1]] %>% 
    filter(X1 != "") %>%
    set_names(nm = .[1,]) %>%
    .[2:length(.$Player),] %>% 
    set_names(., paste({naming}, names(.), sep = "_")) %>% 
    rename("Player" = "_Player", "FL" = "_FL", "FPTS" = "_FPTS") %>% 
    mutate(position = toupper({position}),
           week = week) %>% 
    mutate(team = str_sub(Player, -3),
           team = str_extract(team, "[[:upper:]]+"),
           Player = str_remove(Player, team),
           Player = str_trim(Player)) %>% 
    select(Player, team, position, week, everything()) %>%
    rename_all(tolower)
}