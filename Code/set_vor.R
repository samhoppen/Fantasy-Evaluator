set_vor <- function(points_table, vor_baseline = NULL, vor_var = c("fpts", "floor", "ceiling")){
  if(is.null(vor_baseline))
    vor_baseline <- default_baseline
  
  vor_var <- match.arg(vor_var)
  
  vor_tbl <- select(points_table, "player", "position", vor_var) %>%
    rename(vor_var = !!vor_var) %>% group_by(position) %>%
    mutate(vor_rank = dense_rank(-vor_var), vor_base = vor_baseline[position]) %>%
    filter(vor_rank >= vor_base - 1 &  vor_rank <= vor_base + 1)  %>%
    summarise(vor_base = mean(vor_var)) %>%  ungroup() %>%
    select(position, vor_base) %>% inner_join(points_table, by = c("position")) %>%
    rename(vor_var = !!vor_var) %>%
    mutate(vor = vor_var - vor_base,
           rank = dense_rank(-vor), !!vor_var := vor_var) %>%
    select(player, position, vor, rank) %>% rename_if(is.numeric, funs(paste(vor_var, ., sep = "_"))) %>%
    ungroup()
  
  return(vor_tbl)
}