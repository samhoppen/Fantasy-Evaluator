library(ggplot2)
library(gridExtra)
library(grid)
library(png)
library(cowplot)
library(colorspace)
library(extrafont)
library(RCurl)
library(ggpmisc)
library(magick)
library(ggimage)
library(shadowtext)
library(ggrepel)
library(scales)
library(magrittr)
library(tidyverse)
library(gganimate)
library(gt)
library(ggridges)
library(nflfastR)
library(showtext)
library(rsvg)
library(ggsci)

# decide what font I should use based on what is available on computer
#font_SB <- ifelse(length(grep('HP Simplified',fonts()))>0,'HP Simplified','Bahnschrift')

nba_teams <- c('ATL', 'BKN', 'BOS', 'CHA', 'CHI', 'CLE', 'DAL', 'DEN', 'DET', 'GSW',
               'HOU', 'IND', 'LAC', 'LAL', 'MEM', 'MIA', 'MIL', 'MIN', 'NOR', 
               'NYK', 'OKC', 'ORL', 'PHI', 'PHO', 'POR', 'SAC', 'SAS', 'TOR', 'UTA', 'WAS')

# functions to retrieve images
nba_wordmark_url = function(x) ifelse(is.na(x),NA,paste0('https://raw.githubusercontent.com/samhoppen/Fantasy-Evaluator/main/Logos/',x,'.png'))
#wordmark_url = function(x) ifelse(is.na(x),NA,paste0('https://raw.githubusercontent.com/ajreinhard/data-viz/master/wordmark/',x,'.png'))
#helmet_url = function(x) ifelse(is.na(x),NA,paste0('https://raw.githubusercontent.com/ajreinhard/data-viz/master/helmet_left/',x,'.png'))
#ESPN_logo_url = function(x) ifelse(is.na(x),NA,ifelse(x=='KC',paste0('https://raw.githubusercontent.com/ajreinhard/data-viz/master/alt-logo/',x,'.png'),paste0('https://a.espncdn.com/i/teamlogos/nfl/500/',x,'.png')))
#helm2020 <- function(team, side) paste0('https://raw.githubusercontent.com/ajreinhard/data-viz/master/2020_helm/',team,'_',side,'.png')

# my prefered team order for facets
#.tm_div_order <- c('BUF', 'MIA', 'NE', 'NYJ', 'BAL', 'CIN', 'CLE', 'PIT', 'HOU', 'IND', 'JAX', 'TEN', 'DEN', 'KC', 'LAC', 'LV', 'DAL', 'NYG', 'PHI', 'WAS', 'CHI', 'DET', 'GB', 'MIN', 'ATL', 'CAR', 'NO', 'TB', 'ARI', 'LA', 'SEA', 'SF')
#.tm_div_order_alt <- c('BUF', 'MIA', 'NE', 'NYJ', 'DAL', 'NYG', 'PHI', 'WAS', 'BAL', 'CIN', 'CLE', 'PIT', 'CHI', 'DET', 'GB', 'MIN', 'HOU', 'IND', 'JAX', 'TEN', 'ATL', 'CAR', 'NO', 'TB', 'DEN', 'KC', 'LAC', 'LV', 'ARI', 'LA', 'SEA', 'SF')
font_add_google("Encode Sans Condensed", "encode", regular.wt = 400, bold.wt = 600)
font_add_google("Inconsolata", "incon")
showtext_auto()

# main function to save my branded plots
brand_nba_plot <- function(orig_plot, save_name, asp = 1, base_size = 5, tm_wordmarks = F) {
  
  ## start by adding team wordmarks
  if (tm_wordmarks) {
    orig_plot_bld <- ggplot_gtable(ggplot_build(orig_plot))
    grob_strip_index <- which(sapply(orig_plot_bld$grob, function(x) x$name)=='strip')
    facet_id <- sapply(grob_strip_index, function(grb) {
      orig_plot_bld$grobs[[grb]]$grobs[[1]]$children[[2]]$children[[1]]$label
    })
    
    orig_plot_bld$layout$z[grob_strip_index] <- 0
    
    for (i in 1:length(facet_id)) {
      team_wd <- rasterGrob(image = image_read(nba_wordmark_url(facet_id[i])), vp = viewport(height = 0.8, width = 0.6))
      tot_tree <- grobTree(team_wd)
      
      orig_plot_bld$grobs[[grob_strip_index[i]]] <- tot_tree
    }
    orig_plot <- ggdraw(orig_plot_bld)
  }
  
  # is image taller than wider? if so, make sure the width is at least the base_size
  if (asp < 1) {
    base_size_rat_wid <- (5/base_size)
    base_size <- base_size / asp
  } else {
    base_size_rat_wid <- (5/base_size) / asp
  }
  
  #plt <- ggdraw(plt.final) + draw_image(logo_file, x = 0.002 * (base_size_rat_wid), y = 0, hjust = 0, vjust = 0, height = logo_size, width = 0.08 * (base_size_rat_wid))
  ggsave(save_name, orig_plot, dpi = 480)#, height = base_size, width = base_size * (asp))
  
}
#?ggsave
theme_FE <-  theme(
  line = element_line(lineend = 'round', color='black'),     #rounds the edges of all lines; makes the color black
  text = element_text(color='black'),     #uses the Incon text format for all text; makes the color black
  panel.border = element_rect(color = 'black', fill = NA),     #makes the panel around the plotting area the color black
  panel.background = element_rect(fill = 'white', color = 'transparent'),     #background of the non-plotting area is white
  axis.ticks = element_line(color = 'black', size = 0.5),     #changes the size (width) of the x-axis ticks
  axis.ticks.length = unit(2.75, 'pt'),     #changes the length of the axis ticks
  axis.title = element_text(size = 8),     #changes the size of the axis titles, if any
  axis.text = element_text(size = 7, color = 'black'),     #changes the size of the axis labels
  plot.title = element_text(size = 14, family = "encode"),     #changes the size of the title
  plot.subtitle = element_text(size = 8, family = "encode"),     #changes the size of the subtitle
  plot.caption = element_text(size = 5, family = "encode"),     #changes the size of the caption
  legend.background = element_rect(fill = 'grey90', color = 'black'),     #makes background of the legend to be grey
  legend.key = element_blank(),     #removes the legend key
  panel.grid.minor = element_blank(),     #removes the lines on the plot between the ticks
  panel.grid.major = element_line(color='grey85', size = 0.3),     #changes the size of the major gridlines and makes them grey
  axis.title.y = element_text(angle = 0, vjust = 0.5),     #changes the size of the axis labels
  strip.background = element_blank(),
  strip.text = element_text(size = 6, color = 'black'),
  legend.position = 'bottom',
  panel.spacing.y = unit(0, 'lines'),
  panel.spacing.x = unit(0.1, 'lines')
)
