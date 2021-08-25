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
library(extrafont)

# decide what font I should use based on what is available on computer
#font_SB <- ifelse(length(grep('HP Simplified',fonts()))>0,'HP Simplified','Bahnschrift')

nba_teams <- c('ATL', 'BKN', 'BOS', 'CHA', 'CHI', 'CLE', 'DAL', 'DEN', 'DET', 'GSW',
               'HOU', 'IND', 'LAC', 'LAL', 'MEM', 'MIA', 'MIL', 'MIN', 'NOR', 
               'NYK', 'OKC', 'ORL', 'PHI', 'PHO', 'POR', 'SAC', 'SAS', 'TOR', 'UTA', 'WAS')

nfl_teams <- c('BUF', 'MIA', 'NE', 'NYJ', 'BAL', 'CIN', 'CLE', 'PIT', 'HOU', 'IND', 'JAX', 'TEN', 'DEN', 'KC', 'LAC', 'LV', 'DAL', 'NYG', 'PHI', 'WAS', 'CHI', 'DET', 'GB', 'MIN', 'ATL', 'CAR', 'NO', 'TB', 'ARI', 'LA', 'SEA', 'SF')

# functions to retrieve images
nfl_wordmark_url = function(x) ifelse(is.na(x),NA,paste0('https://raw.githubusercontent.com/samhoppen/Fantasy-Evaluator/main/Logos/NFL/',x,'.png'))
nba_wordmark_url = function(x) ifelse(is.na(x),NA,paste0('https://raw.githubusercontent.com/samhoppen/Fantasy-Evaluator/main/Logos/NBA/',x,'.png'))

font_add_google("Encode Sans Condensed", "encode", regular.wt = 400, bold.wt = 600)
font_add_google("Inconsolata", "incon")
showtext_auto()

logo_asp <- 1.618
#logo_asp <- 1.778

showtext_opts(dpi = 480)

brand_nfl_plot <- function(orig_plot, save_name, asp = 16/9, base_size = 5, tm_wordmarks = F, logo = F,
                               logo_ETR = F, logo_FE = F, logo_4for4 = F, logo_4for4_red = F, logo_SC = F, logo_loc) {
  
  ## start by adding team wordmarks
  if (tm_wordmarks) {
    orig_plot_bld <- ggplot_gtable(ggplot_build(orig_plot))
    grob_strip_index <- which(sapply(orig_plot_bld$grob, function(x) x$name)=='strip')
    facet_id <- sapply(grob_strip_index, function(grb) {
      orig_plot_bld$grobs[[grb]]$grobs[[1]]$children[[2]]$children[[1]]$label
    })
    
    orig_plot_bld$layout$z[grob_strip_index] <- 0
    
    for (i in 1:length(facet_id)) {
      team_wd <- rasterGrob(image = image_read(nfl_wordmark_url(facet_id[i])), vp = viewport(height = 0.6))
      tot_tree <- grobTree(team_wd)
      
      orig_plot_bld$grobs[[grob_strip_index[i]]] <- tot_tree
    }
    orig_plot <- ggdraw(orig_plot_bld)
  }
  
  # is image taller than wider? if so, make sure the width is at least the base_size
  #if (asp < 1) {
  #  base_size_rat_wid <- (5/base_size)
  #  base_size <- base_size / asp
  #} else {
  #  base_size_rat_wid <- (5/base_size) / asp
  #}
  
  # aesthetics for various logos used
  if (logo_FE){
    logo_file <- magick::image_read("C:/Users/Hoppy/OneDrive/Fantasy Evaluator/Logo.png")
    logo_width <- 0.16
    logo_height <- 0.09
    logo_x <- 0.85
    logo_y <- 0.91
  }
  if (logo_SC){
    logo_file <- magick::image_read("C:/Users/Hoppy/OneDrive/NFL Analysis/Data Repository/Ship Chasing.jpg")
    logo_width <- 0.09
    logo_height <- 0.09
    logo_x <- 0.925
    logo_y <- 0.91
  }
  if (logo_4for4){
    logo_file <- magick::image_read("C:/Users/Hoppy/OneDrive/NFL Analysis/Data Repository/4for4.jpg")
    logo_width <- 0.09
    logo_height <- 0.09
    logo_x <- 0.925
    logo_y <- 0.91
  }
  if (logo_4for4_red){
    logo_file <- magick::image_read("C:/Users/Hoppy/OneDrive/NFL Analysis/Data Repository/4for4_red.jpg")
    logo_width <- 0.09
    logo_height <- 0.09
    logo_x <- 0.925
    logo_y <- 0.91
  }
  if (logo_ETR){
    logo_file <- magick::image_read_svg("C:/Users/Hoppy/OneDrive/NFL Analysis/Data Repository/etr.svg")
    logo_width <- 0.12
    logo_height <- 0.0675
    logo_x <- 0.87
    logo_y <- 0.91
  }
  
  if (tm_wordmarks){
    logo_x <- logo_x
    logo_y <- 0.92
  }
  
  orig_plot <- ggdraw(orig_plot) + draw_image(logo_file, x = logo_x, y = logo_y, hjust = 0, vjust = 0, height = logo_height, width = logo_width)
  
  ggsave(save_name, orig_plot, dpi = 480, height = base_size, width = base_size * (asp))
  
}





# brand_nfl_plot <- function(orig_plot, save_name, asp = 16/9, tm_wordmarks = F, logo = F,
#                            logo_ETR = F, logo_FE = F, logo_4for4 = F, logo_4for4_red = F, logo_SC = F) {
# 
#   ## start by adding team wordmarks
#   if (tm_wordmarks) {
#     orig_plot_bld <- ggplot_gtable(ggplot_build(orig_plot))
#     grob_strip_index <- which(sapply(orig_plot_bld$grob, function(x) x$name)=='strip')
#     facet_id <- sapply(grob_strip_index, function(grb) {
#       orig_plot_bld$grobs[[grb]]$grobs[[1]]$children[[2]]$children[[1]]$label
#     })
#     
#     orig_plot_bld$layout$z[grob_strip_index] <- 0
#     
#     for (i in 1:length(facet_id)) {
#       team_wd <- rasterGrob(image = image_read(nfl_wordmark_url(facet_id[i])), vp = viewport(height = 1, width = 0.75)) #height = 1, width = 0.75
#       tot_tree <- grobTree(team_wd)
#       
#       orig_plot_bld$grobs[[grob_strip_index[i]]] <- tot_tree
#     }
#     orig_plot <- ggdraw(orig_plot_bld)
#   }
#   
#   # aesthetics for various logos used
#   if (logo_FE){
#     logo_file <- magick::image_read("C:/Users/Hoppy/OneDrive/Fantasy Evaluator/Logo.png")
#     logo_width <- 0.16
#     logo_height <- 0.09
#     logo_x <- 0.85
#     logo_y <- 0.875
#   }
#   if (logo_4for4){
#     logo_file <- magick::image_read("C:/Users/Hoppy/OneDrive/NFL Analysis/Data Repository/4for4.jpg")
#     logo_width <- 0.09
#     logo_height <- 0.09
#     logo_x <- 0.92
#     logo_y <- 0.875
#   }
#   if (logo_4for4_red){
#     logo_file <- magick::image_read("C:/Users/Hoppy/OneDrive/NFL Analysis/Data Repository/4for4_red.jpg")
#     logo_width <- 0.09
#     logo_height <- 0.09
#     logo_x <- 0.92
#     logo_y <- 0.875
#   }
#   if (logo_ETR){
#     logo_file <- magick::image_read_svg("C:/Users/Hoppy/OneDrive/NFL Analysis/Data Repository/etr.svg")
#     logo_width <- 0.12
#     logo_height <- 0.0675
#     logo_x <- 0.85
#     logo_y <- 0.875
#   }
#   if (logo_SC){
#     logo_file <- magick::image_read("C:/Users/Hoppy/OneDrive/NFL Analysis/Data Repository/Ship Chasing.jpg")
#     logo_width <- 0.09
#     logo_height <- 0.09
#     logo_x <- 0.92
#     logo_y <- 0.875
#   }
#   
#   if (tm_wordmarks){
#     logo_x <- logo_x - 0.05
#     logo_y <- 0.875
#   }
#   
#   if (logo) {final_plot <- ggdraw(
#       xlim = c(0, 900*asp),
#       ylim = c(0, 900)
#     ) + 
#     draw_plot(
#       orig_plot,
#       x = ((900*asp)/2),
#       hjust = 0.5,
#       width = ((900*asp)-36),
#       y = (900-60)/2,
#       height = (900-60),
#       vjust = 0.5
#     ) + 
#     draw_image(logo_file, x = logo_x*900*asp, y = logo_y*900, hjust = 0, vjust = 0, height = logo_height*900, width = logo_width*1600)}
#   
#   else {final_plot <- ggdraw(
#     xlim = c(0, 900*asp),
#     ylim = c(0, 900)
#   ) + 
#     draw_plot(
#       orig_plot,
#       x = ((900*asp)/2),
#       hjust = 0.5,
#       width = ((900*asp)-36),
#       y = (900-60)/2,
#       height = (900-60),
#       vjust = 0.5
#     )}  
#   save_plot(
#     filename = save_name,
#     plot = final_plot,
#     base_height = 900 / 72,
#     base_asp = asp,
#     dpi = 72
#   )
#   
# }
# 
# main function to save my branded plots
brand_nba_plot <- function(orig_plot, save_name, asp = 16/9,tm_wordmarks = F, logo = F,
                           logo_ETR = F, logo_FE = F, logo_4for4 = F, logo_4for4_red = F) {
  
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
  
  # aesthetics for various logos used
  if (logo_FE){
    logo_file <- magick::image_read("C:/Users/Hoppy/OneDrive/Fantasy Evaluator/Logo.png")
    logo_width <- 0.16
    logo_height <- 0.09
    logo_x <- 0.875
    logo_y <- 0.875
  }
  if (logo_4for4){
    logo_file <- magick::image_read("C:/Users/Hoppy/OneDrive/NFL Analysis/Data Repository/4for4.jpg")
    logo_width <- 0.09
    logo_height <- 0.09
    logo_x <- 0.92
    logo_y <- 0.875
  }
  if (logo_4for4_red){
    logo_file <- magick::image_read("C:/Users/Hoppy/OneDrive/NFL Analysis/Data Repository/4for4_red.jpg")
    logo_width <- 0.09
    logo_height <- 0.09
    logo_x <- 0.92
    logo_y <- 0.875
  }
  if (logo_ETR){
    logo_file <- magick::image_read_svg("C:/Users/Hoppy/OneDrive/NFL Analysis/Data Repository/etr.svg")
    logo_width <- 0.12
    logo_height <- 0.0675
    logo_x <- 0.875
    logo_y <- 0.875
  }
  
  if (tm_wordmarks){
    logo_x <- logo_x - 0.05
    logo_y <- 0.875
  }
  
  final_plot <- ggdraw(
    xlim = c(0, 900*asp),
    ylim = c(0, 900)
  ) + 
    draw_plot(
      orig_plot,
      x = ((900*asp)/2),
      hjust = 0.5,
      width = ((900*asp)-36),
      y = (900-60)/2,
      height = (900-60),
      vjust = 0.5
    ) + 
    draw_image(logo_file, x = logo_x*900*asp, y = logo_y*900, hjust = 0, vjust = 0, height = logo_height*900, width = logo_width*1600)
  
  save_plot(
    filename = save_name,
    plot = final_plot,
    base_height = 900 / 72,
    base_asp = 16 / 9,
    dpi = 72
  )
  
}

theme_FE <-  theme(
  line = element_line(lineend = 'round', color='black'),     #rounds the edges of all lines; makes the color black
  text = element_text(color='black'),     #uses the Incon text format for all text; makes the color black
  panel.border = element_blank(),     #makes the panel around the plotting area the color black
  panel.background = element_rect(fill = 'white', color = 'transparent'),     #background of the non-plotting area is white
  axis.line = element_line(color = 'black', size = 0.5),
  axis.ticks = element_line(color = 'black', size = 0.5),     #changes the size (width) of the x-axis ticks
  axis.ticks.length = unit(0.15, 'lines'),     #changes the length of the axis ticks
  axis.title = element_text(size = 8),     #changes the size of the axis titles, if any
  axis.text = element_text(size = 8, color = 'black'),     #changes the size of the axis labels
  plot.title = element_text(size = 16, face = "bold", margin = margin(0,0,5,0), family = "encode"),     #changes the size of the title
  plot.subtitle = element_text(size = 8, margin = margin(0,0,5,0), family = "encode"),     #changes the size of the subtitle
  plot.caption = element_text(size = 8, family = "encode"),     #changes the size of the caption , family = "encode"
  legend.background = element_blank(),     #makes background of the legend to be grey
  legend.key = element_blank(),     #removes the legend key
  panel.grid.minor = element_blank(),     #removes the lines on the plot between the ticks
  panel.grid.major = element_line(color='grey85', size = 0.3),     #changes the size of the major gridlines and makes them grey
  panel.grid.major.x = element_blank(),
  axis.title.y = element_text(angle = 90, vjust = 0.5),     #changes the size of the axis labels
  strip.background = element_blank(),
  strip.text = element_text(size = 8, color = 'black'),
  panel.spacing.y = unit(0, 'lines'),
  panel.spacing.x = unit(0.5, 'lines')
)



# theme_FE <-  theme(
#   line = element_line(lineend = 'round', color='black'),     #rounds the edges of all lines; makes the color black
#   text = element_text(color='black'),     #uses the Incon text format for all text; makes the color black
#   panel.border = element_blank(),     #makes the panel around the plotting area the color black
#   panel.background = element_rect(fill = 'white', color = 'transparent'),     #background of the non-plotting area is white
#   axis.line = element_line(color = 'black', size = 0.5),
#   axis.ticks = element_line(color = 'black', size = 0.5),     #changes the size (width) of the x-axis ticks
#   axis.ticks.length = unit(0.15, 'lines'),     #changes the length of the axis ticks
#   axis.title = element_text(size = 12),     #changes the size of the axis titles, if any
#   axis.text = element_text(size = 12, color = 'black'),     #changes the size of the axis labels
#   plot.title = element_text(size = 24, face = "bold", margin = margin(0,0,10,0), family = "encode"),     #changes the size of the title
#   plot.subtitle = element_text(size = 12, margin = margin(0,0,10,0), family = "encode"),     #changes the size of the subtitle
#   plot.caption = element_text(size = 12, family = "encode"),     #changes the size of the caption , family = "encode"
#   legend.background = element_blank(),     #makes background of the legend to be grey
#   legend.key = element_blank(),     #removes the legend key
#   panel.grid.minor = element_blank(),     #removes the lines on the plot between the ticks
#   panel.grid.major = element_line(color='grey85', size = 0.3),     #changes the size of the major gridlines and makes them grey
#   panel.grid.major.x = element_blank(),
#   axis.title.y = element_text(angle = 90, vjust = 0.5),     #changes the size of the axis labels
#   strip.background = element_blank(),
#   strip.text = element_text(size = 12, color = 'black'),
#   panel.spacing.y = unit(0, 'lines'),
#   panel.spacing.x = unit(0.5, 'lines')
# )

##### CUSTOM FACET WRAP HELPER FUNCTIONS ####
scale_override <- function(which, scale) {
  if(!is.numeric(which) || (length(which) != 1) || (which %% 1 != 0)) {
    stop("which must be an integer of length 1")
  }
  
  if(is.null(scale$aesthetics) || !any(c("x", "y") %in% scale$aesthetics)) {
    stop("scale must be an x or y position scale")
  }
  
  structure(list(which = which, scale = scale), class = "scale_override")
}


CustomFacetWrap <- ggproto(
  "CustomFacetWrap", FacetWrap,
  init_scales = function(self, layout, x_scale = NULL, y_scale = NULL, params) {
    # make the initial x, y scales list
    scales <- ggproto_parent(FacetWrap, self)$init_scales(layout, x_scale, y_scale, params)
    
    if(is.null(params$scale_overrides)) return(scales)
    
    max_scale_x <- length(scales$x)
    max_scale_y <- length(scales$y)
    
    # ... do some modification of the scales$x and scales$y here based on params$scale_overrides
    for(scale_override in params$scale_overrides) {
      which <- scale_override$which
      scale <- scale_override$scale
      
      if("x" %in% scale$aesthetics) {
        if(!is.null(scales$x)) {
          if(which < 0 || which > max_scale_x) stop("Invalid index of x scale: ", which)
          scales$x[[which]] <- scale$clone()
        }
      } else if("y" %in% scale$aesthetics) {
        if(!is.null(scales$y)) {
          if(which < 0 || which > max_scale_y) stop("Invalid index of y scale: ", which)
          scales$y[[which]] <- scale$clone()
        }
      } else {
        stop("Invalid scale")
      }
    }
    
    # return scales
    scales
  }
)

facet_wrap_custom <- function(..., scale_overrides = NULL) {
  # take advantage of the sanitizing that happens in facet_wrap
  facet_super <- facet_wrap(...)
  
  # sanitize scale overrides
  if(inherits(scale_overrides, "scale_override")) {
    scale_overrides <- list(scale_overrides)
  } else if(!is.list(scale_overrides) || 
            !all(vapply(scale_overrides, inherits, "scale_override", FUN.VALUE = logical(1)))) {
    stop("scale_overrides must be a scale_override object or a list of scale_override objects")
  }
  
  facet_super$params$scale_overrides <- scale_overrides
  
  ggproto(NULL, CustomFacetWrap,
          shrink = facet_super$shrink,
          params = facet_super$params
  )
}


#### OLD CODE ####
#theme_FE_reg <-  theme(
#  line = element_line(lineend = 'round', color='black'),     #rounds the edges of all lines; makes the color black
#  text = element_text(color='black'),     #uses the Incon text format for all text; makes the color black
#  panel.border = element_rect(color = 'black', fill = NA),     #makes the panel around the plotting area the color black
#  panel.background = element_rect(fill = 'white', color = 'transparent'),     #background of the non-plotting area is white
#  axis.ticks = element_line(color = 'black', size = 0.5),     #changes the size (width) of the x-axis ticks
#  axis.ticks.length = unit(2.75, 'pt'),     #changes the length of the axis ticks
#  axis.title = element_text(size = 8),     #changes the size of the axis titles, if any
#  axis.text = element_text(size = 7, color = 'black'),     #changes the size of the axis labels
#  plot.title = element_text(size = 14, family = "encode"),     #changes the size of the title
#  plot.subtitle = element_text(size = 8, family = "encode"),     #changes the size of the subtitle
#  plot.caption = element_text(size = 5, family = "encode"),     #changes the size of the caption
#  legend.background = element_rect(fill = 'grey90', color = 'black'),     #makes background of the legend to be grey
#  legend.key = element_blank(),     #removes the legend key
#  panel.grid.minor = element_blank(),     #removes the lines on the plot between the ticks
#  panel.grid.major = element_line(color='grey85', size = 0.3),     #changes the size of the major gridlines and makes them grey
#  axis.title.y = element_text(angle = 0, vjust = 0.5),     #changes the size of the axis labels
#  strip.background = element_blank(),
#  strip.text = element_text(size = 6, color = 'black'),
#  legend.position = 'bottom',
#  panel.spacing.y = unit(0, 'lines'),
#  panel.spacing.x = unit(0.1, 'lines')
#)

#theme_FE_facet <- theme(
#  line = element_line(lineend = 'round', color='black'),     #rounds the edges of all lines; makes the color black
#  text = element_text(color='black'),     #uses the Incon text format for all text; makes the color black
#  panel.border = element_rect(color = 'grey95', size = 0.1), #element_rect(color = 'black', fill = NA),     #makes the panel around the plotting area the color black
#  panel.background = element_rect(fill = 'white', color = 'transparent'),     #background of the non-plotting area is white
#  axis.ticks = element_line(color = 'black', size = 0.5),     #changes the size (width) of the x-axis ticks
#  axis.ticks.y = element_blank(), 
#  axis.ticks.length = unit(0.15, 'lines'),     #changes the length of the axis ticks
#  axis.title.x = element_text(size = 45),     #changes the size of the axis titles, if any
#  axis.title.y = element_blank(),
#  axis.text.x = element_text(size = 15, color = 'black'),     #changes the size of the axis labels
#  axis.text.y = element_blank(),
#  legend.background = element_rect(fill = 'grey90', color = 'black'),     #makes background of the legend to be grey
#  legend.key = element_blank(),     #removes the legend key
#  panel.grid.minor = element_blank(),     #removes the lines on the plot between the ticks
#  panel.grid.major = element_line(color='grey85', size = 0.3),     #changes the size of the major gridlines and makes them grey
#  panel.grid.major.y = element_blank(),
#  strip.background = element_blank(),
#  strip.text = element_text(size = 6, color = 'black'),
#  legend.position = 'bottom',
#  panel.spacing.y = unit(0, 'lines'),
#  panel.spacing.x = unit(0.5, 'lines'), ## START HERE
#  plot.title = element_text(size = 75, family = "encode", face = "bold", margin = margin(0,0,-10,0)),
#  plot.subtitle = element_text(size = 30, family = "encode", margin = margin(0,0,0,0)),
#  plot.caption = element_text(size = 30, family = "encode"),
#  axis.line = element_line(color = 'black', size = 0.5)
#)