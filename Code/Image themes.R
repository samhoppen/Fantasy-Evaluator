library(ggplot2)
library(gridExtra)
library(grid)
library(png)
library(cowplot)
library(colorspace)
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
library(nflplotR)
library(tidyverse)
library(dplyr)
library(nflreadr)
library(nflfastR)
library(gtExtras)

# hide warnings for fonts
options(warn = -1)

# nba_teams <- c('ATL', 'BKN', 'BOS', 'CHA', 'CHI', 'CLE', 'DAL', 'DEN', 'DET', 'GSW',
#                'HOU', 'IND', 'LAC', 'LAL', 'MEM', 'MIA', 'MIL', 'MIN', 'NOR', 
#                'NYK', 'OKC', 'ORL', 'PHI', 'PHO', 'POR', 'SAC', 'SAS', 'TOR', 'UTA', 'WAS')

nfl_teams <- c('BUF', 'MIA', 'NE', 'NYJ', 'BAL', 'CIN', 'CLE', 'PIT', 'HOU', 'IND', 'JAX', 'TEN', 'DEN', 'KC', 'LAC', 'LV', 'DAL', 'NYG', 'PHI', 'WAS', 'CHI', 'DET', 'GB', 'MIN', 'ATL', 'CAR', 'NO', 'TB', 'ARI', 'LA', 'SEA', 'SF')

# functions to retrieve images
# nfl_wordmark_url = function(x) ifelse(is.na(x),NA,paste0('https://raw.githubusercontent.com/samhoppen/Fantasy-Evaluator/main/Logos/NFL/',x,'.png'))
# nba_wordmark_url = function(x) ifelse(is.na(x),NA,paste0('https://raw.githubusercontent.com/samhoppen/Fantasy-Evaluator/main/Logos/NBA/',x,'.png'))

font_add_google("Encode Sans Condensed", "encode", regular.wt = 400, bold.wt = 600)
font_add_google("Cairo", "cairo")
showtext_auto()

logo_asp <- 1.618

showtext_opts(dpi = 480)

brand_nfl_plot <- function(orig_plot, save_name, asp = 16/9, base_size = 5, tm_wordmarks = F, logo = F, 
                           logo_fp = F, logo_bp = F, logo_loc) {
  
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

  if (asp == 1){
    logo_x <- 0.94
  } else{
    logo_x <- 0.95
  }
  # aesthetics for various logos used
  if (logo_fp){
    logo_file <- magick::image_read("C:/Users/sphop/OneDrive/FantasyPros/Logos/fp_logo.jpg")
    logo_width <- 0.06*(5/base_size)
    logo_height <- 0.06*(5/base_size)
    #logo_x <- 0.94
    #logo_y <- 0.93+(0.01*(base_size-5))
    logo_y <- 0.94
  }
  if (logo_bp){
    logo_file <- magick::image_read("C:/Users/sphop/OneDrive/FantasyPros/Logos/bp_logo.jpg")
    logo_width <- 0.06*(5/base_size)
    logo_height <- 0.06*(5/base_size)
    #logo_x <- 0.94
    #logo_y <- 0.93+(0.01*(base_size-5))
    logo_y <- 0.94
  }
  
  
  if (logo) {
    if (tm_wordmarks){
      logo_x <- logo_x
      logo_y <- 0.94
    }
    
    orig_plot <- ggdraw(orig_plot) + draw_image(logo_file, x = logo_x, y = logo_y, hjust = 0, vjust = 0, height = logo_height, width = logo_width)
  }
  else{
    orig_plot <- ggdraw(orig_plot)
  }
  ggsave(save_name, orig_plot, dpi = 480, height = base_size, width = base_size * (asp))
  
}


theme_FE <-  theme(
  line = element_line(lineend = 'round', color='black'),     #rounds the edges of all lines; makes the color black
  text = element_text(color='black', family = "cairo"),     #uses the Incon text format for all text; makes the color black
  panel.border = element_blank(),     #makes the panel around the plotting area the color black
  panel.background = element_rect(fill = 'white', color = 'transparent'),     #background of the non-plotting area is white
  axis.line = element_line(color = 'black', size = 0.5),
  axis.ticks = element_line(color = 'black', size = 0.5),     #changes the size (width) of the x-axis ticks
  axis.ticks.length = unit(0.15, 'lines'),     #changes the length of the axis ticks
  axis.title = element_text(size = 8, family = "cairo"),     #changes the size of the axis titles, if any
  axis.text = element_text(size = 8, color = 'black', family = "cairo"),     #changes the size of the axis labels
  plot.title = element_text(size = 16, face = "bold", margin = margin(0,0,5,0), family = "cairo"),     #changes the size of the title
  plot.subtitle = element_text(size = 8, margin = margin(0,0,5,0), family = "cairo"),     #changes the size of the subtitle
  plot.caption = element_text(size = 8, family = "cairo"),     #changes the size of the caption , family = "encode"
  legend.background = element_blank(),     #makes background of the legend to be grey
  legend.key = element_blank(),     #removes the legend key
  panel.grid.minor = element_blank(),     #removes the lines on the plot between the ticks
  panel.grid.major = element_line(color='grey85', size = 0.3),     #changes the size of the major gridlines and makes them grey
  #panel.grid.major.x = element_blank(),
  axis.title.y = element_text(angle = 90, vjust = 0.5, family = "cairo"),     #changes the size of the axis labels
  strip.background = element_blank(),
  strip.text = element_text(size = 8, color = 'black', family = "cairo"),
  panel.spacing.y = unit(0, 'lines'),
  panel.spacing.x = unit(0.5, 'lines'),
  legend.text = element_text(size = 6, family = "cairo"),
  legend.title = element_text(size = 6, family = "cairo")
)

gt_bars <- function(
    gt_object,
    column,
    height = 16,
    fill = "purple",
    background = "#e1e1e1",
    scaled = FALSE,
    scaled_pct = FALSE,
    labels = FALSE,
    inside_label_color = "white",
    outside_label_color = "black",
    label_value_cutoff = 0.50,
    digits = 1,
    scale_label = TRUE,
    percent_sign = TRUE,
    font_style = "bold",
    font_size = "12px") {


  stopifnot(`'gt_object' must be a 'gt_tbl', have you accidentally passed raw data?` = "gt_tbl" %in%
    class(gt_object))

  # ensure font_style is one of the accepted values
  stopifnot(font_style %in% c("bold", "normal", "italic"))

  data_in <- gt_index(gt_object, column = {{ column }})

  gt_object %>%
    text_transform(
      locations = cells_body(columns = {{ column }}),
      fn = function(x) {
        if (length(na.omit(x)) == 0) {
          return("<div></div>")
        } else {
          if (isFALSE(scaled_pct)){
            max_x <- max(as.double(x), na.rm = TRUE)
          } else {
              max_x <- 1.0
            }
          
        }

        bar <- lapply(data_in, function(x) {
          scaled_value <- if (isFALSE(scaled)) {
            x / max_x * 100
          } else {
            x
          }

          if (labels) {
            # adjust values for labeling // scale_label
            label_values <- if (scale_label) {
              x * 100
            } else {
              x
            }

            # create label string to print out // add % sign if requested
            label <- if (percent_sign) {
              glue::glue("{round(label_values, digits)}%")
            } else {
              round(label_values, digits)
            }

            if (scaled_value > (label_value_cutoff * max_x) * 100) {
              glue::glue("<div style='background:{fill};width:{scaled_value}%;height:{height}px;display:flex;align-items:center;justify-content:center;color:{inside_label_color};font-weight:{font_style};font-size:{font_size};'>{label}</div>")
            } else {
              glue::glue(
                "<div style='background:{fill};width:{scaled_value}%;height:{height}px;display:flex;align-items:center;justify-content:flex-start;position:relative;'>",
                "<span style='color:{outside_label_color};position:absolute;left:100%;margin-left:5px;font-weight:{font_style};font-size:{font_size};'>{label}</span></div>"
              )
            }
          } else {
            glue::glue(
              "<div style='background:{fill};width:{scaled_value}%;height:{height}px;'></div>" # no labels added
            )
          }
        })

        chart <- lapply(bar, function(bar) {
          glue::glue("<div style='flex-grow:1;margin-left:8px;background:{background};'>{bar}</div>")
        })

        chart
      }
    ) %>%
    cols_align(align = "left", columns = {{ column }})
}
                                     
