# custom theme

# use a function to define new theme (or build upon extant) with %+replace%
# https://github.com/tidyverse/ggplot2/blob/main/R/theme-defaults.r


theme_nsse <- function(
  # NSSE style guide:
  #  only major grid lines, and only against orientation (eg only horiz for vert bars)
  #  major grid lines for every unitid (0, 25, 50, 100; 0, 20, 40, 60)
  #  for stacked bars, x% superimposed and no "& in labels
  #  plot background in light blue: #D9E6F9 (217-230-249) or light grey
  #  Font: Calibri (sans?), NSSE navy, unless for FY+SR side-by-side: plum and ital
  #  Percentage rounded to 0, with %
  #  other measures (eg hours/wk) no unit
  
  # default args, can be supplied to existing theme
  base_size = 11, 
  base_family = "sans", # one of windowsFonts()
  base_line_size = base_size /  22, 
  base_rect_size  = base_size / 22) {
  library(ggplot2)
  library(dplyr)
  library(tidyr)
  library(stringr)
  windowsFonts(`Calibri` = windowsFont("Calibri")) # adding fonts in session
  
  theme_minimal(
    # ident
    base_size = base_size, 
    base_family = "Calibri", 
    base_line_size = base_line_size, 
    base_rect_size = base_rect_size
  ) %+replace%
    theme(
      # below, what theme_minimal adds to theme_bw
      # axis.ticks = element_blank(),
      # legend.background = element_blank(),
      # legend.key = element_blank(),
      # panel.background = element_blank(),
      # panel.border = element_blank(),
      # strip.background = element_blank(),
      # plot.background = element_blank(),
      # complete = TRUE
      plot.title = element_text(colour = "#002D6D", hjust = 0), # text= to affect all
      plot.subtitle = element_text(colour = "#002D6D", hjust = 0, size = rel(.9)), # and axis.text for axis
      plot.title.position = "plot",
      axis.text = element_text(size = rel(.8)),
      axis.text.x = element_text(color = "#A6A6A6"), # per bob
      panel.grid.minor = element_blank(),
      # for vert bars; x for horiz bars (reversed for coord_flip, regardless of order)
      panel.grid.major.x = element_line(size = 1), 
      panel.grid.major.y = element_blank(),
      panel.background = element_rect(#fill = "#CCCCCC", 
                                      fill = "#D9D9D9",
                                      color = "white"),
      
      legend.position = "right", #looks better when flipped
      complete = TRUE
    )
}

# bar plots below, as fun?

# Example ####
# source("nsse_colors.r")
# 
# d <- tibble(
#   x = sample(c("Never", "Sometimes", "Often", "Very often"), 1000,
#              replace = TRUE, prob = c(.25, .3, .3, .15)) %>%
#     factor(levels =c("Never", "Sometimes", "Often", "Very often")),
#   y = sample(c("Never", "Sometimes", "Often", "Very often"), 1000,
#              replace = TRUE, prob = c(.2, .3, .35, .15)) %>%
#     factor(levels =c("Never", "Sometimes", "Often", "Very often")),
#   z = sample(c("FY", "SR"), 1000, replace = TRUE, prob = c(.55, .45)) %>%
#     factor(levels = c("FY", "SR")),
# 
# )
# d_table <- pivot_longer(d, -z) %>%
#   group_by(z, name) %>%
#   count(value) %>%
#   mutate(p = n / sum(n) * 100, 
#          value = forcats::fct_rev(value),
#          ) %>%
#   left_join(
#     tibble(name = c("x", "y"),
#            text = c("X: Attended an art exhibit, musical event, etc.",
#                     "Y: Asked questions during class")
#            ),
#     by = "name"
#   )
# 
# 
# # for comparison
# p1 <- ggplot(d_table, aes(x = text, y = p, fill = value)) +
#   geom_col(position = "stack") +
#   labs(title = "Theme minimal", subtitle = "subtitle", caption = "caption") +
#   coord_flip() +
#   theme_minimal() +
#   facet_wrap(~z) +
#   scale_x_discrete(labels = function(x) str_wrap(x, width = 16)) +
#   scale_fill_manual(values = rev(NSSEcolors$standard4$hex))
# 
# p2 <- ggplot(d_table, aes(x = text, y = p, fill = value)) +
#   geom_col(position = "stack") +
#   # if stacked, superimpose % in white, bold
#   labs(title = "Theme NSSE", subtitle = "plot-pos better for long text (stem, RO)", caption = "caption") +
#   theme_nsse() +
#   coord_flip() +
#   facet_wrap(~z) +
#   scale_x_discrete(labels = function(x) str_wrap(x, width = 16)) +
#   scale_y_continuous(labels = paste0(seq(0, 100, 25), "%"),
#                      breaks = seq(0, 100, 25),
#                      limits = c(0, 100)) +
#   scale_fill_manual(values = rev(NSSEcolors$standard4$hex)) +
#   labs(x = NULL, y = "Percentage")
# 
# 
# gridExtra::grid.arrange(p1, p2)
# 
