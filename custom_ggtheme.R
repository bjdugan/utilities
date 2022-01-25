# custom theme

library(ggplot2)
library(dplyr)
library(tidyr)
library(gridExtra)

# use a function to define new theme (or build upon extant) with %+replace%
# https://github.com/tidyverse/ggplot2/blob/main/R/theme-defaults.r

theme_nsse <- function(
  # default args, can be supplied to existing theme
  base_size = 11, 
  base_family = "serif", # one of windowsFonts()
  base_line_size = base_size /  22, 
  base_rect_size  = base_size / 22) {
  theme_minimal(
    # ident
    base_size = base_size, 
    base_family = base_family, 
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
    
    )
  
}

# something to plot
p1 <- mutate(mtcars, 
                 am = if_else(am == 0, "auto", "manual") %>% 
                   factor(),
                 vs = if_else(vs == 0, "V-shaped", "Straight") %>% 
                   factor()) %>% 
  tibble::rownames_to_column("name") %>% 
  select(am, mpg, cyl) %>% 
  ggplot(aes(x = cyl, y = mpg, color = am)) +
  geom_point() + 
  scale_x_continuous(breaks = c(4, 6, 8)) + 
  labs(title = "title", subtitle = "subtitle", caption = "caption") + 
  theme_minimal()

p2 <- mutate(mtcars, 
             am = if_else(am == 0, "auto", "manual") %>% 
               factor(),
             vs = if_else(vs == 0, "V-shaped", "Straight") %>% 
               factor()) %>% 
  tibble::rownames_to_column("name") %>% 
  select(am, mpg, cyl) %>% 
  ggplot(aes(x = cyl, y = mpg, color = am)) +
  geom_point() + 
  scale_x_continuous(breaks = c(4, 6, 8)) + 
  labs(title = "title", subtitle = "subtitle", caption = "caption") + 
  theme_nsse()
  
grid.arrange(p1, p2)

