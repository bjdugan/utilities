#nsse colors

library(tidyr)
library(tibble)
library(dplyr)
library(purrr)

# col2rgb makes a named integer, this cleans that into tibble
tidy_colors <- function(x) {
  nm <- if (is.null(names(x))) {x} else names(x)

  col2rgb(x) %>%
    rbind(rgb2hsv(.)) %>%
    `colnames<-`(nm) %>%
    t() %>%
    data.frame() %>%
    rownames_to_column(var = "color") %>%
    as_tibble()

}

r_colors <- tidy_colors(colors())

NSSEcolors <- list(
  standard4 = tidy_colors(c("nsse_navy" = "#002D6D",
                 "nsse_gold" = "#EFAA22",
                 "nsse_blue" = "#4189dd",
                 "nsse_plum" = "#7a1a57")),
  standard7 = tidy_colors(c(
    "nsse_navy" = "#002D6D",
    "nsse_darkgrey" = "#645950",
    "nsse_gold" = "#EFAA22",
    "nsse_darkorange" = "#855723",
    "nsse_blue" = "#4189dd",
    "nsse_lightgrey" = "#D9D9D9", #CCCCCC might be too dark for Bob's; rgb(217, 217, 217, .5, maxColorValue = 255)
    "nsse_plum" = "#7a1a57")),
  standard4_grad5 = lapply(list(
    # from htmlcsscolor.com/hex, picking every other, where base color is middle, light to dark
    nsse_navy = c("#97a9c3", "#5C79a1", "#002d6d", "#001d46", "#00122d"),
    nsse_gold = c("#f9dda4","#f5c971" , "#efaa22", "#996d16", "#62460e"),
    nsse_blue = c("#b1cff1", "#85b4e9", "#4189dd", "#2a588e", "#1b385b"),
    nsse_plum = c("#C9A1BA", "#AA6D94", "#7a1a57", "#4E1138", "#320B24")), tidy_colors)) %>% 
  map_if(is.data.frame, mutate, hex = rgb(red, green, blue, maxColorValue = 255)) 









