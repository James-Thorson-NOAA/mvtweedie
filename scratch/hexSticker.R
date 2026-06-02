
setwd( R'(C:\Users\james\OneDrive\Desktop\Git\mvtweedie\scratch)' )

#pak::pak("emilioxavier/hexSticker")
library(hexSticker)

# https://github.com/GuangchuangYu/hexSticker/issues/155
#theme_sticker <- function(size=1.2, ...) {
#    center <- 1
#    radius <- 1
#    h <- radius
#    w <- sqrt(3)/2 * radius
#    m <- 1.05
#    list(
#      theme_transparent() +
#        theme(plot.margin = margin(t=0, r=0, b=0, l=0, unit = "lines"),
#              strip.text = element_blank(),
#              line = element_blank(),
#              text = element_blank(),
#              title = element_blank(), ...),
#      coord_fixed(),
#      scale_y_continuous(expand = c(0, 0), limits = c(center-h*m , center+h*m )),
#      scale_x_continuous(expand = c(0, 0), limits = c(center-w*m , center+w*m ))
#    )
#}

p = sticker(
  subplot = "mvtweedie_plot.png",
  package = "mvtweedie",

  # package text
  p_family = "sans",
  p_size = 20,
  p_color = "white",

  # hexagon
  h_fill = "#1B4332",
  h_color = "#081C15",
  h_size = 1,

  # image positioning
  s_x = 1,
  s_y = 0.9,
  asp = 1.6,
  s_width = 0.8, # Only use s_width or s_height ... both results in only one used

  # text positioning
  p_x = 1,
  p_y = 0.6,
  width = 2500,
  height = 2500,

  filename = "mvtweedie-sticker.png"
) # + theme_sticker()
