# Packages ----------------------------------------------------------------

library(ggplot2)
library(showtext)
library(cropcircles)
library(ggimage)


# Fonts -------------------------------------------------------------------

font_add_google("Source Sans 3", "Source")
showtext_auto()
showtext_opts(dpi = 300)


# Functions ---------------------------------------------------------------

is_even <- function(x) {
  return((x %% 2) == 0)
}

make_hex_coords <- function(x0, y0, r) {
  angles <- seq(pi / 6, 2 * pi + pi / 6, length.out = 7)
  hexagon_coords <- function(xc, yc, rad, id) {
    x <- xc + rad * cos(angles)
    y <- yc + rad * sin(angles)
    data.frame(x = x, y = y, grp = id, x_grp = xc, y_grp = yc)
  }
  result <- do.call(
    rbind,
    mapply(hexagon_coords, x0, y0, r, seq_along(x0),
           SIMPLIFY = FALSE
    )
  )
  
  return(result)
}


# Parameters --------------------------------------------------------------

n_x <- 5
n_y <- 3

col_palette <- c("#FBEBFF", "#E999FF", "#9300B8", "#400052")
bg_col <- "#200029"
body_font <- "Source"

padding <- 20
width <- 5


# Generate data -----------------------------------------------------------

inputs <- expand.grid(
  x = seq(1, length.out = n_x, by = 1),
  y = seq(1, length.out = n_y, by = 1)
) |>
  tibble::as_tibble() |>
  dplyr::mutate(
    x = dplyr::if_else(
      is_even(y),
      x + 0.5,
      x
    )
  )
col_df <- data.frame(
  col_grp = seq(1, n_x + 0.5, by = 0.5),
  color = rev(grDevices::colorRampPalette(col_palette)(n_x * 2)),
  alpha = seq(0.2, 0.6, length.out = n_x * 2)
)
output <- make_hex_coords(
  x0 = inputs$x,
  y0 = inputs$y,
  r = rep(0.5, n_x * n_y)
) |>
  tibble::as_tibble() |>
  dplyr::left_join(col_df, by = c("x_grp" = "col_grp"))


# Subplots ----------------------------------------------------------------

g1 <- ggplot() +
  geom_col(
    data = data.frame(
      x = LETTERS[1:3],
      y = c(2, 5, 3)
    ),
    mapping = aes(x = x, y = y),
    fill = bg_col
  ) +
  theme_void() +
  theme(
    plot.background = element_rect(
      fill = "white", color = "white"
    ),
    axis.line.x.bottom = element_line(color = bg_col, linewidth = 1),
    axis.line.y.left = element_line(color = bg_col, linewidth = 1),
    plot.margin = margin(30, 30, 30, 30),
    aspect.ratio = 1
  )
tmp_a <- tempfile()
ggsave(tmp_a, g1,
       device = "png",
       height = 400, width = 400,
       dpi = 300, bg = bg_col,
       units = "px"
)
img_cropped_a <- crop_hex(tmp_a, bg_fill = "white")


# Plot --------------------------------------------------------------------

ggplot() +
  geom_polygon(
    data = output,
    mapping = aes(
      x = x, y = y, group = grp,
      color = alpha(color, alpha)
    ),
    fill = "transparent",
    linewidth = 0.4
  ) +
  geom_image(
    data = output[1,],
    aes(
      x = 4.5,
      y = 2,
      image = img_cropped_a
    ),
    size = 0.28
  ) +
  scale_color_identity() +
  scale_y_reverse() +
  coord_fixed(expand = FALSE, clip = "off") +
  theme_void() +
  theme(
    plot.background = element_rect(fill = bg_col, color = bg_col),
    plot.margin = margin(-padding, -padding, -padding, -padding)
  )


# Save --------------------------------------------------------------------

if (interactive()) {
  ggsave("RLadiesRome/2025-11-04/images/bg.png",
         height = (9/16)*width, width = width,
         dpi = 300, bg = bg_col
  )
}
