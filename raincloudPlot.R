# ---- Setup ----
library(cowplot)
library(ggplot2)
library(dplyr)
library(tidyr)
library(tibble)

# --- helper: flat violin geom (only if not already defined) -------------------
if (!exists("GeomFlatViolin")) {
  GeomFlatViolin <- ggplot2::ggproto(
    "GeomFlatViolin", ggplot2::Geom,
    setup_data = function(data, params) {
      data$width <- data$width %||% params$width %||%
        (resolution(data$x, FALSE) * 0.9)
      transform(data,
                ymin = y, ymax = y,
                xmin = x, xmax = x + width / 2
      )
    },
    draw_group = function(data, panel_scales, coord) {
      data <- transform(data, xminv = x, xmaxv = x + violinwidth * (xmax - x))
      newdata <- rbind(
        arrange(transform(data, x = xmaxv), y),
        arrange(transform(data, x = xminv), -y)
      )
      newdata <- rbind(newdata, newdata[1, ])
      ggplot2:::ggname("geom_flat_violin",
                       ggplot2::GeomPolygon$draw_panel(newdata, panel_scales, coord)
      )
    },
    draw_key = ggplot2::draw_key_polygon,
    default_aes = ggplot2::aes(weight = 1, colour = "grey20", fill = "white",
                               size = 0.5, alpha = NA, linetype = "solid"),
    required_aes = c("x", "y")
  )
  geom_flat_violin <- function(mapping = NULL, data = NULL, stat = "ydensity",
                               position = "identity", ..., trim = TRUE,
                               scale = "area", show.legend = NA,
                               inherit.aes = TRUE) {
    ggplot2::layer(
      data = data, mapping = mapping, stat = stat,
      geom = GeomFlatViolin, position = position, show.legend = show.legend,
      inherit.aes = inherit.aes, params = list(trim = trim, scale = scale, ...)
    )
  }
}

set.seed(64)

# --- simulate data ---

n <- 500  # points per distribution 

# 1) Normal(0,1)
x_norm <- rnorm(n, mean = 0, sd = 1)

# 2) Bimodal with overall mean 0 and sd 1
d <- 1.2
s <- sqrt(1 - d^2 / 2)
x_bi <- c(rnorm(n/2, -d, s), rnorm(n/2, d, s))

# 3) Asymmetric (skewed) with mean 0 and sd 1
x_asym <- rexp(n, rate = 1) - 1

# --- tidy data for plotting ---
simdat_long <- tibble(
  Normal     = x_norm,
  Bimodal    = x_bi,
  Asymmetric = x_asym
) |>
  pivot_longer(
    cols = everything(),
    names_to = "group",
    values_to = "score"
  ) |>
  mutate(
    group = factor(group, levels = c("Normal", "Bimodal", "Asymmetric"))
  )

# (Optional) quick check that sample means/sds are near 0/1
simdat_long |>
  group_by(group) |>
  summarise(mean = mean(score), sd = sd(score), .groups = "drop")


# --- plot: rainclouds ---
# Notes:
# - flat violin ("cloud") nudged right by +0.25
# - jittered points ("rain") centered
# - boxplot centered at x + 0.25 to align with the cloud
w <- 7; h <- 4.5  # size for saving

p6 <- ggplot(simdat_long, aes(x = group, y = score, fill = group, colour = group)) +
  geom_flat_violin(
    position = position_nudge(x = 0.25, y = 0),
    adjust = 2, trim = FALSE
  ) +
  geom_point(
    position = position_jitter(width = 0.15, height = 0, seed = 64),
    size = 0.25, alpha = 0.6
  ) +
  geom_boxplot(
    aes(x = as.numeric(group) + 0.25, y = score),
    width = 0.1, outlier.shape = NA, alpha = 0.3, colour = "black"
  ) +
  coord_flip() +
  labs(
    x = NULL,
    y = "Value",
    title = "Rainclouds (mean = 0, sd = 1)"
  ) +
  scale_colour_brewer(palette = "Dark2") +
  scale_fill_brewer(palette = "Dark2") +
  theme_cowplot() +
  guides(fill = "none", colour = "none")

ggsave("rainclouds_three_groups.png", p6, width = w, height = h, bg = "white")
p6
