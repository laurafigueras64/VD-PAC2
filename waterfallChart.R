# ---- Setup ----
library(tidyverse)
library(waterfalls)

df <- read_delim("data/housing_price_index.csv", 
                              delim = ";",
                              escape_double = FALSE,
                              col_names = FALSE, 
                              col_types = cols(X1 = col_skip(),
                                               X2 = col_skip(), 
                                               X3 = col_skip(),
                                               X4 = col_skip(), 
                                               X5 = col_skip(),
                                               X6 = col_skip()), trim_ws = TRUE, 
                              skip = 1)

clean_df <- df %>%
  # 1. Rename columns
  rename(period = X7, value = X8) %>%
  
  # 2. Convert decimal comma to numeric
  mutate(
    value = as.numeric(gsub(",", ".", value))
  ) %>%
  
  # 3. Filter only rows from 2020 to 2022
  filter(as.numeric(substr(period, 1, 4)) >= 2020) %>%
  filter(as.numeric(substr(period, 1, 4)) <= 2022)

# Prepare the input for the waterfall function
wf <- clean_df %>%
  mutate(
    year    = as.integer(substr(period, 1, 4)),
    quarter = as.integer(str_extract(period, "(?<=T)\\d+"))
  ) %>%
  arrange(year, quarter) %>%
  select(period, value)

# Waterfall chart
wf_plot <- waterfall(wf,
            calc_total = TRUE,
            linetype = NA,
            rect_border = NA) +
    labs(title = "Housing Price Index (2020 - 2022",
         x = "Quarter",
         y = "Index") +
    theme_minimal()

# Save the plot as a PNG
ggsave("housing_waterfall.png", wf_plot, width = 8, height = 5, dpi = 300)
