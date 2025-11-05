# ---- Setup ----
library(readr)
library(dplyr)
library(stringr)
library(tidyr)
library(networkD3)

# Parameters
n_top <- 5
airports_path <- "data/airports.csv"
routes_path   <- "data/routes.csv"

# ---- AIRPORTS ----
# Read only the columns we’ll use and discard the rest early for speed/memory.
# After skipping, the useful columns are:
# X1 = ID, X2 = Name, X3 = City, X4 = Country, X12 = tz_timezone
airports <- read_csv(
  airports_path,
  col_names = FALSE,
  col_types = cols(
    X1  = col_integer(),  # ID
    X2  = col_skip(),
    X3  = col_skip(),
    X4  = col_character(),# Country
    X5  = col_skip(),
    X6  = col_skip(),
    X7  = col_skip(),
    X8  = col_skip(),
    X9  = col_skip(),
    X10 = col_skip(),
    X11 = col_skip(),
    X12 = col_character(),# tz_timezone
    X13 = col_skip(),
    X14 = col_skip()
  ),
  skip = 9
) %>%
  rename(
    ID          = X1,
    Country     = X4,
    tz_timezone = X12
  ) %>%
  # Keep only European airports (timezone string begins with "Europe")
  filter(str_starts(tz_timezone, "Europe")) %>%
  select(ID, Country) # keep just what we need for the joins

# ---- ROUTES ----
# Keep only source/destination & IDs; discard the rest early.
# Based on your mapping after skipping: X3=Source, X4=Source_ID, X5=Destination, X6=Destination_ID, X8=Stops
routes <- read_csv(
  routes_path,
  col_names = FALSE,
  col_types = cols(
    X1 = col_skip(),
    X2 = col_skip(),
    X3 = col_skip(),
    X4 = col_integer(),   # Source_ID
    X5 = col_skip(),
    X6 = col_integer(),   # Destination_ID
    X7 = col_skip(),
    X8 = col_skip(),
    X9 = col_skip()
  ),
  skip = 9
) %>%
  rename(
    Source_ID      = X4,
    Destination_ID = X6,
  )

# ---- Join routes with airport countries (both ends) ----
# We join twice (source & destination) and then keep only routes that
# connect two European airports (non-NA on both joins).
edges <- routes %>%
  left_join(airports %>% select(ID, Country), by = c("Source_ID" = "ID")) %>%
  rename(Source_Country = Country) %>%
  left_join(airports %>% select(ID, Country), by = c("Destination_ID" = "ID")) %>%
  rename(Destination_Country = Country) %>%
  filter(!is.na(Source_Country), !is.na(Destination_Country)) %>%
  select(Source_Country, Destination_Country)

# ---- Aggregate: number of routes per (Source_Country, Destination_Country) ----
edges <- edges %>%
  group_by(Source_Country, Destination_Country) %>%
  summarise(Count = n(), .groups = "drop") %>%
  # No self-loops
  filter(Source_Country != Destination_Country) %>%
  arrange(desc(Count))

# ---- Pick top N countries by total traffic (in + out) ----
top_countries <- edges %>%
  reframe(
    Country = c(Source_Country, Destination_Country),
    W       = c(Count, Count)  # weight each appearance by Count
  ) %>%
  group_by(Country) %>%
  summarise(Traffic = sum(W), .groups = "drop") %>%
  slice_max(order_by = Traffic, n = n_top, with_ties = FALSE)

# Keep only edges where both ends are in the top N countries
edges_top <- edges %>%
  semi_join(top_countries, by = c("Source_Country" = "Country")) %>%
  semi_join(top_countries, by = c("Destination_Country" = "Country"))

# ---- Collapse bidirectional pairs to the higher-count direction ----
# For each unordered country pair, keep only the row with the larger Count.
# This avoids double arrows between the same two countries.
edges_uni <- edges_top %>%
  mutate(
    a = pmin(Source_Country, Destination_Country),
    b = pmax(Source_Country, Destination_Country),
    pair = paste(a, b, sep = " | ")
  ) %>%
  group_by(pair) %>%
  slice_max(order_by = Count, n = 1, with_ties = FALSE) %>%
  ungroup() %>%
  select(Source_Country, Destination_Country, Count)

# ---- Build Sankey nodes & links (two columns: left sources -> right destinations) ----
countries <- top_countries$Country
n <- length(countries)

nodes <- tibble(
  # duplicate country labels: left set first [0..n-1], right set next [n..2n-1]
  name  = c(countries, countries),
  side  = rep(c("left", "right"), each = n),
  label = rep(countries, 2) # not used by networkD3 but handy in case
)

# Map country to 0-based index for the left side; right side is offset by +n
links <- edges_uni %>%
  transmute(
    source = match(Source_Country, countries) - 1L,
    target = n + match(Destination_Country, countries) - 1L,
    value  = Count
  )

# ---- Plot Sankey ----
sankeyNetwork(
  Links   = links,
  Nodes   = nodes,
  Source  = "source",
  Target  = "target",
  Value   = "value",
  NodeID  = "name",
  fontSize = 12,
  nodeWidth = 20,
  sinksRight = TRUE
)
