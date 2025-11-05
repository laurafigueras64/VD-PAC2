## AIRPORTS
airports <- read_csv("data/airports.csv", 
                     col_names = FALSE,
                     col_types = cols(X5 = col_skip(),
                                      X6 = col_skip(),
                                      X7 = col_skip(),
                                      X8 = col_skip(),
                                      X9 = col_skip(),
                                      X10 = col_skip(),
                                      X11 = col_skip(),
                                      X13 = col_skip(),
                                      X14 = col_skip()), 
                     skip = 9)

# put names to columns
colnames(airports) <- c("ID", "Name", "City", "Country", "tz_timezone")

# get only european airports
airports <- airports[grepl("^Europe", airports$tz_timezone), ]
airports$tz_timezone <- NULL

## ROUTES
routes <- read_csv("data/routes.csv",
                   col_names = FALSE, 
                   col_types = cols(X1 = col_skip(),
                                    X2 = col_skip(),
                                    X4 = col_integer(),
                                    X6 = col_integer(),
                                    X7 = col_skip(),
                                    X9 = col_skip()), 
                   skip = 9)

# put names to columns
colnames(routes) <- c("Source", "Source_ID", "Destination", "Destination_ID", "Stops")

# join airports with routes to get routes with country source and destination
df <- routes %>%
  left_join(
    airports %>% select(
      ID, Country
    ),
    by = c("Source_ID" = "ID")
  ) %>%
  rename(Source_Country = Country) %>%
  
  left_join(
    airports %>% select(
      ID, Country
    ),
    by = c("Destination_ID" = "ID")
  ) %>%
  rename(Destination_Country = Country) %>%
  
  filter(!is.na(Source_Country), !is.na(Destination_Country)) %>%
  
  select(Source_Country, Destination_Country)

# sum number of routes per countrys
df <- df %>%
  group_by(Source_Country, Destination_Country) %>%
  summarise(Count = n()) %>%
  arrange(desc(Count)) %>%
  filter(Source_Country != Destination_Country)

# keep only the most visited 100 countries
get_top_countries <- function(edges, n) {
  edges <- edges %>% arrange(desc(Count))
  picked <- character()
  
  for (i in seq_len(nrow(edges))) {
    e <- edges[i, ]
    for (v in c(e$Source_Country, e$Destination_Country)) {
      if (length(picked) >= n) break
      if (!(v %in% picked)) picked <- c(picked, v)
    }
    if (length(picked) >= n) break
  }
  
  tibble(Rank = seq_along(picked), Country = picked)
}

top_Countries <- get_top_countries(df, 5)

df <- df %>%
  filter(
    Source_Country %in% top_Countries$Country &
    Destination_Country %in% top_Countries$Country
  )

# keep just unidirectional routes
df <- df %>%
  rowwise() %>%
  mutate(pair_id = paste(sort(c(Source_Country, Destination_Country)), collapse = "_")) %>%
  ungroup() %>%
  group_by(pair_id) %>%
  slice_max(order_by = Count, n = 1) %>%
  ungroup() %>%
  select(-pair_id)

## SANKEY PLOT
countries <- top_Countries$Country
# 1) Build nodes: duplicate countries, one copy for LEFT and one for RIGHT
n <- length(countries)
nodes <- tibble(
  name  = c(paste0(countries), paste0(countries)),
  side  = rep(c("left","right"), each = n),
  label = rep(countries, 2)  # not used by networkD3, but handy to keep
)

# 2) Create lookup indices (0-based) for each side
left_index  <- setNames(seq(0, n-1), countries)
right_index <- setNames(seq(n, 2*n-1), countries)

# 3) Build links mapped to left->right copies
links <- df %>%
  transmute(
    source = left_index[Source_Country],
    target = right_index[Destination_Country],
    value  = Count
  )

# 4) Plot Sankey (two columns: left sources -> right destinations)
sankeyNetwork(
  Links = links,
  Nodes = nodes,
  Source = "source",
  Target = "target",
  Value  = "value",
  NodeID = "name",
  fontSize = 12,
  nodeWidth = 20,
  sinksRight = TRUE
)
