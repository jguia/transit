library(tidyverse)
library(tidycensus)
library(tidytransit)
options(tigris_use_cache = TRUE)

census_api_key("8bd7df545848f3b6492fa75b84e6281ef42ca70b",TRUE, TRUE)


gtfs_catalog <- read_csv("https://bit.ly/catalogs-csv") %>%
    filter(location.subdivision_name == "New York", urls.latest != "NA")


load_gtfs_from_url <- function(url, destfile) {
    download.file(url, destfile = destfile)
    g <- read_gtfs(destfile)
    file.remove(destfile)
    g
}

# take a gtfs and create a tibble that joins the route geometries onto the
# $routes tibble
join_shape <- function(gtfs) {
    sf <- gtfs_as_sf(gtfs, crs = 4326)
    gtfs$trips %>%
        select(route_id, shape_id) %>%
        left_join(gtfs$routes, by = "route_id") %>%
        group_by(route_id) %>%
        slice(1) %>%
        ungroup() %>%
        inner_join(sf$shape, by = "shape_id")
}

gs <- list()
for (i in seq_along(gtfs_catalog)) {
    municipality <- gtfs_catalog$location.municipality[i]
    provider <- gtfs_catalog$provider[i]
    name <- gtfs_catalog$name[i]
    url <- gtfs_catalog$urls.latest[i]

    g <- load_gtfs_from_url(url, "./tmp.zip") %>%
        join_shape %>%
        mutate(municipality = municipality, provider = provider, name = name)
    gs[[i]] <- g
}

routes <- bind_rows(gs)

## census population data for NY State
NY <- get_acs(
  state = "NY",
  geography = "tract",
  variables = "B19013_001",
  geometry = TRUE,
  year = 2020
)

## plot it together
ggplot() +              
  geom_sf(data = NY, aes(fill = estimate), color = NA) +
  geom_sf(data = routes$geometry, color = "black") +
  scale_fill_viridis_c(option = "plasma") 
          

