pacman::p_load(bertin, tidyterra, ggplot2, rgdal, RSQLite, readxl, tidyverse, sf, openxlsx, abjutils, stringdist, scraEP, geosphere, tm, stringr, data.table, plyr, geobr, tidygeocoder, cartogram, raster, ggridges, terra, glue, ggtext, showtext, patchwork, camcorder, install = TRUE)

# Plot size
gg_record(
  dir = file.path(tempdir(),"recording"), 
  device = "png", 
  width = 21, 
  height = 21, 
  units = "cm", 
  dpi = 300 
)

# Short function to create %!in% operator 
'%!in%' <- function(x,y)!('%in%'(x,y))

# 1. Loading Data ----
# Base Map of France
fr <- sf::read_sf('https://github.com/BjnNowak/bertin_tuto/raw/main/map/france_sport.gpkg') |>
  dplyr::rename(geometry = geom)

# Raster Population Density in France
pop <- terra::rast('https://github.com/BjnNowak/bertin_tuto/raw/main/map/france_pop_l93_2.tif') 
names(pop) <- "pop"

# France is a vector layer (sf object) 
ggplot() +
  geom_sf(fr, 
          mapping = aes(geometry = geometry))

# France population is a raster layer (SpatRaster object)
ggplot() + 
  tidyterra::geom_spatraster(data = pop) # Problem with strong heterogeneity of population density in Paris

# 2. Data Cleaning ----
# First Step - Reduce the number of pixels in the raster
nrow(pop)
ncol(pop)

# Map with valued points will not be legivle with such a large number of points. 
# Before converting pixels into points, we will degrade the resolution of the raster

# Compute factor to reduce raster to 100 rows
factor <- round(nrow(pop)/100)

# Degrade the resolution of the raster by the corresponding factor
pop_agg <- terra::aggregate(pop,
                            fact = factor,
                            fun = "mean",
                            na_rm = TRUE)

# Replace potential NAs by 0 
pop_agg[is.na(pop_agg)] <- 0

# Extracting the centroid of each pixel
  # Convert the raster to tibble
  # Convert the tibble obtained into a point vector

pop_tib <- as_tibble(pop_agg,
                     xy = TRUE,    # Get centroid of each cell
                     na.rm = TRUE)

sf_pop <- pop_tib |>
  st_as_sf(coords = c("x", "y"),   # Column with Lat/Lon
           crs = 2154)             # CRS

# sf object that may now be plotted 
ggplot(sf_pop) +
  geom_sf()

# Intersection to keep only points inside the country
sf_pop_metro <- st_intersection(sf_pop, fr)

ggplot(sf_pop_metro) +
  geom_sf()

# Creating density population classes to remove heterogeneity
sf_pop_metro <- sf_pop_metro |>
  mutate(dens = case_when(pop < 20 ~ "A",
                          pop < 70 ~ "B",
                          pop < 140 ~ "C",
                          pop < 200 ~ "D",
                          TRUE ~ "E"))

# 3. Crating the Map ----
ggplot() +
  geom_sf(sf_pop_metro,
          mapping = aes(geometry = geometry, size = dens)) +
  scale_size_manual(values = c(0.5, 1, 1.5, 2, 2.5),
                    label = c("< 20 inhabitants", "20 to 69", "70 to 139", "140 to 100", "≥200 inhabitants"))

# 4. Making Improvements ----
ggplot() +
  geom_sf(
    fr, 
    mapping = aes(geometry = geometry),
    color = "black",
    fill = "white"
  ) +
  geom_sf(
    sf_pop_metro,
    mapping = aes(geometry = geometry, size = dens),
    # Use a point with a border and a fill
    pch = 21,
    color = "white",
    fill = "black",
    stroke = 0.05
  ) +
  scale_size_manual(
    values = c(1, 1.5, 2, 2.5, 3),
    label = c("< 20 inhabitants", "20 to 69", "70 to 139", "140 to 100", "≥200 inhabitants")
  ) +
  # Customize labels
  labs(
    title = "**Population Density in France**",
    subtitle = "in the way of Jacques Bertin",
    size = "**Population density**<br><span style = 'color:grey'>Inhabitants per km<sup>2</sup></span>",
    caption = "**Data** GHSL **| Plot** @Pedro"
  ) +
  theme_void() +
  theme(
    plot.margin = margin(1, 1, 1, 1, "cm"),
    plot.background = element_rect(fill = "white", color = NA),
    legend.position = "right",
    legend.title = element_markdown(),
    legend.text = element_text(),
    plot.title = element_markdown(hjust = 0.5),
    plot.subtitle = element_text(hjust = 0.5, color = "grey40"),
    plot.caption = element_markdown(color = "grey20", hjust = 0.5)
  )
