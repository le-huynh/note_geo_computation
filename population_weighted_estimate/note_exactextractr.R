#'---
#' title: Summarizing gridded population data
#' author: ""
#' date: ""
#' output:
#'  github_document
#'---

#+ message=FALSE
pacman::p_load(
    rio,            # import and export files
    here,           # locate files 
    tidyverse,      # data management and visualization
    exactextractr,
    raster,
    sf
)

#' [Summarizing gridded population data](https://isciences.gitlab.io/exactextractr/articles/vig1_population.html)  
#' - Region: São Miguel, the largest and most populous island of the Azores archipelago.  
#' - Population data: [Gridded Population of the World](https://www.earthdata.nasa.gov/data/projects/gpw)  
#' - Elevation data: [EU-DEM](https://www.eea.europa.eu/data-and-maps/data/copernicus-land-monitoring-service-eu-dem)  
#' **Note:** six _concelhos_ = six municipalities 

#---
#' # Data
# data #-----------
#' ## Geographical boundary
concelhos <- st_read(system.file('sao_miguel/concelhos.gpkg',
                                 package = 'exactextractr'),
                     quiet = TRUE)
concelhos

#' ## Population data
#' Count data
pop_count <- raster(system.file('sao_miguel/gpw_v411_2020_count_2020.tif',
                                package = 'exactextractr'))
pop_count

plot(pop_count, axes = FALSE)
plot(st_geometry(concelhos), add = TRUE)

#' Density data: number of persons per square kilometer of land area in each pixel
pop_density <- raster(system.file('sao_miguel/gpw_v411_2020_density_2020.tif',
                                  package = 'exactextractr'))
pop_density

plot(pop_density, axes = FALSE)
plot(st_geometry(concelhos), add = TRUE)

#' ## Elevation data
elev <- raster(system.file('sao_miguel/eu_dem_v11.tif',
                           package = 'exactextractr'))
elev

plot(elev, axes = FALSE, box = FALSE)
plot(st_geometry(concelhos), add = TRUE)

#' => Population data and elevation data have different resolutions

#---
#' # Calculate population
# calculate population #-----------------------
#' ## Total population
cellStats(pop_count, 'sum')

#' ## From population count file
exact_extract(x = pop_count,
              y = concelhos, 
              fun = 'sum',
              progress = FALSE)

exact_extract(x = pop_count,
              y = concelhos, 
              fun = 'sum',
              append_cols = "name",
              progress = FALSE)

exact_extract(x = pop_count,
              y = concelhos, 
              fun = 'sum',
              append_cols = "name",
              progress = FALSE) %>% 
    janitor::adorn_totals()

#' Total population = 131864 < 145603  
#' → 9% of the population is unaccounted for in the concelho totals.  
#' → in southern coast, many cells are only partially covered by the concelho boundaries.  

#---
#' ## From population density file
#' population count = population density x area of each cell

exact_extract(pop_density,
              concelhos, 
              function(density, frac, area) {
                  sum(density * frac * area)
              }, 
              weights = raster::area(pop_density),
              append_cols = 'name',
              progress = FALSE) %>% 
    janitor::adorn_totals()
#' Limitation: pre-compute an area raster → store it in memory 
#' → load all raster values intersecting a given polygon into memory at a single time.

# Solution: recommended!!!
exact_extract(x = pop_density,
              y = concelhos,
              fun = 'weighted_sum',
              weights = 'area',
              append_cols = 'name',
              progress = FALSE) %>% 
    janitor::adorn_totals()

#' # Population-weighted statistics
# population-weighted estimate #----------------------
#' Calculate the average elevation of a residence in each of the six concelhos.  

#' Assume all pixel areas to be equivalent
exact_extract(x = elev,
              y = concelhos,
              fun = 'weighted_mean',
              weights = pop_density,
              append_cols = 'name',
              progress = FALSE)

#' Pixel areas vary across the region
exact_extract(x = elev,
              y = concelhos,
              function(df) {
                  weighted.mean(x = df$value,
                                w = df$coverage_fraction * df$pop_density * df$area,
                                na.rm = TRUE)},
              weights = stack(list(pop_density = pop_density,
                                   area = area(pop_density))),
              summarize_df = TRUE,
              progress = FALSE,
              append_cols = 'name')
#' Limitation: pre-compute an area raster → store it in memory 
#' → load all raster values intersecting a given polygon into memory at a single time.

# Solution: recommended!!!
#' `coverage_area = TRUE`: all calculations use the area of each cell that is 
#' covered by the polygon instead of the fraction of each cell that is 
#' covered by the polygon
exact_extract(elev,
              concelhos,
              c('mean', 'weighted_mean'),
              weights = pop_density,
              coverage_area = TRUE, 
              append_cols = 'name',
              progress = FALSE)

