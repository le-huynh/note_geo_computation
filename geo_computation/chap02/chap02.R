#'---
#' title: Chap02 - Geographic data
#' author: ""
#' date: ""
#' output:
#'  github_document
#'---

#+ message=FALSE
# install.packages("spDataLarge",
#                  repos = "https://geocompr.r-universe.dev")
pacman::p_load(
    rio,            # import and export files
    here,           # locate files 
    tidyverse,      # data management and visualization
    skimr,
    sf,             # classes and functions for vector data
    terra,          # classes and functions for raster data
    spData,         # load geographic data
    spDataLarge     # load larger geographic data
)

#' # Vector data
# vector data #---------------------
#' ## `sf`: Simple feature
## sf #-----------
class(world)

names(world)

world

plot(world)

skimr::skim(world)

summary(world["lifeExp"])

world %>% 
    slice(1:2) %>% 
    select(1:3)

#' ## Basic map
## basic map #-----------
plot(world[3:6])

plot(world["pop"])

#' Add layers to existing map
(world_asia <- world %>% filter(continent == "Asia"))

(asia <- st_union(world_asia))

plot(world_asia["pop"])

plot(asia)

plot(world["pop"], reset = FALSE)
plot(asia, add = TRUE, col = "red")

#' Add points to map
plot(world["continent"], reset = FALSE)

cex <- sqrt(world$pop) / 10000

(world_cents <- st_centroid(world, of_largest = TRUE))

st_geometry(world_cents)

plot(st_geometry(world_cents), add = TRUE, cex = cex)

#' Modify map
(vietnam <- world %>% filter(name_long == "Vietnam"))

plot(vietnam["pop"])

plot(st_geometry(vietnam),
     expandBB = c(0, 0.2, 0.1, 1),
     col = "darkolivegreen1",
     lwd = 3)
plot(st_geometry(world_asia), add = TRUE)

#' # Raster data
# raster data #---------------------
#' ## `terra::`
## terra #-------------
raster_filepath = system.file("raster/srtm.tif", package = "spDataLarge")

my_rast = rast(raster_filepath)

class(my_rast)

my_rast

# number of rows, columns and layers
dim(my_rast)

# number of cells
ncell(my_rast)

# spatial resolution
res(my_rast)

# spatial extent
ext(my_rast)

# CRS
crs(my_rast)

# number of layers
nlyr(my_rast)

# data location: in memory or on disk?
inMemory(my_rast)

# data location
sources(my_rast)

#' ## Basic map
## basic map #-----------
plot(my_rast)

#' ## Raster with multiple layers
## multi-layer #---------------
multi_raster_file = system.file("raster/landsat.tif", package = "spDataLarge")

multi_rast = rast(multi_raster_file)

multi_rast

names(multi_rast)

# select layer
multi_rast[[1]]

multi_rast[["landsat_1"]]

multi_rast$landsat_1

subset(multi_rast, 1)

subset(multi_rast, "landsat_1")

