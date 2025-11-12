#'---
#' title: Chap03 - Attribute data operations
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
    skimr,
    sf,             # classes and functions for vector data
    terra,          # classes and functions for raster data
    spData,         # load geographic data
    spDataLarge     # load larger geographic data
)


#' # Vector data
# vector data #-----------
methods(class = "sf")

#' # Raster data
# raster data #-----------
# create elevation raster data: numeric values
elev = rast(nrows = 6, ncols = 6,
            xmin = -1.5, xmax = 1.5, ymin = -1.5, ymax = 1.5,
            vals = 1:36)

elev

# create grain raster data: categorical values
grain_order = c("clay", "silt", "sand")
grain_char = sample(grain_order, 36, replace = TRUE)
grain_fact = factor(grain_char, levels = grain_order)
grain = rast(nrows = 6, ncols = 6, 
             xmin = -1.5, xmax = 1.5, ymin = -1.5, ymax = 1.5,
             vals = grain_fact)

grain

## subset data #-----------
# row 1, column 1
elev[1, 1]

# cell ID 1
elev[1]

# multi-layered raster object
two_layers <- c(grain, elev)
two_layers[1]

# extract all values
values(elev)
values(two_layers)

elev[]

## summarize data #--------
summary(elev)
summary(two_layers)

terra::global(elev)
terra::global(elev, sd)
terra::global(two_layers)

terra::freq(grain)
terra::freq(elev)
terra::freq(two_layers)

## quick plot #-------------
boxplot(elev)
boxplot(two_layers)

density(elev)
density(two_layers)

hist(elev)
hist(two_layers)

pairs(two_layers)

