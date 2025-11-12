#'---
#' title: Chap04 - Spatial data operations
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
#' ## Subset data
## subset data #--------------------
nz
plot(nz)

nz_height
plot(nz_height)

# option 1
(canterbury <- nz |> filter(Name == "Canterbury"))
(canterbury_height <- nz_height[canterbury, ])

# option 2
(sel_sgbp <- st_intersects(x = nz_height, y = canterbury))
class(sel_sgbp)

(sel_logical <- lengths(sel_sgbp) > 0)
(canterbury_height2 <- nz_height[sel_logical, ])

# option 3
(canterbury_height3 <- nz_height %>% 
    st_filter(y = canterbury,
              .predicate = st_intersects))

#' ## Topological relations
## topological relations #---------------------
# create polygon_sfc
polygon_matrix = cbind(
    x = c(0, 0, 1, 1,   0),
    y = c(0, 1, 1, 0.5, 0))
polygon_sfc = st_sfc(st_polygon(list(polygon_matrix)))
polygon_sfc

plot(polygon_sfc)

# create point_sf
point_df = data.frame(
    x = c(0.2, 0.7, 0.4),
    y = c(0.1, 0.2, 0.8))
point_sf = st_as_sf(point_df, coords = c("x", "y"))
point_sf

plot(point_sf)

# plot 2 sf
plot(polygon_sfc)
plot(point_sf, add = TRUE)

#' Which of the points in `point_sf` intersect in some way with 
#' polygon `polygon_sfc`?
st_intersects(point_sf, polygon_sfc)

st_intersects(point_sf, polygon_sfc, sparse = FALSE)

#' Which points lie within the polygon?
st_within(point_sf, polygon_sfc)

#' Which features are on or contain a shared boundary with y?
st_touches(point_sf, polygon_sfc)

#' objects that do not spatially relate in any way to the selecting object
st_disjoint(point_sf, polygon_sfc)

st_disjoint(point_sf, polygon_sfc, sparse = FALSE)[, 1]

#' Features that `almost touch` the selection object
st_is_within_distance(point_sf, polygon_sfc, dist = 0.2, sparse = FALSE)[, 1]
st_is_within_distance(point_sf, polygon_sfc, dist = 0.1, sparse = FALSE)[, 1]

#' ## Distance relations
## distance relations #---------------------
st_distance(point_sf, polygon_sfc)

#' Distances between the first three features in nz_height and 
#' the Otago and Canterbury regions of New Zealand
(co <- filter(nz, grepl("Canter|Otag", Name)))
st_distance(nz_height[1:3, ], co)

#' The second and third points are **in Otago** → `distance = 0`
plot(st_geometry(co)[2])
plot(st_geometry(nz_height)[2:3], add = TRUE)

#' ## Distance-based joins
## distance-based joins #---------------------
#' Join objects: closely related but NOT touch
plot(st_geometry(cycle_hire), col = "blue")
plot(st_geometry(cycle_hire_osm), add = TRUE, pch = 3, col = "red")

any(st_intersects(cycle_hire, cycle_hire_osm, sparse = FALSE))

(sel <- st_is_within_distance(cycle_hire, cycle_hire_osm, 
                            dist = units::set_units(20, "m")))
summary(lengths(sel) > 0)

#' ## Spatial aggregation
## spatial aggregation #--------------------
(nz_agg <- aggregate(x = nz_height, by = nz, FUN = mean))
plot(nz_agg[2])

(nz_agg2 <- st_join(x = nz, y = nz_height) %>% 
    group_by(Name) %>% 
    summarize(elevation = mean(elevation, na.rm = TRUE)))
plot(nz_agg2[2])

#' # Raster data
# raster data #-----------
elev = rast(system.file("raster/elev.tif", package = "spData"))
elev

grain = rast(system.file("raster/grain.tif", package = "spData"))
grain

#' ## Subset data
## subset data #------------------------
#' Find value of the cell that covers a point located at coordinates (0.1, 0.1)
# option 1
(id = cellFromXY(elev, xy = matrix(c(0.1, 0.1), ncol = 2)))
elev[id]

# option 2
terra::extract(elev, matrix(c(0.1, 0.1), ncol = 2))

#' Extract values of `raster1` (elev) that fall within `raster2` (clip)
clip = rast(xmin = 0.9, xmax = 1.8, ymin = -0.45, ymax = 0.45,
            resolution = 0.3, vals = rep(1, 9))
clip

# option 1
elev[clip]

# option 2
terra::extract(elev, ext(clip))

#' Return raster object
elev[clip, drop = FALSE]

#' ## Map algebra
## map algebra #-------------------
#' ### Local operations
### local operations #--------------------
elev + elev

elev^2

log(elev)

elev > 5

#' ### Focal operations
### focal operations #--------------------
# find minimum value in 3-by-3 moving windows
r_focal = focal(elev,
                w = matrix(1, nrow = 3, ncol = 3),
                fun = min)
r_focal

#' ### Zonal operations
### zonal operations #--------------------
# calculate mean elevation associated with each grain-size class
(z = zonal(elev, grain, fun = "mean"))

# return raster object
zonal(elev, grain, fun = "mean", as.raster = TRUE)

