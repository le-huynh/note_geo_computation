#'---
#' title: Chap09 - Maps
#' author: ""
#' date: ""
#' output:
#'  github_document
#'---

#+ message=FALSE
pacman::p_load(
    here,           # locate files 
    tidyverse,      # data management and visualization
    sf,
    terra,
    spData,
    spDataLarge,
    tmap,
    grid,
    leaflet,
    mapview
)

#' # Data
# data #-----------
nz_elev = rast(system.file("raster/nz_elev.tif", package = "spDataLarge"))

nz_elev

#' # Static maps
# static maps #-----------
#' ## `tmap` basics
## tmap basics #--------------------

#' Add fill layer to nz shape
tm_shape(nz) +
    tm_fill() 

#' Add border layer to nz shape
tm_shape(nz) +
    tm_borders() 

#' Add fill and border layers to nz shape
tm_shape(nz) +
    tm_fill() +
    tm_borders() 

#' `qtm(nz)` = `tm_shape(nz) + tm_fill() + tm_borders()`
qtm(nz)

qtm(nz_height)

qtm(nz) + qtm(nz_height)

#' ## Map objects
## map objects #-------------------
(map_nz <- tm_shape(nz) + tm_polygons())
class(map_nz)

(map_nz1 <- map_nz +
    tm_shape(nz_elev) + 
    tm_raster(col_alpha = 0.7))

(nz_water <- st_union(nz) %>% 
    st_buffer(22200) %>% 
    st_cast(to = "LINESTRING"))

(map_nz2 <- map_nz1 +
    tm_shape(nz_water) + 
    tm_lines())

(map_nz3 = map_nz2 +
    tm_shape(nz_height) + 
    tm_symbols())

tmap_arrange(map_nz1, map_nz2, map_nz3)

ma1 = tm_shape(nz) + tm_polygons(fill = "red")
ma2 = tm_shape(nz) + tm_polygons(fill = "red", fill_alpha = 0.3)
ma3 = tm_shape(nz) + tm_polygons(col = "blue")
ma4 = tm_shape(nz) + tm_polygons(lwd = 3)
ma5 = tm_shape(nz) + tm_polygons(lty = 2)
ma6 = tm_shape(nz) + tm_polygons(fill = "red", 
                                 fill_alpha = 0.3,
                                 col = "blue", 
                                 lwd = 3, lty = 2)
tmap_arrange(ma1, ma2, ma3, ma4, ma5, ma6)

plot(st_geometry(nz), col = nz$Land_area)

tm_shape(nz) + tm_fill(fill = "Land_area")

#' ## Scales
## scales #---------------------
tm_shape(nz) + tm_polygons(fill = "Median_income")

tm_shape(nz) + tm_polygons(fill = "Median_income",
                           fill.scale = tm_scale(breaks = c(0, 30000, 40000, 50000)))

tm_shape(nz) + tm_polygons(fill = "Median_income",
                           fill.scale = tm_scale(n = 10))

tm_shape(nz) + tm_polygons(fill = "Median_income",
                           fill.scale = tm_scale(values = "BuGn"))

#' Colors
tm_shape(nz) + 
    tm_polygons("Median_income", fill.scale = tm_scale(values = "greens"))

tm_shape(nz) + 
    tm_polygons("Median_income", fill.scale = tm_scale(values = "yl_gn_bu"))

tm_shape(nz) + 
    tm_polygons("Median_income",
                fill.scale = tm_scale_continuous(values = "pu_gn_div", 
                                                 midpoint = 28000))

#' ## Legends
## legends #----------------
legend_title = expression("Area (km"^2*")")
tm_shape(nz) +
    tm_polygons(fill = "Land_area", fill.legend = tm_legend(title = legend_title))

tm_shape(nz) +
    tm_polygons(fill = "Land_area",
                fill.legend = tm_legend(title = legend_title,
                                        orientation = "landscape",
                                        position = tm_pos_out("center", "bottom")))

#' ## Layouts
## layouts #-----------------
map_nz + 
    tm_graticules() +
    tm_compass(type = "8star", position = c("left", "top")) +
    tm_scalebar(breaks = c(0, 100, 200), text.size = 1, position = c("left", "top")) +
    tm_title("New Zealand")

map_nz + tm_layout(scale = 4)

map_nz + tm_layout(bg.color = "lightblue")

map_nz + tm_layout(frame = FALSE)

#' ## Facets
## facets #--------------------
(urb_1970_2030 = urban_agglomerations |> 
    filter(year %in% c(1970, 1990, 2010, 2030)))

tm_shape(world) +
    tm_polygons() +
    tm_shape(urb_1970_2030) +
    tm_symbols(fill = "black", col = "white", size = "population_millions") +
    tm_facets_wrap(by = "year", nrow = 2)

#' ## Inset maps
## inset #--------------------
#' ### New Zealand’s Southern Alps
### New Zealand #-------------
#' **Step 01**: define the area of interest
nz_region = st_bbox(c(xmin = 1340000, xmax = 1450000,
                      ymin = 5130000, ymax = 5210000),
                    crs = st_crs(nz_height)) |> 
    st_as_sfc()
nz_region

#' **Step 02**: base-map showing New Zealand’s Southern Alps area
nz_height_map = tm_shape(nz_elev, bbox = nz_region) +
    tm_raster(col.scale = tm_scale_continuous(values = "YlGn"),
              col.legend = tm_legend(position = c("left", "top"))) +
    tm_shape(nz_height) + tm_symbols(shape = 2, col = "red", size = 1) +
    tm_scalebar(position = c("left", "bottom"))
nz_height_map

#' **Step 03**: create inset map
nz_map = tm_shape(nz) + tm_polygons() +
    tm_shape(nz_height) + tm_symbols(shape = 2, col = "red", size = 0.1) + 
    tm_shape(nz_region) + tm_borders(lwd = 3) +
    tm_layout(bg.color = "lightblue")
nz_map

#' **Step 04**: calculate the aspect ratios of two main datasets
norm_dim = function(obj){
    bbox = st_bbox(obj)
    width = bbox[["xmax"]] - bbox[["xmin"]]
    height = bbox[["ymax"]] - bbox[["ymin"]]
    w = width / max(width, height)
    h = height / max(width, height)
    return(unit(c(w, h), "snpc"))
}

(main_dim = norm_dim(nz_region))

(ins_dim = norm_dim(nz))

#' **Step 05**: pecify the sizes and locations of two maps
main_vp = viewport(width = main_dim[1], height = main_dim[2])

# make the inset map twice smaller as the main
ins_vp = viewport(width = ins_dim[1] * 0.5, height = ins_dim[2] * 0.5,
                  x = unit(1, "npc") - unit(0.5, "cm"), y = unit(0.5, "cm"),
                  just = c("right", "bottom"))

#' **Step 06**: combine two maps
grid.newpage()
print(nz_height_map, vp = main_vp)
pushViewport(main_vp)
print(nz_map, vp = ins_vp)

#' **Step 07**: save the map
# tmap_save()

#' ### United States
### United States #-------------
#' US map
us_states_map = tm_shape(us_states, crs = "EPSG:9311") + 
    tm_polygons() + 
    tm_layout(frame = FALSE)
us_states_map

#' Hawaii map
hawaii_map = tm_shape(hawaii) +
    tm_polygons() + 
    tm_title("Hawaii") +
    tm_layout(frame = FALSE, bg.color = NA, 
              title.position = c("LEFT", "BOTTOM"))
hawaii_map

#' Alaska map
alaska_map = tm_shape(alaska) +
    tm_polygons() + 
    tm_title("Alaska") +
    tm_layout(frame = FALSE, bg.color = NA)
alaska_map

#' Combine maps
us_states_map
print(hawaii_map, vp = grid::viewport(0.35, 0.1, width = 0.2, height = 0.1))
print(alaska_map, vp = grid::viewport(0.15, 0.15, width = 0.3, height = 0.3))

#' # Animated maps
# animated maps #-----------
urb_anim = tm_shape(world) + tm_polygons() + 
    tm_shape(urban_agglomerations) + tm_symbols(size = "population_millions") +
    tm_facets_wrap(by = "year", nrow = 1, ncol = 1, free.coords = FALSE)

tmap_animation(urb_anim,
               filename = here("geo_computation/chap09/urb_anim.gif"),
               delay = 25)

#' # Interactive maps
# interactive maps #-----------
#' ## `tmap::tmap_mode("view")`
## tmap::tmap_mode("view") #---------

# tmap::tmap_mode("view")
# 
# map_nz
# 
# alaska_map

#' `tmap::tmap_leaflet()`
# map_nz + tm_basemap(server = "OpenTopoMap")
# 
# (world_coffee = left_join(world, coffee_data, by = "name_long"))
# facets = c("coffee_production_2016", "coffee_production_2017")
# tm_shape(world_coffee) + 
#     tm_polygons(facets) + 
#     tm_facets_wrap(nrow = 1, sync = TRUE)

#' Switch back to plotting mode
tmap::tmap_mode("plot")

#' ## `mapview::mapview()`
## mapview::mapview() #---------
# mapview::mapview(nz)

# oberfranken = subset(franconia, district == "Oberfranken")
# trails |>
#     st_transform(st_crs(oberfranken)) |>
#     st_intersection(oberfranken) |>
#     st_collection_extract("LINESTRING") |>
#     mapview(color = "red", lwd = 3, layer.name = "trails") +
#     mapview(franconia, zcol = "district") +
#     breweries

