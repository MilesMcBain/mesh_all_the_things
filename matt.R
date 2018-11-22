library(ceramic)
library(quadmesh)
library(raster)
library(rgl)

get_3d_loc <- function(loc, buffer){

  buffer_meters<- buffer

  decode_elevation <- function(dat,...) {
    height <-  -10000 + ((dat[[1]] * 256 * 256 + dat[[2]] * 256 + dat[[3]]) * 0.1)
    projection(height) <- "+proj=merc +a=6378137 +b=6378137"
    height
  }

  tex_tiles_72 <-
  ceramic::cc_location(loc = loc,
                       buffer = buffer_meters,
                       type = "mapbox.satellite",
                       max_tiles = 72,
                       debug = TRUE,
                       crop_to_buffer = TRUE)

dem_tiles <-
  ceramic::cc_location(loc = loc,
                       buffer = buffer_meters,
                       type = "mapbox.terrain-rgb",
                       max_tiles = 16,
                       crop_to_buffer = TRUE)

dem <- decode_elevation(dem_tiles)

loc_mesh <- quadmesh(dem,
                     texture = tex_tiles_72,
                     texture_filename = "location.png")
plot3d(loc_mesh)
}

rio_grande_gorge<- c(-105.732960, 36.475326)

mt_tibbro <- c(152.946954, -26.926598)

monte_tomaro <- c(8.863926, 46.106759)

iceland  <- c(-18.453073, 64.775668)

fuji <- c(138.731212, 35.364927)

get_3d_loc(fuji, 5000)



