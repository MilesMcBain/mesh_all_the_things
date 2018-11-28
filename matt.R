library(ceramic) ## hypertidy/ceramic
library(quadmesh) ## hypertidy/quadmesh
library(cartilage) ## hypertidy/cartilage
library(raster)  
library(rgl)
library(r2vr) ## milesmcbain/r2vr
library(r2vr.gis) ## milesmcbain/r2vr.gis
## In your .Renviron: MAPBOX_API_KEY=<YOUR MAPBOX API KEY>


get_loc_elev <- function(loc, buffer = 5000){

  decode_elevation <- function(dat,...) {
    height <-  -10000 + ((dat[[1]] * 256 * 256 + dat[[2]] * 256 + dat[[3]]) * 0.1)
    projection(height) <- "+proj=merc +a=6378137 +b=6378137"
    height
  }

  dem_tiles <-
    ceramic::cc_location(loc = loc,
                         buffer = buffer,
                         type = "mapbox.terrain-rgb",
                         max_tiles = 16,
                         crop_to_buffer = TRUE,
                         debug = TRUE)

  decode_elevation(dem_tiles)
}

get_loc_3d <- function(loc, buffer = 5000){

  tex_tiles_128 <-
  ceramic::cc_location(loc = loc,
                       buffer = buffer,
                       type = "mapbox.satellite",
                       max_tiles = 128,
                       debug = TRUE,
                       crop_to_buffer = TRUE)

  dem <- get_loc_elev(loc, buffer)

  loc_mesh <- quadmesh(dem,
                       texture = tex_tiles_128,
                       texture_filename = "location.png")
  loc_mesh
}

plotVR <- function(q_mesh){


  side_length <-  max(t(q_mesh$vb[1, ])) - min(t(q_mesh$vb[1, ]))

  scale_factor <- 300/side_length

  q_mesh_tris <- triangulate_quads(q_mesh$ib)

  q_mesh_json <- trimesh_to_threejson(t(q_mesh$vb[1:3, ]),
                                      t(q_mesh_tris),
                                      vertex_uvs = t(q_mesh$texcoords),
                                      texture_file = q_mesh$material$texture)

  readr::write_file(q_mesh_json, "q_mesh.json")

  mesh_asset <-
    a_asset(id = "location",
            src="q_mesh.json",
            .parts = q_mesh$material$texture)

  mesh_model <- a_json_model(id = "mesh",
                             src = mesh_asset, mesh_smooth = TRUE,
                             rotation = c(-90, 0, 0),
                             scale = c(1, 1, 1) * scale_factor)

  controls <- a_pc_control_camera(acceleration = 300)

  mesh_scene <- a_scene(.template = "empty",
                        .children = list(mesh_model,
                                         controls))

  a_kill_all_scenes()
  mesh_scene$serve()

}

## Example locations
rio_grande_gorge<- c(-105.732960, 36.475326)
mt_tibbro <- c(152.946954, -26.926598)
monte_tomaro <- c(8.863926, 46.106759)
iceland  <- c(-18.453073, 64.775668)
fuji <- c(138.731212, 35.364927)
hobart <- c(147.326011, -42.880124)
claremont <- c(147.210690, -42.779476)
barron_falls <- c(145.643248, -16.833429)

q_mesh <- get_loc_3d(fuji, 100000)

## VR
plotVR(q_mesh)

## rgl
shade3d(q_mesh)

## Cartilage
elev <- get_loc_elev(rio_grande_gorge, 5000)
rs_plot <- sphere(elev, progbar = FALSE,
                  texture = "desert")






