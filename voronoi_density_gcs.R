# AIM: to plot voronoi densities and comapre with "regular" density of GCS

deldir_pol = readRDS("/Users/gretatimaite/Desktop/pedestrian_simulation/deldir_pol.RDS")

# first let's "borrow" some density code from `gcs.qmd` notebook

# let's convert GCS environment (walls) into an sf object so we can apply spatial operations
# polygon(x = c(0, 0, 53, 53),
#         y = c(0, 50, 50,0),
#         border = "black",
#         lwd = 2) # draw walls of a GCS
matrix_walls = matrix(c(0,0,0,50*14,53*14,50*14,53*14, 0,0,0),
                      ncol = 2,
                      byrow = TRUE)
matrixlist_walls = list(matrix_walls)
polygon_walls = sf::st_polygon(matrixlist_walls)

# divide gcs polygon by creating a grid
gcs_div = sf::st_make_grid(polygon_walls, 
                           n = 2,
                           what = "polygons")
# convert gcs_div to an sf object
gcs_div_sf = gcs_div |> 
  sf::st_as_sf() |> 
  dplyr::rename(geom = x)

gcs_div_sf |> plot(add = T)

# function 

gcs_deldir_fun = function(gcs = list(),
                          deldir_pol = list()){
  sf::st_intersection(gcs,
                      deldir_pol)
}

gcs_deldir1 = list()
gcs_deldir2 = list()
gcs_deldir3 = list()
gcs_deldir4 = list()
  for (k in 1:length(deldir_pol)){
    if (k > 43 &
        k < length(deldir_pol) + 1){
      gcs_deldir1[[k]] = gcs_deldir_fun(gcs = gcs_div_sf[1,],
                                       deldir_pol = deldir_pol[[k]])
      gcs_deldir2[[k]] = gcs_deldir_fun(gcs = gcs_div_sf[2,],
                                       deldir_pol = deldir_pol[[k]])
      gcs_deldir3[[k]] = gcs_deldir_fun(gcs = gcs_div_sf[3,],
                                       deldir_pol = deldir_pol[[k]])
      gcs_deldir4[[k]] = gcs_deldir_fun(gcs = gcs_div_sf[4,],
                                       deldir_pol = deldir_pol[[k]])
    }
  }

