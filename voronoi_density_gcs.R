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

# Now let's figure out how many tiles are in each polygon
# number of tiles (rows) in each dataframe in gcs_deldir indicates the number of agents
# this number will be divided by the estimates area of a polygon

# find out the area size of each polygon
gcs_area = list()
for (i in 1:lengths(gcs_div_sf)){
  gcs_area[[i]] = sf::st_area(gcs_div_sf[[i]] / 14) # convert to metres
  # print(gcs_area)
}
# make it a vector
gcs_area = gcs_area |>
  unlist() |>
  as.vector()

# plot 1
voronoi_density1_temp = vector()
voronoi_density1 = data.frame()
for (i in 1:length(gcs_deldir1)){
  if(is.null(gcs_deldir1[[i]]) == FALSE){
    voronoi_density1_temp[i] = nrow(gcs_deldir1[[i]]) / gcs_area[1]
    voronoi_density1 = as.data.frame("x" = voronoi_density1_temp) |> 
      dplyr::mutate(frame = dplyr::row_number(),
                    sec = frame / 25)
    colnames(voronoi_density1) = c("density", "frame", "sec")
  }
  }

ggplot2::ggplot(voronoi_density1)+
  ggplot2::aes(x = sec,
               y = density)+
  ggplot2::geom_line() +
  ggplot2::geom_line(data = frames_d[[1]], color = "red")

# plot 2
voronoi_density2_temp = vector()
voronoi_density2 = data.frame()
for (i in 1:length(gcs_deldir2)){
  if(is.null(gcs_deldir2[[i]]) == FALSE){
    voronoi_density2_temp[i] = nrow(gcs_deldir1[[i]]) / gcs_area[2]
    voronoi_density2 = as.data.frame("x" = voronoi_density2_temp) |> 
      dplyr::mutate(frame = dplyr::row_number(),
                    sec = frame / 25)
    colnames(voronoi_density2) = c("density", "frame", "sec")
  }
}

ggplot2::ggplot(voronoi_density2)+
  ggplot2::aes(x = sec,
               y = density)+
  ggplot2::geom_line() +
  ggplot2::geom_line(data = frames_d[[2]], color = "red")


# plot 3

voronoi_density3_temp = vector()
voronoi_density3 = data.frame()
for (i in 1:length(gcs_deldir3)){
  if(is.null(gcs_deldir3[[i]]) == FALSE){
    voronoi_density3_temp[i] = nrow(gcs_deldir3[[i]]) / gcs_area[3]
    voronoi_density3 = as.data.frame("x" = voronoi_density3_temp) |> 
      dplyr::mutate(frame = dplyr::row_number(),
                    sec = frame / 25)
    colnames(voronoi_density3) = c("density", "frame", "sec")
  }
}

ggplot2::ggplot(voronoi_density3)+
  ggplot2::aes(x = sec,
               y = density)+
  ggplot2::geom_line() +
  ggplot2::geom_line(data = frames_d[[3]], color = "red")

# lot 4

voronoi_density4_temp = vector()
voronoi_density4 = data.frame()
for (i in 1:length(gcs_deldir4)){
  if(is.null(gcs_deldir4[[i]]) == FALSE){
    voronoi_density4_temp[i] = nrow(gcs_deldir4[[i]]) / gcs_area[4]
    voronoi_density4 = as.data.frame("x" = voronoi_density4_temp) |> 
      dplyr::mutate(frame = dplyr::row_number(),
                    sec = frame / 25)
    colnames(voronoi_density4) = c("density", "frame", "sec")
  }
}

ggplot2::ggplot(voronoi_density4)+
  ggplot2::aes(x = sec,
               y = density)+
  ggplot2::geom_line() +
  ggplot2::geom_line(data = frames_d[[4]], color = "red")
