# AIM: to measure density of the divided gcs env.
# This script builds up on gcs_flow.R script

gcs_walls_area = polygon_walls |> sf::st_area()

gcs_d = frames_sf_m |> 
  sf::st_drop_geometry() |> 
  dplyr::group_by(frame) |> 
  dplyr::summarise(n = dplyr::n(),
                   sec = sec) |> 
  dplyr::mutate(
    density = n / gcs_walls_area
  )

plot_density = ggplot2::ggplot(gcs_d)+
  ggplot2::aes(x = sec,
               y = density)+
  ggplot2::geom_line()

ggplot2::ggplot(gcs_d)+
  ggplot2::aes(x = sec,
               y = density)+
  ggplot2::geom_line() +
  ggplot2::geom_line(data = gcs_d,
                     ggplot2::aes(x = sec,
                                  y = n),
                     col = "red")

# find out the area size of each polygon
gcs_area = list()
for (i in 1:lengths(gcs_div_sf)){
  gcs_area[[i]] = sf::st_area(gcs_div_sf[[i]]) 
  # print(gcs_area)
}
# make it a vector
gcs_area = gcs_area |> 
  unlist() |> 
  as.vector()

# add area as a new column
# frames_a = list()
# for (i in 1:length(gcs_frames_joined_grouped)){
#   frames_a[[i]] = gcs_frames_joined_grouped[[i]] |> dplyr::mutate(area = gcs_area[i])
# }

frames_d = list()
for (i in 1:length(gcs_area)){
  frames_d[[i]] = frames_a[[i]] |> dplyr::mutate(density = n / gcs_area[i])
}

  
