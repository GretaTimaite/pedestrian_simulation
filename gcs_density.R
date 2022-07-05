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

# plotting densities

plots_den = list()
for (i in 1:length(frames_d)){
  plots_den[[i]] = ggplot2::ggplot(frames_d[[i]]) +
    ggplot2::aes(x = sec,
                 y = density) +
    ggplot2::geom_line() 
  print(plots_den)
}
# let's plot a polygons 1-4 and 5-8
gridExtra::grid.arrange(plots[[1]], plots[[2]],plots[[3]],plots[[4]], layout_matrix = rbind(c(1,2),c(3,4)))
gridExtra::grid.arrange(plots_den[[13]],plots_den[[14]],plots_den[[15]], plots_den[[16]], layout_matrix = rbind(c(1,2),c(3,4)))

