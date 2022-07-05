# AIM 1: plot flow of all GCS environment
# AIM 2L plot flow of a chosen area in GCS

# ================================== AIM 1
# first read in data
frames_df = read.csv("https://github.com/GretaTimaite/pedestrian_simulation/releases/download/data/gcs_frames.csv")

# convert to an sf object for spatial analysis
frames_sf = frames_df |> 
  sf::st_as_sf(coords = c("x_coord", "y_coord"))

# the coordinates of frames_sf are in pixels, but having them in meters might make the analysis more intuitive.
# 1 metre = 14 pixels (also see `pixel_geo.R` script)
frames_sf_m = frames_sf |> 
  dplyr::mutate(geometry = geometry / 14)

# let's plot the new geometry column on the environment drawn in metres (see script `metres_geo.R` to draw it)

frames_sf_m |> 
  sf::st_geometry() |> 
  plot(add = T) 

# now let's create a new column for seconds
# 1 second = 25 frames
frames_sf_m = frames_sf_m |> 
  dplyr::mutate(sec = frame / 25)

# group df by seconds
frames_grouped = frames_sf_m |> 
  sf::st_drop_geometry() |> #drop geometry as it's not needed here
  dplyr::group_by(sec) |> # group by seconds
  dplyr::summarise(n = dplyr::n()) # summarise 

# let's calculate mean and median values of seconds to add to the plot
frames_mean = frames_sf_m$sec |> mean()
frames_median = frames_sf_m$sec |> median()

# let's calculate mean and median values of n to add to the plot
frames_mean_s = frames_sf_m$sec |> mean()
frames_median_s = frames_sf_m$sec |> median()
frames_mean_n = frames_grouped$n |> mean()
frames_median_n = frames_grouped$n |> median()

ggplot2::ggplot(frames_grouped) +
  ggplot2::aes(x = sec,
               y = n) +
  ggplot2::geom_line() +
  ggplot2::geom_vline(xintercept = frames_mean_s,
                      col = "red") +
  ggplot2::geom_text(ggplot2::aes(x=frames_mean_s+5, label=paste0("Mean\n",round(frames_mean_s,2)), y=80)) +
  ggplot2::geom_vline(xintercept = frames_median_s, 
                      col = "blue") +
  ggplot2::geom_hline(yintercept = frames_mean_n,
                      col = "red") +
  ggplot2::geom_hline(yintercept = frames_median_n,
                      col = "blue")

# ================================== AIM 2
# divide polygon into n subareas
## walls as sf
# polygon(x = c(0, 0, 53, 53),
#         y = c(0, 50, 50,0),
#         border = "black",
#         lwd = 2) # draw walls of a GCS
matrix_walls = matrix(c(0,0,0,50,53,50, 53,0,0,0),
                      ncol = 2,
                      byrow = TRUE)
matrixlist_walls = list(matrix_walls)
polygon_walls = sf::st_polygon(matrixlist_walls)

# divide gcs polygon by creating a grip
gcs_div = sf::st_make_grid(polygon_walls, 
                           # cellsize = 1,
                           n = 4,
                           what = "polygons")
# plot grid over gcs polygon to check their order (starts bottom left)
polygon_walls |> plot(reset = T)
gcs_div |> plot(add =T)
gcs_div_sf[1,] |> plot(add =T, col = "red")
gcs_div_sf[2,] |> plot(add =T, col = "blue")
gcs_div_sf[5,] |> plot(add =T, col = "green")

# convert gcs_div to an sf object
gcs_div_sf = gcs_div |> 
  sf::st_as_sf() |> 
  dplyr::rename(geom = x)

# let's iterate through all 16 polygons to find out which rows (agents) are within each polygon
# first create a list to store our multiple dataframes
gcs_frames_joined = list()
for (i in 1:lengths(gcs_div_sf)){
  gcs_frames_joined[[i]] = frames_sf_m[gcs_div_sf[i,], op = sf::st_within]
}

# sanity check
gcs_frames_joined1 = frames_sf_m[gcs_div_sf[1,], op = sf::st_within]
gcs_frames_joined2 = frames_sf_m[gcs_div_sf[2,], op = sf::st_within]
identical(gcs_frames_joined1, gcs_frames_joined[[1]]) # TRUE
identical(gcs_frames_joined2, gcs_frames_joined[[2]]) # TRUE



# tasks:
# 1. write env as an object


