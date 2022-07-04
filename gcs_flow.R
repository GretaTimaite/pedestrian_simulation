# AIM 1: plot flow of all GCS environment
# AIM 2L plot flow of a chosen area in GCS

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
  ggplot2::geom_vline(xintercept = frames_median_s, 
                      col = "blue")+
  ggplot2::geom_hline(yintercept = frames_mean_n,
                      col = "red")+
  ggplot2::geom_hline(yintercept = frames_median_n,
                      col = "blue")

# tasks:
# 1. write env as an object


