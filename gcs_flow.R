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

ggplot2::ggplot(frames_sf_m,
                ggplot2::aes(x = sec,
                    y = stat(count)))+
  ggplot2::stat_count()





# tasks:
# 1. write env as an object


