# AIM: plot fundamental diagram (speed ~ density)

# We have density per frame measured in the gcs_density.R script
# For speed we have distance travelled in 1 frame by an agent, which is equivalent to an agent speed/frame
# However, to plot speed and density against each other, we need to have them on the same scale.
# Currently density is an aggregate metric (D = N/area2) while speed is agent-focused.
# Speed needs to be aggregated and averaged to get a single number to plot againt density
# Thus, we need to group distances (ie speed per frame), sum them and divide by the number of pedestrians to get an average speed per frame
# S(av) = sum(agent distance per frame)/N(agents per frame)

##====================== 
## Getting data

# NOTE: skip this section (start from line 100) if you download the prepped 'frames' data
frames = read.csv("https://github.com/GretaTimaite/pedestrian_simulation/releases/download/data/frames_final.csv")

# NOTE: the rest of this section shows how I got `frames` dataset
frames_df = read.csv("https://github.com/GretaTimaite/pedestrian_simulation/releases/download/data/gcs_frames.csv")
agent_dist_list = readRDS(url("https://github.com/GretaTimaite/pedestrian_simulation/releases/download/data/agent_dist_list.rds"))


# problem: our `agent_dist_list` list that has distance values are sorted by agents,
# thus we cannot simply group all the lists by frame unless we unlist them first.

# Let's try to add dist column from `agent_dist_list` to frames_sf data


## Spatial joins don't work nicely as, for some reason, new duplicate rows are created for some frames but not others.
## See an example below (needs uncommenting)
# new_df = frames_sf |>  dplyr::mutate(geometry = geometry/14) |> 
#   sf::st_join(agent_dist_list[[1]],
#               join = st_within)

## Note: splitting geometry column into x and y coordinates to join non-spatially by x or y coordinates did not work nicely either.
# new_df1 = agent_dist_list[[1]] |> 
#   dplyr::mutate(x_coord = purrr::map(agent_dist_list[[1]]$geometry,1) |> unlist(),
#                 y_coord = purrr::map(agent_dist_list[[1]]$geometry,2) |> unlist()) |> 
#   # dplyr::select(x_coord,dist) |> 
#   sf::st_drop_geometry() 
# frames_df0 = frames_df |> 
#   dplyr::filter(ID == 0) |> 
#   dplyr::mutate(x_coord = (x_coord/ 14) |> unlist(),
#                 y_coord = (y_coord / 14) |> unlist()) 
# new_joined = dplyr::left_join(frames_df0, new_df1[c("dist", "x_coord")], 
#                               by = c("x_coord" = "x_coord"))
# # So, even though numbers are equal, they are not 
# all.equal(frames_df0$y_coord[1],
#           new_df1$y_coord[1])
# #>[1] TRUE
# identical(frames_df0$y_coord[1],
#           new_df1$y_coord[1])
# #>[1] FALSE
# frames_df0$x_coord[1] == new_df1$x_coord[1]
# #>[1] FALSE

# SOLUTION
# we will group the frames_df dataframe by an agent ID and join with a matching list in agent_dist_list, thus creating a new list. 
# Then we will extract each list and simply join all of them by row (since columns will be identical)

# a dataframe to store our final output
frames_dist_df = data.frame()
# a bunch of temp_lists for intermediate steps
temp_list = list()
temp_list1 = list()
temp_list2 = list()
  for (i in 1:length(agent_dist_list)){
    temp_list[[i]] = agent_dist_list[[i]][c("dist", "frame", "ID")] |> # extract only columns we need
      sf::st_drop_geometry() |> # drop geometry column
      dplyr::arrange(frame) # arrange by frame (just for sanity's sake to ensure ascending order)
    temp_list1[[i]] = frames_df |> 
      dplyr::filter(ID == unique(agent_dist_list[[i]]$ID)) |>
      dplyr::arrange(frame) # arrange by frame (just for sanity's sake to ensure ascending order)
    temp_list2[[i]] = dplyr::left_join(temp_list[[i]],
                                       temp_list1[[i]],
                                       by = c("frame" = "frame") # join by frame
                                       )
      frames_dist_df = rbind(frames_dist_df, # df to append (add rows to)
                        temp_list2[[i]]) |> # what is being appended
        dplyr::distinct(.keep_all = TRUE) # keeping only distinct (unique) rows. This is needed as the last list gets added twice.
      # I think this is because we need to append to something (empty dataframe does not work), hence it appends the last list in the temp_list2
  }

# A good guess that the join worked is the fact that we have the same rows in both frames_df and newly created frames_dist_df.
## a quick check that a randomly selected list in both temp_list and temp_list1 have identical frames (spoiler alert: yes)
# identical(temp_list[[131]]$frame,
#           temp_list[[131]]$frame)

## another quick check 
# identical(frames_dist_df |> dplyr::filter(ID.x == 121) |> dplyr::select(x_coord),
#           frames_df |> dplyr::filter(ID == 121) |> dplyr::select(x_coord))

# Let's drop one of the ID columns and rename another to a more sensible one
frames_final = frames_dist_df |> 
  dplyr::select(-ID.x) |> 
  dplyr::rename(ID = ID.y)
# save as csv
# write.csv(frames_final,
#           "frames_final.csv")

##======================          
## Plotting fundamentals for the entire env

# let's add a sec column
# 1 sec = 25 frames
frames_sec = frames |> 
  dplyr::mutate(sec = frame / 25)
frames_sec_sf = frames_sec |> 
  sf::st_as_sf(coords = c("x_coord", "y_coord")) |> 
  dplyr::mutate(geometry = geometry/14)

# density adapted from here: https://github.com/GretaTimaite/pedestrian_simulation/blob/main/gcs_density.R

matrix_walls = matrix(c(0,0,0,50,53,50, 53,0,0,0),
                      ncol = 2,
                      byrow = TRUE)
matrixlist_walls = list(matrix_walls)
polygon_walls = sf::st_polygon(matrixlist_walls)
gcs_walls_area = polygon_walls |> sf::st_area()

den = frames_sec |> 
  dplyr::group_by(frame) |> 
  dplyr::summarise(n = dplyr::n(),
                   density = n / gcs_walls_area)  

# average speed per frame
test= frames_sec |> 
  dplyr::group_by(frame) |> 
  dplyr::summarise(n = dplyr::n()) 
test2 = frames_sec |> 
  dplyr::filter(dist != 0) |> 
  dplyr::group_by(frame) |> 
  dplyr::summarise(dist_sum = sum(dist))
test2[1035,]
test_joined = dplyr::left_join(test, test2) |> 
  dplyr::mutate(speed_av = dist_sum/n) 

test_joined2 = dplyr::left_join(test_joined, den)

ggplot2::ggplot(data = test_joined2,
                ggplot2::aes(x = speed_av,
                            y = density))+
  ggplot2::geom_point()

cor.test(test_joined2$speed_av,
    test_joined2$density,
    method = "pearson")

##=======================
# Plotting fundamentals for each divided area
# divide gcs polygon by creating a grid
gcs_div = sf::st_make_grid(polygon_walls, 
                           n = 2, #2x2
                           what = "polygons")
# convert gcs_div to an sf object
gcs_div_sf = gcs_div |> 
  sf::st_as_sf() |> 
  dplyr::rename(geom = x)

# first create a list to store our multiple dataframes
gcs_frames_joined1 = list()
for (i in 1:lengths(gcs_div_sf)){
  gcs_frames_joined1[[i]] = frames_sec_sf[gcs_div_sf[i,], op = sf::st_intersects] 
}

# find out the area size of each polygon
gcs_area = list()
for (i in 1:lengths(gcs_div_sf)){
  gcs_area[[i]] = sf::st_area(gcs_div_sf[[i]]) 
  print(gcs_area)
}
# make it a vector
gcs_area = gcs_area |>
  unlist() |>
  as.vector()

frames_d1 = list()
for (i in 1:length(gcs_area)){
  frames_d1[[i]] = gcs_frames_joined1[[i]] |>
    dplyr::group_by(frame) |> 
    dplyr::summarise(n = dplyr::n()) |> 
    dplyr::mutate(density = n / gcs_area[i])
}

frames_s1 = list()
for (i in 1:length(gcs_area)){
  frames_s1[[i]] = gcs_frames_joined1[[i]] |> 
    dplyr::filter(dist != 0) |> 
    dplyr::group_by(frame) |> 
    dplyr::summarise(n = dplyr::n(),
                     dist_sum = sum(dist),
                     speed_av = dist_sum / n)
}

frames_ds = list()
for (i in 1:length(frames_d1)){
  frames_ds[[i]] = dplyr::left_join(frames_d1[[i]] |> sf::st_drop_geometry(),
                                    frames_s1[[i]] |> sf::st_drop_geometry())
}

# plotting!
plots_ds = list()
for (i in 1:length(frames_ds)){
  plots_ds[[i]] = ggplot2::ggplot(frames_ds[[i]]) +
    ggplot2::aes(x = speed_av,
                 y = density) +
    ggplot2::geom_line() 
  # print(plots_den)
}
gridExtra::grid.arrange(plots_ds[[1]], plots_ds[[2]],plots_ds[[3]],plots_ds[[4]], layout_matrix = rbind(c(1,2),c(3,4)))


# Conclusions

# Fundamental diagrams do not show the expected negative slope -- the increase in density does not lead to a 
# decreased speed. Indeed, a reverse pattern can be seen in all 4 plots. This might have happened because in all 4 
# areas density is low, thus no congestion occurs. An increase in average speed and density might be a result of a 
# new agent entering the area that walks fast, thus boosting the av.speed and density (a little bit!). 
# Thus, it could be useful to pick a dense location near the exits and plot a FD for it. I
# ndeed, a density next to an exit could be compared to an adjacent area that leads to an exit but is not as crowded.
# 
# Also it could be interesting to experiment with different agent numbers in JPS to figure out when/how congestion 
# occurs, thus leading to the expected outcome -- as density increases, speed/velocity decreases.
