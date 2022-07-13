# AIM: plot fundamental diagram (speed ~ density)

# We have density per frame measured in the gcs_density.R script
# For speed we have distance travelled in 1 frame by an agent, which is equivalent to an agent speed/frame
# However, to plot speed and density against each other, we need to have them on the same scale.
# Currently density is an aggregate metric (D = N/area2) while speed is agent-focused.
# Speed needs to be aggregated and averaged to get a single number to plot againt density
# Thus, we need to group distances (ie speed per frame), sum them and divide by the number of pedestrians to get an average speed per frame
# (most likely a ) S(av) = sum(agent distance per frame)/N(agents per frame)

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

## let's split geometry column into x and y coordinates to apply non-spatial join.

new_df1 = agent_dist_list[[1]] |>  
  dplyr::mutate(geometry = geometry) 
new_df1 = new_df1 |> 
  dplyr::mutate(x_coord = purrr::map(new_df1$geometry,1) |> unlist() |> round(1),
                y_coord = purrr::map(new_df1$geometry,2) |> unlist() |> round(1)) |> 
  # dplyr::select(x_coord,dist) |> 
  sf::st_drop_geometry() |> 
  dplyr::group_by(x_coord)
frames_df0 = frames_df |> 
  dplyr::filter(ID == 0) |> 
  dplyr::mutate(x_coord = (x_coord/ 14) |> unlist() |> round(1),
                y_coord = (y_coord / 14) |> unlist() |> round(1)) |> 
  dplyr::group_by(x_coord)
new_joined = dplyr::left_join(new_df1[c("dist", "x_coord")], frames_df0, 
                              by = c("x_coord" = "x_coord"))

new_cbind = cbind(frames_df_new, new_df1)

frames_df0$y_coord %in% new_df1$y_coord
match(frames_df0$y_coord[1:2],
      new_df1$y_coord[1:2])
all.equal(frames_df0$y_coord[1],
          new_df1$y_coord[1])
frames_df0$y_coord == new_df1$y_coord
identical(frames_df0$y_coord,
          new_df1$y_coord)

# for some reason joining via x or y coordinates is not successful as not all values are identical (?),

new_joined1 = dplyr::left_join(
  new_df1[c("frame", "dist")] |> dplyr::group_by(frame),
  frames_df0 |> dplyr::group_by(frame),
  by = c("frame" = "frame"))

# ========== working (more or less)
joined_df = data.frame()
temp_list = list()
temp_list1 = list()
temp_list2 = list()
  for (i in 1:length(agent_dist_list)){
    temp_list[[i]] = agent_dist_list[[i]][c("dist", "frame", "ID")] |> sf::st_drop_geometry() |> dplyr::group_by(frame)
    temp_list1[[i]] = frames_df |> dplyr::filter(ID == unique(agent_dist_list[[i]]$ID)) |> dplyr::group_by(frame)
    temp_list2[[i]] = dplyr::left_join(temp_list[[i]],
                                       temp_list1[[i]],
                                       by = c("frame" = "frame")
                                       )
    # joined_df = temp_list2[[1]]
    # i = i
    # k = i + 1
    # if (k < length(temp_list2)) {
      joined_df = rbind(joined_df, temp_list2[[i]]) |> dplyr::distinct(.keep_all = TRUE)
    # }
    # else {
    #   break
    # }
  }

test= rbind(temp_list2[[1]],
            temp_list2[[2]])

frames_df_short = frames_df |> dplyr::select(ID,dist,frame)

