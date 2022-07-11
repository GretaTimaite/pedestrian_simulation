# AIM: measure pedestrian speed
# how: measure the distance an agent travelled from frame A to frame B, this will be indicative of how much an agent moved in one frame (0.04 sec)
# later this can be aggregated as to estimate the distance travelled in, for instance, 1 second
# enchancement: write a function that allows to specify interval at which to estimate travelled distance. For example, it could be for every frame
# (ie from frame 1 to frame 2) or every 25 frames (ie from frame 1 to frame 25). The second would simplify the trajectory but not much detail should be lost 
# as there's as much an agent can travel in one second. 

# get data
frames_sf = sf::st_read("https://github.com/GretaTimaite/pedestrian_simulation/releases/download/data/gcs_frames.geojson")

# our geometry column is in pixels rather than meters, so let's convert it to metres

frames_sf_m = frames_sf |> 
  dplyr::mutate(geometry = geometry / 14)

## now we'll create a list of dataframes for each agent
# first, let's find out the number of unique agents as this will be the number of dataframes in the list
unique_ids = frames_sf_m |> dplyr::pull(ID) |> unique() |> sort()

# create an empty list to store DFs
agent_list = list()
# create a loop
for (i in 1:length(unique_ids)){
  agent_list[[i]] = frames_sf_m |> # iterate through frames_sf
    dplyr::filter(ID %in% unique_ids[i]) # filter frames_sf by a unique ID
}

## sanity check
identical(agent_list[[30]],
          frames_sf_m |> dplyr::filter(ID %in% unique_ids[30]))
# >TRUE


## Now we will create a matrix of distances for each frame of a given agent. 
# It will be used later in a function to extract the distances sequentially (from frame 1 to frame 2 to frame 3, etc)

distance_matrix_list = list()
# system.time( # to measure how long it takes to run 
for (i in 1:length(agent_list)){
  distance_matrix_list[[i]] = sf::st_distance(agent_list[[i]])
}
# )

# create a function that accepts a list of matrices
dist_function = function(list_matrix = list()){ # accepts lists
  dist_list = list() # empty list to store new lists
  for(i in 1:length(list_matrix)){ # reiteration over the length of a list
    k = i # initialisation of a matrix row
    j = k + 1  # initialisation of a matrix column, it needs to be + 1 compared to the row as otherwise we will get distance equal to 0 
    # (there's no distance traveled from, e.g. frame 2 to frame 2)
    if (j <= ncol(list_matrix)){ # columns should not exceed the number of columns of a matrix (looping (out of bounds) error might be given otherwise)
      dist_list[[i]] = list_matrix[k,j] # for each `i` list extract matrix values at row k and column j 
    }
    else {
      break # break if j is above number of columns of a matrix
    }
  }
  return(dist_list)
}

# an empty list for distance vectors
distance_list = list()
# loop over `distance_matrix_list` and apply a `dist_function()`
# system.time( # to measure how long it takes to run 
for (i in 1:length(distance_matrix_list)){
  distance_list[[i]] = distance_matrix_list[[i]] |> dist_function()
}
# )

# NOTE: each list is shorter by 1 element compared to the original input list because of initialisation starting at 1, thus capturing the distance between frame 1 and 2. 
# In other words, the function omits the starting point at which an agent.

## testing
# here we'll check that the function works as it should.

distance_list[[5]][1] # agent 5's traveled distance from frame 1 to 2
#> [1] 0.1474067
# finding distance manually
sf::st_distance(agent_list[[5]][1,3], # frame 1 of agent 5
                agent_list[[5]][2,3]) # frame 2 of agent 5
#> [1,] 0.1474067
distance_matrix_list[[5]][2,1] # second row in matrix 1 ([1,1] would return 0 as there's the distance from frame 1 to frame 1 equals to 0) of agent 5.
#> [1] 0.1474067


distance_matrix_list1 = list()
for (i in 1:length(distance_matrix_list)){
  distance_matrix_list1[[i]] = distance_matrix_list[[i]] |> dist_function()
}



# Now let's create a new column showing the distance traveled in each frame.
# Note: the frame in which an agent entered the environment will be its starting frame, hence the column will be 0

distance_matrix = agent_geom1_list |> sf::st_distance()

vec_test = vector()
for(i in 1:nrow(distance_matrix)){
  k = i
  j = k + 1
  if (j <= nrow(distance_matrix)){
    vec_test[i] = distance_matrix[k,j]
  }
  else {
    break
  }
}

vec_test[121]
sf::st_distance(agent_geom1_list[[218]], agent_geom1_list[[122]])
distance_matrix[121,122]

# over a list

# loop as a function
dist_function = function(list_matrix = list()){
  list_test = list()
  for(i in 1:length(list_matrix)){
    k = i
    j = k + 1
    if (j <= nrow(list_matrix)){
      list_test[[i]] = list_matrix[k,j]
    }
    else {
      break
    }
  }
  return(list_test)
}
# test
f_test = dist_function(list_matrix = distance_matrix)


# a list of distance matrices
distance_matrix_list = list()
# system.time( # to measure how long it takes to run 
for (i in 1:length(agent_list)){
  distance_matrix_list[[i]] = sf::st_distance(agent_list[[i]])
}
# )

distance_matrix_list1 = list()
for (i in 1:length(distance_matrix_list)){
  distance_matrix_list1[[i]] = distance_matrix_list[[i]] |> dist_function()
}




# ============================================ works [or doesnt!]
# agent_geom1_list = agent_list1$geometry |> as.list()
# agent_geom1_list[[1]]
# 
# vec_test = vector()
# for (i in 1:length(agent_geom1_list)){
#   k = i + 1
#   vec_test[i] = sf::st_distance(agent_geom1_list[[i]], agent_geom1_list[[k]])
# }
# 
# sf::st_distance(agent_geom1_list[[2]], agent_geom1_list[[3]])
# 
# vec_test
# agent_list1_joined = cbind(agent_list1[2:220,], vec_test)
# agent_list1_new = agent_list1 |> dplyr::mutate(vec_test = 0)
# agent_list1_joined = rbind(agent_list1_new[1,], agent_list1_joined)
# 
# lapply_test = lapply(agent_geom1_list, sf::st_distance)

distance_matrix = agent_geom1_list |> sf::st_distance()

vec_test = vector()
for(i in 1:nrow(distance_matrix)){
  k = i
  j = k + 1
  if (j <= nrow(distance_matrix)){
    vec_test[i] = distance_matrix[k,j]
  }
  else {
    break
  }
}

vec_test[121]
sf::st_distance(agent_geom1_list[[218]], agent_geom1_list[[122]])
distance_matrix[121,122]

# over a list

# loop as a function
dist_function = function(list_matrix = list()){
  list_test = list()
  for(i in 1:length(list_matrix)){
    k = i
    j = k + 1
    if (j <= nrow(list_matrix)){
      list_test[[i]] = list_matrix[k,j]
    }
    else {
      break
    }
  }
  return(list_test)
}
# test
f_test = dist_function(list_matrix = distance_matrix)


# a list of distance matrices
distance_matrix_list = list()
# system.time( # to measure how long it takes to run 
for (i in 1:length(agent_list)){
  distance_matrix_list[[i]] = sf::st_distance(agent_list[[i]])
}
# )

distance_matrix_list1 = list()
for (i in 1:length(distance_matrix_list)){
  distance_matrix_list1[[i]] = distance_matrix_list[[i]] |> dist_function()
}




