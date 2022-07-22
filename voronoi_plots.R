# AIM: plot voronoi diagrams for each frame 

# read frames data
frames = read.csv("https://github.com/GretaTimaite/pedestrian_simulation/releases/download/data/frames_final.csv")

# ========== 
## preparing data
# creting a list of frames
frames_list = list()
# a vector of unique frames that we will use to loop over
unique_frames = unique(frames$frame)
# for loop
for (i in 1:length(unique_frames)){
  frames_list[[i]] = frames[frames$frame == i,] 
}

# Delaunay triangulation (needed for Voronoi diagrams) requires at least two points, so all frames that have only one agent need to be exluded.
# Let's find out how many frames have only 1 agent present
frames |> 
  dplyr::group_by(frame) |> 
  dplyr::summarise(n = dplyr::n()) |> 
  dplyr::filter(n == 1) |> nrow()
#> [1] 326

# Hence, 326 lists need to be excluded before computing Voronoi diagrams
frames_list_short = list()
for (i in 1:length(frames_list)){
  k = i
  if(nrow(frames_list[[k]]) > 1){ # dataset in each `frames_list` list needs to have more than 1 row (thus, agent)
    frames_list_short[[k]] = frames_list[[k]] # if condition is met, it gets added to the new list
  }
  else { # if the condition it not met, then the list is to be skipped
    next
  }
}

# We have a new list of 5405 elements (lists) inside. 
# This is more than expected as the difference between `frames_list` and `frames_list_short` is 283 instead of 326
# the reason for this is that for some reason the first 43 lists in `frames_list_short` have not been dropped 
# but, instead, are NULL.
# This might not be a big issue in creatingdeldir objects and plotting Voronoi diagrams

# ========
## Deldir lists

# create empty lists to store deldir objects
deldir_frames = list()
deldir_tiles = list()
for (i in 1:length(frames_list_short)){
  if(is.null(frames_list_short[[i]]) == FALSE) { # if list IS NOT NULL, then
    deldir_frames[[i]] = deldir::deldir(x = as.numeric(unlist(frames_list[[i]]["x_coord"])),
                                        y = as.numeric(unlist(frames_list[[i]]["y_coord"])),
                                        rw = c(0, 742, 0, 700))
    deldir_tiles[[i]] = deldir::tile.list(deldir_frames[[i]])
  }
  else{ # if list is NULL then skip
    next
  }
}

deldir_tiles[[100]] |> plot()

# ========
## converting deldir to sf
# deldir provides vertex coordinates of tiles, but to make spatial operations easier (ie find out intersections), it is useful to have
# tiles as polygons.

# a function to convert deldir tiles to sf polygons for a single frame
tiles_pol = function(deldir_tiles = list()){
  list_xy = list()
  list_sf = list()
  list_comb = list()
  list_pol = list()
  for (i in 1:length(deldir_tiles)){
    list_xy[[i]] = data.frame("x" = deldir_tiles[[i]][["x"]],
                              "y" = deldir_tiles[[i]][["y"]])
  }
  for (i in 1:length(list_xy)){
    list_sf[[i]] = list_xy[[i]] |>
      sf::st_as_sf(coords = c("x", "y"))
  }
  
  for (i in 1:length(list_sf)){
    list_comb[[i]] = sf::st_combine(list_sf[[i]])
  }
  for (i in 1:length(list_comb)){
    list_pol[[i]] = list_comb[[i]] |> sf::st_cast("POLYGON")
  }
  return(list_pol)
}

del44 = deldir_tiles[[44]] |> tiles_pol()
deldir_tiles[[44]] |> plot()
del44[[2]] |> plot(add = T)

for (i in 1:length(deldir_tiles)){
  list[[i]] = deldir_tiles[[100]] |> tiles_as_pol()
}


# use function to return a list
# list = list()
# for (i in 1:length(deldir_tiles)){
#   j = 1 
#   if(j <  length(deldir_tiles[[i]]) + 1){
#     list[[i]] = deldir_tiles[[i]] |> tiles_as_pol()
#   }
#   else{
#     break
#   }
# }
list = list()
for (i in 1:length(deldir_tiles)){
  # if(is.null(deldir_tiles[[i]]) == FALSE ){
  list[[i]][[k]] = for (k in 1:length(deldir_tiles[[i]])){}
        
        deldir_tiles[[i]][[k]]
  }
    # else{
    #   next
    # }
  }
  }

deldir_frames_test =  list()
for(i in 1:length(deldir_frames)){
  deldir_frames_test[[i]] = get(deldir_frames[[i]]) 
}




# ====== 
## failure

tiles_as_pol = function(deldir_tiles = list()){
  tile_coord = data.frame("x" = deldir_tiles$x,
                          "y" = deldir_tiles$y)
  
  tile_sf = tile_coord |> 
    sf::st_as_sf(coords = c("x", "y"))
  
  tile_comb = sf::st_combine(tile_sf)
  
  tile_pol = sf::st_cast(tile_comb, 
                         "POLYGON")
}

deldir_tiles[[700]] |> tiles_as_pol()

# tile 140 as an sf object 
tile140 = list("x" = list(deldir_tiles[[140]][[1]][[3]],
                                 deldir_tiles[[140]][[2]][[3]],
                                 deldir_tiles[[700]][[3]][[3]]
                                 ),
                   "y" = list(deldir_tiles[[140]][[1]][[4]],
                               deldir_tiles[[140]][[2]][[4]],
                               deldir_tiles[[700]][[3]][[4]])
                   )
tile_list = list()
for (i in 1:length(tile140[["x"]])){
  tile_list[[i]] = rbind(tile140[["x"]][[i]],
                         tile140[["y"]][[i]]) |> 
    as.data.frame() |> 
    t()
  colnames(tile_list[[i]]) = c("x", "y")
}

tiles_sf = list()
tiles_comb = list()
for (i in 1:length(tile_list)){
  tiles_sf[[i]] = tile_list[[i]] |> 
    as.data.frame() |> 
    sf::st_as_sf(coords = c("x", "y"))
  
  tiles_comb = lapply(tiles_sf, sf::st_combine) 
}  

tiles_pol = list()
for (i in 1:length(tiles_comb)){
  tiles_pol[[i]] = tiles_comb[[i]] |> sf::st_cast("POLYGON")
}

tiles_bind = sf::st_union(tiles_pol[[1]], 
                         tiles_pol[[2]],
                         tiles_pol[[3]])

tiles_unique = rbind(
  tile_list[[1]], tile_list[[2]], tile_list[[3]]
) |> 
  as.data.frame() |> 
  dplyr::distinct(.keep_all = T)

tiles_unique_sf = tiles_unique |> 
  sf::st_as_sf(coords = c("x", "y"))
tiles_unique_sf |> plot(add = T, col = "red")

tiles_comb1 = sf::st_cast(tiles_bind, "POLYGON")
tiles_comb[[1]] |> plot()

tiles_pol = tiles_comb[[1]] |>  sf::st_cast("POLYGON")
tiles_pol |> plot(add = T)
deldir_tiles[[140]] |> plot()
tiles_comb[[2]] |>  sf::st_cast("POLYGON") |> plot(add = T, col = "red")
tiles_comb[[3]] |>  sf::st_cast("POLYGON") |> plot(add = T, col = "blue")
tiles_bind |> plot(add = T,col = "yellow")


tile140_sf = tile140 |> sf::st_as_sf(coords = c("x", "y"))


# tile7 = tile7 |> 
#   dplyr::mutate(row_id = dplyr::row_number())
tile7_sf = tile7 |> sf::st_as_sf(coords = c("x", "y"))
# tile7_point = tile7_sf[1,]
# tile7_pol = rbind(tile7_sf, tile7_point)
tile7_pol = sf::st_combine(tile7_sf)
# tile7_pol = sf::st_cast(tile7_pol, "MULTIPOINT")
tile7_pol = sf::st_cast(tile7_pol, "POLYGON")
