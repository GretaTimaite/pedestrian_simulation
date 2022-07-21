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



list = apply(frames_list_short, function(x){
  deldir::deldir(x = as.numeric(unlist(frames_list["x_coord"])),
                 y = as.numeric(unlist(frames_list["y_coord"])),
                 rw = c(0, 742, 0, 700))
})

