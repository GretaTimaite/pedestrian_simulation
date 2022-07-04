# AIM: in this script frames from GCS data will be read in and converted to a .csv 

# an end result of this script (.csv and/or .geojson files) can be downloaded from with the following:

frames_df = read.csv("https://github.com/GretaTimaite/pedestrian_simulation/releases/download/data/gcs_frames.csv")
frames_sf = sf::st_read("https://github.com/GretaTimaite/pedestrian_simulation/releases/download/data/gcs_frames.geojson")

# get frames data
# read all frames (.dat files) at once into a list
setwd("/Users/gretatimaite/Desktop/pedestrian_simulation/GCS_data/frames") # set working directory to read files from
frames_l = list() # creates a list
listdat = dir(pattern = "*.dat") # creates the list of all the .dat files in the directory
for (k in 1:length(listdat)){
  frames_l[[k]] = read.delim(listdat[k],
                             header = T,
                             sep = "")
}
# === the commented code below shows why list needs sorting

# convert list to a df
# frames_df = data.frame(t(sapply(frames_l,c)))
# frames_df = frames_df |> 
#   # rename columns and keep only 3
#   dplyr::rename(pedestrianID = X.,
#                                 pedestrian_id = pedestrianID,
#                                 x_coord = pedestrianID,
#                                 y_coord = x) %>% 
#   dplyr::select(1:3) 

# problem: files are not read one by one (ie frame_0.0.dat, frame_1.0.dat,frame_3.0.dat)
# but alphabetically (ie. frame_0.0.dat, frame_1.0.dat,frame_10.0.dat)
# it messes up frame order

# ======

# solution borrowed from stackoverflow :)
# https://stackoverflow.com/questions/10777367/how-can-i-read-the-files-in-a-directory-in-sorted-order-using-r
listdat_sorted = sort(listdat)
listdat_sorted_split = strsplit(listdat_sorted, "frame_") 
listdat_sorted_split = as.numeric(sapply(listdat_sorted_split,
                                          function(x) x = sub(".dat", "", x[2])))

listdat_correct_order = listdat_sorted[order(listdat_sorted_split)] 
# let's get the frames in a correct order
for (k in 1:length(listdat_correct_order)){
  frames_l[[k]] = read.delim(listdat_correct_order[k],
                             header = T,
                             sep = "")
}

# convert list to a df joined by
frames_df = data.frame(t(sapply(frames_l,c)))
# rename columns and leave only the ones we need
frames_df = frames_df %>% rename(pedestrianID = X.,
                                 pedestrian_id = pedestrianID,
                                 x_coord = pedestrianID,
                                 y_coord = x) %>% 
  dplyr::select(1:3)

# unlist columns in frames_df (test with a subset) 

frames_sub = frames_df |> 
  # creates a new column with row id which is indicative of a frame (ie time)in which the agent is present
  dplyr::mutate(row_id = row_number()) |> 
  # subset
  slice(50:55)

frames_sub = frames_sub |> 
  # finds out the length of pedestrianID column, this will be needed when unlisting
  dplyr::mutate(id_length = frames_sub$pedestrianID |> lengths()) 

row_ids = rep(
  x = frames_sub$row_id,
  times = frames_sub$id_length
) # replicates row ids based on the id_length

row_ids_split = split(row_ids, 
                      rep(1:length(frames_sub$id_length), 
                          frames_sub$id_length)) # splits row_ids into multiple lists with a number of elements based on the id_length
row_ids_split_df = data.frame(sapply(row_ids_split,c)) # convert list to a df by column
row_ids_split_vect = row_ids_split_df |> 
  as.list() |> # turn df into a list
  unlist()  # unlist it

frames_unlist = frames_sub |>
  # remove column that we need no longer
  dplyr::select(-c(row_id, id_length)) |>
  # unlist the df
  unlist()

frames_matrix = frames_unlist |> 
  # turn a list into a matrix; 
  matrix(nrow = 12, ncol = 3, byrow = F) # values will change depending on the previously unlisted df
colnames(frames_matrix) = c("ID", "x_coord", "y_coord") # give column names

frames_matrix = cbind(frames_matrix, row_ids_split_vect) # join a df and a vector
frames_sf = frames_matrix |> 
  # convert to a df
  as.data.frame() |>
  # convert to an sf object with spatial information (points)
  sf::st_as_sf(coords = c("x_coord", "y_coord"))

# ======================================= 

frames_df1 = frames_df |>
  # creates a new column with row id that will be indicative of a time frame (ie time) in which the agent is present
  dplyr::mutate(row_id = row_number()) 

frames_df1 = frames_df1 |> 
  # finds out the length of pedestrianID column, this will be needed later in replicating the values and unlisting
  dplyr::mutate(id_length = frames_df$pedestrianID |> lengths()) 

row_ids1 = rep(
  x = frames_df1$row_id,
  times = frames_df1$id_length
) # replicates row ids based on the id_length

row_ids_split1 = split(row_ids1, 
                      rep(1:length(frames_df1$id_length), 
                          frames_df1$id_length)) # splits row_ids into multiple lists with a number of elements based on the id_length
# we cannot convert the row_ids_split1 list to a df directly because of different row lengths. 
# We will need to tell R to to add NAs when if cell is empty
# let's find out the max length of a list
length = row_ids_split1 |> 
  lengths() |>
  max()

# define a max number of rows based on `length` as columns not having all the rows filled with values will be filled with NAs instead
row_ids_split_df1 = as.data.frame(do.call(cbind, 
                                          lapply(row_ids_split1, 
                                                 `length<-`,
                                                 max(length))))

row_ids_split_vect1 = row_ids_split_df1 |>
  as.list() |> # turn df into a list
  lapply(function(x) x[!is.na(x)]) |> # removes NAs before unlisting
  unlist() # unlist it

frames_unlist1 = frames_df1 |>
  # remove columns that we need no longer
  dplyr::select(-c(row_id, id_length)) |>
  # unlist the df
  unlist()

frames_matrix1 = frames_unlist1 |> 
  # turn a list into a matrix; 
  matrix(nrow = 226390, ncol = 3, byrow = F) # we unlisted 3 different columns each having 226390 rows
# to find nrow you can, for example, run the code below:
# frames_df1$pedestrianID |> lengths() |> sum()
colnames(frames_matrix1) = c("ID", "x_coord", "y_coord") # give column names

frames_matrix1 = cbind(frames_matrix1, row_ids_split_vect1) # join a df and a vector
colnames(frames_matrix1)[4] = "frame" # rename a column

## save frames as a csv file
# write.csv(x = frames_matrix1,
#           file = "gcs_frames.csv", # writes to working directory
#           col.names = T, # keeps column names
#           row.names = F) # ignores row names

frames_sf = frames_matrix1 |> 
  # convert to a df
  as.data.frame() |>
  # convert to an sf object with spatial information (points)
  sf::st_as_sf(coords = c("x_coord", "y_coord"))

## save frames_sf as a geojson file
# sf::write_sf(frames_sf1,
#              "gcs_frames.geojson")


