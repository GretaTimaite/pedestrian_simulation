# AIM: read GCS data

library(tidyverse)

activation = read.delim("/Users/gretatimaite/Desktop/uncertainty/GCS_data/activation.dat",
                        header = T,
                        sep = "")
activation[234,]

filelist = list.files(pattern = "/Users/gretatimaite/Desktop/uncertainty/*.dat")
datalist = lapply(filelist, FUN = read.delim)

frames = vroom::vroom(filelist)
frames = vroom::vroom(arfile1)


arfile = "/Users/gretatimaite/Desktop/uncertainty/GCS_data/frames.tar.gz" # Or "cat.tar.gz" if that is right
arfile1 = untar(arfile,
                list = TRUE)
data = purrr::map_df %>% read.delim(arfile1,
                   header = T)

# ===========
list_of_files <- list.files(path = "/Users/gretatimaite/Desktop/uncertainty", 
                            recursive = TRUE,
                            pattern = "^frame", 
                            full.names = TRUE)

df <- list_of_files %>%
  set_names(.) %>%
  map_df(read_table2, .id = "FileName")

df %>% names()
# 

# ============

trajectories = R.matlab::readMat("/Users/gretatimaite/Desktop/uncertainty/GCS_data/trajectoriesNew.mat")
trajectories_df = as.data.frame(trajectories)

trajectories_df[1,]

# read all frames at once into a single dataframe
setwd("/Users/gretatimaite/Desktop/uncertainty/GCS_data/frames")
ldf <- list() # creates a list
listdat <- dir(pattern = "*.dat") # creates the list of all the csv files in the directory
for (k in 1:length(listdat)){
  ldf[[k]] <- read.delim(listdat[k],
                         header = T,
                         sep = "")
}
str(ldf[[1]]) 

# convert list to a df
traj_df = data.frame(t(sapply(ldf,c)))
traj_df = traj_df %>% rename(pedestrianID = X.,
                             pedestrian_id = pedestrianID,
                             x_coord = pedestrianID,
                             y_coord = x) %>% 
  dplyr::select(-1)
df %>% select()

traj_df = traj_df %>% 
  mutate(
    pedestrian_id = X.,
    x_coord = pedestrianID,
    y_coord = x
  ) %>% 
  select(5:7)

