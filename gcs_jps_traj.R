# plot GCS pedestrian trajectories

traj = read.table(file = "/Users/gretatimaite/Desktop/pedestrian_simulation/traj.txt",
                  header = TRUE,
                  sep = "")

library(tidyverse)

traj_id = traj %>% 
  dplyr::group_by(FR)

plot(traj$X,
     traj$Y)

traj_5001 = traj %>% 
  dplyr::filter(ID == 5001) 

plot(traj_5001$X,
       traj_5001$Y)

traj_5001 %>% dplyr::slice_max(FR, n=1)

# drawing GCS
# concourse parameters: width(x) = 53, height(y) = 50;

plot(-10:60, -10:60, col = "white", xlab = "X", ylab = "Y") # draw an empty plot
polygon(x = c(0, 0, 53, 53),
        y = c(0, 50, 50,0),
        border = "black",
        lwd = 2) # draw walls of a GCS
polygon(x= c(0, -3, -3, 0),
        y = c(7, 7, 25, 25),
        border = "red",
        lwd = 2) # exit 10
polygon(x = c(0, 0, 11, 11),
        y = c(50, 53, 53, 50),
        border = "red",
        lwd = 2) # exit 9
polygon(x = c(11, 11, 33, 33),
        y = c(50, 53, 53, 50),
        border = "red",
        lwd = 2) # exit 8
polygon(x = c(33, 33, 48.5, 48.5),
        y = c(50, 53, 53, 50),
        border = "red",
        lwd = 2) # exit 7
polygon(x = c(53, 56, 56, 53),
        y = c(50, 50, 46.5, 46.5),
        border = "red",
        lwd = 2) # exit 6
polygon(x = c(53, 56, 56, 53),
        y = c(40, 40, 29, 29),
        border = "red",
        lwd = 2) # exit 5
polygon(x = c(53, 56, 56, 53),
        y = c(22, 22, 10, 10),
        border = "red",
        lwd = 2) # exit 4
polygon(x = c(53, 56, 56, 53),
        y = c(4.5, 4.5, 0, 0),
        border = "red",
        lwd = 2) # exit 3
polygon(x = c(53, 53, 33, 33),
        y = c(0, -3, -3, 0),
        border = "red",
        lwd = 2) # exit 2
polygon(x = c(33, 33, 11, 11),
        y = c(0, -3, -3, 0),
        border = "red",
        lwd = 2) # exit 1
polygon(x = c(11, 11, 0, 0),
        y = c(0, -3, -3, 0),
        border = "red",
        lwd = 2) # exit 0
# annotation of a plot
text(x = -4,
     y = 15,
     label = "Exit 10",
     srt = 90)
text(x = 7,
     y = 55,
     label = "Exit 9")
text(x = 20,
     y = 55,
     label = "Exit 8")
text(x = 40,
     y = 55,
     label = "Exit 7")
text(x = 57,
     y = 48,
     label = "Exit 6",
     srt = -90)
text(x = 57,
     y = 34,
     label = "Exit 5",
     srt = -90)
text(x = 57,
     y = 15,
     label = "Exit 4",
     srt = -90)
text(x = 57,
     y = 2,
     label = "Exit 3",
     srt = -90)
text(x = 45,
     y = -5,
     label = "Exit 2")
text(x = 22,
     y = -5,
     label = "Exit 1")
text(x = 6,
     y = -5,
     label = "Exit 0")

# converting to simple feature geometries
## creating an sfc
traj_sf = traj %>% 
  dplyr::select(ID, FR, X, Y) %>% 
  sf::st_as_sf(coords = c("X", "Y"))
traj_sf_5001 = traj_sf %>% 
  filter(ID == 5001)


