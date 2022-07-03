# drawing GCS in pixels

# I followed this notebook and approximated 1 meter to 14 pixels
# https://github.com/Urban-Analytics/dust/blob/main/Projects/ABM_DA/experiments/grand_central_terminal_data/GCT-data.ipynb

plot(-100:800, -100:800, col = "white", xlab = "X", ylab = "Y") # draw an empty plot
polygon(x = c(0, 0, 742, 742),
        y = c(0, 700, 700, 0),
        border = "black",
        lwd = 2) # draw walls of a GCS
polygon(x= c(0, -42, -42, 0),
        y = c(98, 98, 350, 350),
        border = "red",
        lwd = 2) # exit 10
polygon(x = c(0, 0, 154, 154),
        y = c(700, 742, 742, 700),
        border = "red",
        lwd = 2) # exit 9
polygon(x = c(154, 154, 462, 462),
        y = c(700, 742, 742, 700),
        border = "red",
        lwd = 2) # exit 8
polygon(x = c(462, 462, 679, 679),
        y = c(700, 742, 742, 700),
        border = "red",
        lwd = 2) # exit 7
polygon(x = c(742, 784, 784, 742),
        y = c(700, 700, 651, 651),
        border = "red",
        lwd = 2) # exit 6
polygon(x = c(742, 784, 784, 742),
        y = c(560, 560, 406, 406),
        border = "red",
        lwd = 2) # exit 5
polygon(x = c(742, 784, 784, 742),
        y = c(308, 308, 140, 140),
        border = "red",
        lwd = 2) # exit 4
polygon(x = c(742, 784, 784, 742),
        y = c(63, 63, 0, 0),
        border = "red",
        lwd = 2) # exit 3
polygon(x = c(742, 742, 462, 462),
        y = c(0, -42, -42, 0),
        border = "red",
        lwd = 2) # exit 2
polygon(x = c(462, 462, 154, 154),
        y = c(0, -42, -42, 0),
        border = "red",
        lwd = 2) # exit 1
polygon(x = c(154, 154, 0, 0),
        y = c(0, -42, -42, 0),
        border = "red",
        lwd = 2) # exit 0
polygon(x = c(294, 252, 210, 210, 252, 294, 336, 336, 294),
        y = c(294, 294, 336, 378, 420, 420, 378, 336, 294),
        col = "red") # information booth (an obstacle)

# annotation of a plot
text(x = -56,
     y = 210,
     label = "Exit 10",
     srt = 90)
text(x = 98,
     y = 770,
     label = "Exit 9")
text(x = 280,
     y = 770,
     label = "Exit 8")
text(x = 560,
     y = 770,
     label = "Exit 7")
text(x = 798,
     y = 672,
     label = "Exit 6",
     srt = -90)
text(x = 798,
     y = 476,
     label = "Exit 5",
     srt = -90)
text(x = 798,
     y = 210,
     label = "Exit 4",
     srt = -90)
text(x = 798,
     y = 28,
     label = "Exit 3",
     srt = -90)
text(x = 630,
     y = -70,
     label = "Exit 2")
text(x = 308,
     y = -70,
     label = "Exit 1")
text(x = 84,
     y = -70,
     label = "Exit 0")

