###########################################
rm(list = ls(all = TRUE))  
###########################################

setwd("~cardiac/Experiments_of_Maria/01102019_First_experiments/")

library(plotly)
library(data.table)

Data <- as.data.frame(fread(file = "~cardiac/Experiments_of_Maria/01102019_First_experiments/BSA_beta.txt", header = FALSE, fill = TRUE))
colnames(Data) <- c("x", "y", "z", "w")

# Data <- as.matrix(Data)
ax <- list(
  title = "",
  zeroline = F,
  showline = F,
  showticklabels = F,
  showgrid = F)
plot_ly(Data, x = ~x , y = ~y , z = ~z,
        marker = list(color = ~w, colorbar=list(title='beta (a.u)',len = 0.55, x = 1.13, y = 0.8), 
                      colorscale = c('#1972A4', '#FF7070'), size=10,
                      cauto = F,
                      cmin = -0.2,
                      cmax = 0.2,showscale = TRUE)) %>%
  add_markers() %>%
  layout(scene = list(xaxis = ax,yaxis = ax, zaxis = ax, dragmode="turntable"))
p

# p <- plot_ly(Data, x = ~x , y = ~y , z = ~z, color = ~w,colors= c('#1972A4', '#FF7070'),
#            alpha = 1) %>%
#   add_markers() %>%
#   layout(scene = list(xaxis = ax,yaxis = ax, zaxis = ax, dragmode="turntable")) %>%
#   colorbar(limits = c(-0.18,0.3), title = "beta (a.u)")
# p

#### save image ####
orca(p, "BSA_beta.png")

library( rgl )
library(magick)
plot3d( Data[,1], Data[,2], Data[,3], col = Data$w, type = "s", radius = .2 )
# We can indicate the axis and the rotation velocity
play3d( spin3d( axis = c(0, 0, 1), rpm = 20), duration = 10 )

# Save like gif
movie3d(
  movie="3dAnimatedplot", 
  spin3d( axis = c(0, 0, 1), rpm = 7),
  duration = 10, 
  dir = "P:/01102019_Experiments_of_Maria",
  type = "gif", 
  clean = TRUE
)


#### ggplot2 ####

library(ggplot2)
library(reshape2) # for melt
Dat3d <- melt(Data)
names(Dat3d) <- c("x", "y", "z")
v <- ggplot(Data, aes(x, y, z = z))
v + geom_tile(aes(fill = z)) + stat_contour()
v

