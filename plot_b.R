###########################################
rm(list = ls(all = TRUE))  
###########################################

setwd("~cardiac/Experiments_of_Maria/01102019_First_experiments/")

library(plotly)
library(data.table)

Data <- read.table("Z:/Experiments_of_Maria/01102019_First_experiments/BSA_beta.txt", quote="\"", comment.char="")
colnames(Data) <- c("x", "y", "z", "w")
Data_pvalues <- read.table("Z:/Experiments_of_Maria/01102019_First_experiments/BSA_BHpvaluesTFCE.txt", quote="\"", comment.char="")
pvaluesTFCE<-Data_pvalues$V4
pos<-which(pvaluesTFCE < 0.1 & Data$w > 0.1 & Data$w > -0.1)
df = data.frame(Data$x[pos], Data$y[pos], Data$z[pos], Data$w[pos])
colnames(df) <- c("df_x", "df_y", "df_z", "df_w")

# Data <- as.matrix(Data)
ax <- list(
  title = "",
  zeroline = F,
  showline = F,
  showticklabels = F,
  showgrid = F)
p<-plot_ly(Data, x = ~x , y = ~y , z = ~z,
           marker = list(color = ~w, colorbar=list(title='beta (a.u)',len = 0.55, x = 1.13, y = 0.8), 
                         colorscale = c('#1972A4', '#FF7070'), size=10,
                         cauto = F,
                         cmin = -0.2,
                         cmax = 0.2,showscale = TRUE)) %>%
  add_markers() %>%
  layout(scene = list(xaxis = ax,yaxis = ax, zaxis = ax, dragmode="turntable",
                      camera = list(eye = list(x = cos(3.3)*2, y = sin(3.3)*2, z= 0.25))))

p<- add_trace(p = p,
              z = Data$z[pos],
              x = Data$x[pos],
              y = Data$y[pos],type = "scatter3d", mode = "markers",inherit = T,
              marker = list(color =  ~Data$w[pos],colorbar=list(title='beta_BHpvalueTFCE',len = 0.4, x = 1.13, y = 0.4),
                            colorscale = c('#1972A4', '#FF7070'), size=10,
                            cauto= T,
                            cmin = 0,
                            cmax = max(Data$w[pos]), showscale = F))%>%
  layout(showlegend = F)

p

##### save images with 360 rotation and make a gif ####
it<-0
zoom <- 2

for(i in seq(0,6.3,by=0.1))
{
  # 6.3 is enough for a full 360 rotation
  it=it+1
  p<-plot_ly(Data, x = ~x , y = ~y , z = ~z,
             marker = list(color = ~w, colorbar=list(title='beta (a.u)',len = 0.55, x = 1.13, y = 0.8), 
                           colorscale = c('#1972A4', '#FF7070'), size=10,
                           cauto = F,
                           cmin = -0.2,
                           cmax = 0.2,showscale = TRUE)) %>%
    add_markers() %>%
    layout(scene = list(xaxis = ax,yaxis = ax, zaxis = ax, dragmode="turntable",
                        camera = list(eye = list(x = cos(i)*zoom, y = sin(i)*zoom, z= 0.25))))
  
  p<- add_trace(p = p,
                z = Data$z[pos],
                x = Data$x[pos],
                y = Data$y[pos],type = "scatter3d", mode = "markers",inherit = T,
                marker = list(color =  ~Data$w[pos],colorbar=list(title='beta_BHpvalueTFCE',len = 0.4, x = 1.13, y = 0.4),
                              colorscale = c('#1972A4', '#FF7070'), size=10,
                              cauto= T,
                              cmin = 0,
                              cmax = max(Data$w[pos]), showscale = F))%>%
    layout(showlegend = F)

  # The above camera parameters should orbit 
  # horizontally around the chart.
  # The multiplier controls how far out from
  # from the graph centre the camera is (so 
  # is functionally a 'zoom' control).
  setTxtProgressBar(txtProgressBar(style=3),it/length(seq(0,6.3,by=0.1)))
  
  ##### requires orca installation !!! #####
  orca(p, paste("BSA_beta",it, sep="_"))
}

library(magick)
frames <- paste0("Z:/Experiments_of_Maria/01102019_First_experiments//BSA_beta_", 1:10, ".png")
m <- image_read(frames)
m <- image_animate(m, fps=4)
image_write(m, "BSA_beta.gif")

#END

