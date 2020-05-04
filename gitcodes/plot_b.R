###########################################
rm(list = ls(all = TRUE))  
###########################################

setwd("")
library(plotly)
library(data.table)
library(dplyr)

Data <- read.table("X_beta.txt", quote="\"", comment.char="")
colnames(Data)<-c("x","y","z","w")
Data_pvalues <- read.table("X_BHpvaluesTFCE.txt", quote="\"", comment.char="")
colnames(Data_pvalues)<-c("x","y","z","p")

endoEpi <- read.table("endo_epi.txt")
vert2print <- list(which(endoEpi[,4]==0),which(endoEpi[,4]==1),1:length(endoEpi[,4]))

Data <- Data[vert2print[[2]],]
Data_pvalues <- Data_pvalues[vert2print[[2]],]
pos<-which(Data_pvalues$p < 0.05)
df = data.frame(Data$x[pos], Data$y[pos], Data$z[pos], Data$w[pos])
colnames(df) <- c("df_x", "df_y", "df_z", "df_w")

sig <- (length(pos)/nrow(Data))*100
significance_area<-format(round(sig, 0), nsmall = 0)
average_beta <- format(round(mean(Data$w[pos]), 3), nsmall = 2)
average_pvalue <- mean(pvaluesTFCE[pos])

ax <- list(
  title = "",
  zeroline = F,
  showline = F,
  showticklabels = F,
  showgrid = F)
p<-plot_ly(Data, x = ~x , y = ~y , z = ~z,
           marker = list(color = "lightgrey",colorbar=list(title='Beta_coefficient - (a.u.)',len = 0.55, x = 1.13, y = 0.6), 
                         colorscale = c('#1972A4', '#FF7070'),
                         cauto = F,
                         cmin = -1,
                         cmax = 1,showscale = F)) %>%
  add_markers() %>%
  layout(title = paste("<b>Beta coefficient <b>\n with average beta = ",average_beta, "and significant area = ",significance_area, "% "),
         scene = list(xaxis = ax,yaxis = ax, zaxis = ax, dragmode="turntable",
                      camera = list(eye = list(x = cos(3.3)*2, y = sin(3.3)*2, z= 0.23))))%>%
 
  add_trace(z = Data$z[pos],
            x = Data$x[pos],
            y = Data$y[pos],type = "scatter3d", mode = "markers",inherit = T,
            marker = list(color =  Data$w[pos],colorbar=list(title='Beta_coefficient - (a.u.)',len = 0.55, x = 1.13, y = 0.6),
                          colorscale = c('#1972A4', '#FF7070'),
                          cauto= F,
                          cmin = -max(Data$w[pos]),
                          cmax = max(Data$w[pos]), showscale = T))%>%
  layout(showlegend = F)
p


 orca(p, paste("X_plot_beta"), more_args = c("-d",path_to_output ))

#END
