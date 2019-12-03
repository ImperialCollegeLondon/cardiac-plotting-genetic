# -*- coding: utf-8 -*-
"""
Created on Fri Nov 22 10:43:50 2019

@author: mthanaj
"""
#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
Created on Sun Nov 17 13:38:14 2019

@author: mt1e16
"""

import numpy as np
import pandas as pd
import os

os.chdir("Z:/Experiments_of_Maria")
path_data = "./20191122_rtopython/data"


# prepare the data
Data = pd.read_csv(os.path.join(path_data,"BSA_beta.txt"), sep=" ", header=None)
Data.columns = ["x", "y", "z", "w"]
Data_pvalues = pd.read_csv(os.path.join(path_data,"BSA_BHpvaluesTFCE.txt"), sep=" ", header=None)
Data_pvalues.columns = ["x", "y", "z", "p"]

Data=Data[Data["w"] < 9999]
Data_pvalues=Data_pvalues[Data_pvalues["p"] < 9999]

#print(Data.shape)
#print(Data_pvalues.shape)
pvaluesTFCE=Data_pvalues["p"]
pos=np.where(pvaluesTFCE <=0.05)
#print(len(Data.x))
print(len(pos[0]))
#pos[pos.columns[0]]
df=Data.iloc[pos]
df.columns = ["x", "y", "z", "w"]
print(df)
sig=(len(pos[0])/len(Data.x))*100
print(sig)
average_beta=df["w"].mean()
print(average_beta)

# plot
import math

import plotly.graph_objects as go
ax = dict(title_text = "",
          showgrid = False, 
          zeroline = False, 
          showline = False,
          showticklabels = False,
          showbackground= False)
fig = go.Figure(data=[go.Scatter3d(
     x=Data.x, y=Data.y, z=Data.z,
    mode='markers',
    marker=dict(
        size=10,cauto=False,
        cmax=Data["w"].max()*2,
        cmin=-Data["w"].max()*2,
        color=Data.w,
        colorbar=dict(
            title="beta (a.u.)"
        ),                
        colorscale="RdBu", showscale = False,reversescale=True,
    ),showlegend=False
)])

fig.add_trace(go.Scatter3d(x=df["x"], y=df["y"], z=df["z"],
                           marker=dict(size=10,cauto=False,
                                       cmax=df["w"].max(),
                                       cmin=df["w"].min(),
                                       color=df["w"],
                                       colorbar=dict(
                                               title="beta_pvalues (a.u.)"
                                               ),
                                               colorscale='Reds',
                                               showscale = True,reversescale=False),
                                               mode="markers",
                                               showlegend=False
    ))
                                              

fig.update_layout(title='Beta coefficient for WT vs BSA', autosize=False,scene = dict(xaxis = ax, yaxis = ax,zaxis = ax),
                  scene_camera_eye=dict(x=math.cos(2.8)*2, y= math.sin(2.8)*2, z=-0.25)
)
fig.show()
#if not os.path.exists("images"):
#    os.mkdir("images")

#fig.write_image("BSA_beta.png")
