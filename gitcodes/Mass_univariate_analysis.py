#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
Created on Sun Nov 17 13:38:14 2019

@author: mt1e16
"""

import rpy2.robjects as robjects
#import rpy2.robjects.packages as rpackages
from rpy2.robjects.packages import importr
import numpy as np
import pandas as pd
import os
from rpy2.robjects import pandas2ri
from rpy2.robjects.vectors import FloatVector
#from statsmodels.sandbox.stats.multicomp import multipletests
#import pyreadr
pandas2ri.activate()
#from sklearn.preprocessing import scale
#from pandas import DataFrame
#importing R package custom.analytics and replacing . with _ in #package names to ensure no conflicts
d = {'package.dependencies': 'package_dot_dependencies',
     'package_dependencies': 'package_uscore_dependencies'}

# to run in trillian
os.chdir("/home/mthanaj@isd.csc.mrc.ac.uk")
path_data = "./cardiac/xyz_inputrds"

#stats = importr('stats', robject_translations=d)
base = importr('base', robject_translations=d)
utils = importr('utils', robject_translations=d)
multtest = importr('multtest', robject_translations=d)
mutools3D = importr('mutools3D', robject_translations=d)
nofCores = 48

#readRDS = robjects.r['readRDS']
#IMAGING DATA MATRIX
#NCOL = N POINTS ON THE ATLAS
#NROW = N PATIENTS
Y = utils.read_table("Y.txt",header = True)
print(Y.shape)
#DATA PRE-PROCESSING
Y = np.asarray(Y)
Y = base.scale(Y)

#CLINICAL DATA MATRIX
#NROW = N PATIENTS
X = utils.read_table("X.txt",header = True)
X=np.asarray(X)

##NUMBER OF PERMUTATIONS
nPermutations = 1000
##READ THE NNLIST ASSOCIATED TO EACH VERTEX
NNmatrix = base.readRDS("redepiNNmatrix.rds")

##READ AREAS ASSOCIATED TO EACH VERTEX
A = base.readRDS("epiLVareas.rds")
#READ MESH COORDINATES
meshCoordinates = pd.read_csv("mesh_Coordinates.txt", sep=" ", header=None)

#SPECIFY IF YOU WANT TO STUDY THE FULL SHAPE OR ONLY THE EPICARDIUM OR THE ENDOCARDIUM
whichEE = 2
#1 endo, 2 epi, 3 full shape
endoEpi = pd.read_csv("endo_epi.txt", sep=" ", header=None)

extract = 5

# MASS UNIVARIATE REGRESSION
result = mutools3D.murHC4m(X,Y,extract)
#print(result)
print(result.shape)

beta = np.delete(result, np.s_[1:3], axis=1)
print(beta)
pv = np.delete(result, np.s_[0:2], axis=1)

#MULTIPLE TESTING CORRECTION

corrected = multtest.mt_rawp2adjp(FloatVector(pv), proc="BH", na_rm = False)
BHpvalues = corrected[1]
print(BHpvalues)

# TFCE
sign = mutools3D.permFL(X,Y,extract,A,NNmatrix,nPermutations, True, True, nofCores, E = 0.5, H = 2)
#print(sign)
pv2 = np.delete(sign, np.s_[1:2], axis=1)
pfdr5TSBH = multtest.mt_rawp2adjp(FloatVector(pv2), proc="BH", na_rm = False)
BHpvaluesTFCE = pfdr5TSBH[1]
print(BHpvaluesTFCE)


# prepare the data
#from pandas import DataFrame
Data = pd.concat([meshCoordinates, pd.DataFrame(beta)], axis=1)
Data.columns = ["x", "y", "z", "w"]
Data_pvalues = pd.concat([meshCoordinates, pd.DataFrame(BHpvalues)], axis=1)
Data_pvalues.columns = ["x", "y", "z", "p"]
print(Data.shape)
print(Data_pvalues.shape)
Data=Data[Data["w"] < 9999]
Data_pvalues=Data_pvalues[Data_pvalues["p"] < 9999]

pvaluesTFCE=Data_pvalues["p"]
pos=np.where(pvaluesTFCE <=0.05)
print(len(Data.x))
print(len(pos))
df=Data.iloc[pos]
df.columns = ["x", "y", "z", "w"]
sig=(len(pos)/len(Data.x))*100
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
        cmax=Data["w"].max(),
        cmin=-Data["w"].max(),
        color=Data.w,
        colorbar=dict(
            title="Beta_coefficient (a.u.)"
        ),                
        colorscale='RdBu', showscale = True,reversescale=True
    ),showlegend=False
)])
#fig.update_layout(title='Beta coefficient', autosize=True,
#                  scene = dict(xaxis = ax, yaxis = ax,zaxis = ax),
#                  scene_camera_eye=dict(x=math.cos(2.8)*2, y= math.sin(2.8)*2, z=-0.25)
#)        
#fig.show()
fig.add_trace(go.Scatter3d(x=df["x"], y=df["y"], z=df["z"],
                           marker=dict(size=10,cauto=False,
                                       cmax=df["w"].max(),
                                       cmin=df["w"].min(),
                                       color=df["w"],
                                       colorbar=dict(
                                               title="Beta_coefficient (a.u.)"
                                               ),
                                               colorscale='Reds',
                                               showscale = True,reversescale=False),
                                               mode="markers",
                                               showlegend=False
    ))
                                              

fig.update_layout(title='Beta coefficient', autosize=True,
                  scene = dict(xaxis = ax, yaxis = ax,zaxis = ax),
                  scene_camera_eye=dict(x=math.cos(2.8)*2, y= math.sin(2.8)*2, z=-0.25)
)
fig.show()

if not os.path.exists("images"):
    os.mkdir("images")
fig.write_image("images/plot_beta.png")  
