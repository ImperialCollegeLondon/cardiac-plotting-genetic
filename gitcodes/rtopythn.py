#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
Created on Sun Nov 17 13:38:14 2019

@author: mt1e16
"""

#import rpy2.robjects as robjects
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

os.chdir("Z:/Experiments_of_Maria")
path_data = "./20191122_rtopython/data"
#from sklearn.preprocessing import scale
#from pandas import DataFrame
#importing R package custom.analytics and replacing . with _ in #package names to ensure no conflicts

#path = '/Users/mt1e16/Desktop/PostDoc/R_work/'

base = importr('base', robject_translations=d)
utils = importr('utils', robject_translations=d)
#multtest = importr('multtest', robject_translations=d)
mutools3D = importr('mutools3D', robject_translations=d)
nofCores = 48

#readRDS = robjects.r['readRDS']
#IMAGING DATA MATRIX
#NCOL = N POINTS ON THE ATLAS
#NROW = N PATIENTS
Y = base.readRDS(os.path.join(path_data,"WTedLVepi.rds"),header = True)
#print(Y.shape)
#DATA PRE-PROCESSING
#Y = np.asarray(Y)
Y = base.scale(Y)

#CLINICAL DATA MATRIX
#NROW = N PATIENTS
X = base.readRDS(os.path.join(path_data,"WTedmodel.rds"),header = True)
#del X['X']
#X=X.as_matrix()
print(X)
print(X.shape)
#NUMBER OF PERMUTATIONS
nPermutations = 1000
##READ THE NNLIST ASSOCIATED TO EACH VERTEX
NNmatrix = base.readRDS(os.path.join(path_data,"redepiNNmatrix.rds"))
#print(np.shape(NNmatrix))
##READ AREAS ASSOCIATED TO EACH VERTEX
A = base.readRDS(os.path.join(path_data,"epiLVareas.rds"))
#print(np.shape(A))
#READ MESH COORDINATES
meshCoordinates = pd.read_csv(os.path.join(path_data,"meshCoordinates.txt"), sep=" ", header=None)
print(meshCoordinates.shape)
# meshCoordinates = meshCoordinates[,-4]
#meshCoordinates = cbind(meshCoordinates,99999)
#SPECIFY IF YOU WANT TO STUDY THE FULL SHAPE OR ONLY THE EPICARDIUM OR THE ENDOCARDIUM
whichEE = 2
#1 endo, 2 epi, 3 full shape
endoEpi = pd.read_csv('./cardiac/Experiments_of_Maria/20191122_rtopython/data/endo_epi.txt', sep=" ", header=None)
#vert2print = list(which(endoEpi[,4]==0),which(endoEpi[,4]==1),1:length(endoEpi[,4]))

extract = 6
#extractnames = list(X.columns) 
# MASS UNIVARIATE REGRESSION
result = mutools3D.murHC4m(X,Y,extract)
beta = np.delete(result, np.s_[1:3], axis=1)
#beta=bt.tolist()
print(beta)
#print(bt.shape)
pv = np.delete(result, np.s_[0:2], axis=1)
#pvals=pv.tolist()
print(pv)
#print(result.shape)
#MULTIPLE TESTING CORRECTION
#corrected = multtest.mt.rawp2adjp(result[,3], proc=c("BH"), na.rm = FALSE)
#pvalueADJ5tsbh = array(dim = length(result[,3]))
#BHpvalues = corrected.adjp[order(corrected.index),][,2]
corrected = multtest.mt_rawp2adjp(FloatVector(pv), proc="BH", na_rm = False)
#print(corrected)
cr=corrected[0]
ci=base.order(corrected[1])
crdf = pd.DataFrame(cr[:,1])
BHpvalues = np.asarray(crdf.loc[ci])
print(BHpvalues)
print(len(BHpvalues))
#sign = mutools3D.permFL(X,Y,extract,A,NNmatrix,nPermutations, True, True, nofCores, E = 0.5, H = 2)

#pfdr5TSBH = multtest.mt.rawp2adjp(sign[,1], proc=c("BH"), na.rm = FALSE)
#pvalueADJ5tsbh <- array(dim = length(sign[,1+(iEx-1)*2]))
#BHpvaluesTFCE = pfdr5TSBH.adjp[order(pfdr5TSBH.index),][,2]

#from rpy2.robjects.packages import importr
#plotly=robjects.r('plotly')
#datafr=importr('data.frame')

# prepare the data
Data = pd.concat([meshCoordinates, pd.DataFrame(beta)], axis=1)
#Data = pd.read_csv(os.path.join(path_data,"BSA_beta.txt"), sep=" ", header=None)
Data.columns = ["x", "y", "z", "w"]
Data_pvalues = pd.concat([meshCoordinates, pd.DataFrame(BHpvalues)], axis=1)
#Data_pvalues = pd.read_csv(os.path.join(path_data,"BSA_BHpvaluesTFCE.txt"), sep=" ", header=None)
Data_pvalues.columns = ["x", "y", "z", "p"]

Data=Data[Data["w"] < 9999]
Data_pvalues=Data_pvalues[Data_pvalues["p"] < 9999]

#print(Data.shape)
#print(Data_pvalues.shape)
pvaluesTFCE=Data_pvalues["p"]
pos=np.where(pvaluesTFCE <=0.05)
print(len(Data.x))
print(len(pos))
#pos[pos.columns[0]]
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
        cmax=0,
        cmin=0,
        color='lightgrey',
        colorbar=dict(
            title="beta (a.u.)"
        ),                
        colorscale=['#1972A4', '#FF7070'], showscale = False
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
if not os.path.exists("images"):
    os.mkdir("images")
#fig.write_image("images/BSA_beta.png")  
