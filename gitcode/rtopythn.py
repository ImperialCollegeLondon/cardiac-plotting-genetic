#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
Created on Sun Nov 17 13:38:14 2019

@author: mt1e16
"""


from rpy2.robjects.packages import importr
import numpy as np
import pandas as pd
import os
from rpy2.robjects import pandas2ri
from rpy2.robjects.vectors import FloatVector

pandas2ri.activate()

#importing R package custom.analytics and replacing . with _ in #package names to ensure no conflicts
d = {'package.dependencies': 'package_dot_dependencies',
     'package_dependencies': 'package_uscore_dependencies'}

path = '/Users/mt1e16/Desktop/PostDoc/R_work/r2py_code_data/'

#stats = importr('stats', robject_translations=d)
base = importr('base', robject_translations=d)
utils = importr('utils', robject_translations=d)
multtest = importr('multtest', robject_translations=d)
mutools3D = importr('mutools3D', robject_translations=d)
nofCores = 48

#IMAGING DATA MATRIX
#NCOL = N POINTS ON THE ATLAS
#NROW = N PATIENTS
Y = base.readRDS(os.path.join(path,"data/WTedLVepi.rds"))
print(Y.shape)
#DATA PRE-PROCESSING
Y = base.scale(Y)

#CLINICAL DATA MATRIX
#NROW = N PATIENTS
X=base.readRDS(os.path.join(path,"data/WTedmodel.rds"))

print(X)
print(X.shape)
#NUMBER OF PERMUTATIONS
nPermutations = 1000
##READ THE NNLIST ASSOCIATED TO EACH VERTEX
NNmatrix = base.readRDS(os.path.join(path,"data/redepiNNmatrix.rds"))
#print(np.shape(NNmatrix))
##READ AREAS ASSOCIATED TO EACH VERTEX
A = base.readRDS(os.path.join(path,"data/epiLVareas.rds"))
#print(np.shape(A))
#READ MESH COORDINATES
meshCoordinates = pd.read_csv(os.path.join(path,"data/meshcoordinates.txt"), sep=" ", header=None)
#print(meshCoordinates.shape)

#SPECIFY IF YOU WANT TO STUDY THE FULL SHAPE OR ONLY THE EPICARDIUM OR THE ENDOCARDIUM
whichEE = 2
#1 endo, 2 epi, 3 full shape
#endoEpi = pd.read_csv(os.path.join(path,"r2py_code_data/data/endo_epi.txt"), sep=" ", header=None)
#vert2print = list(which(endoEpi[,4]==0),which(endoEpi[,4]==1),1:length(endoEpi[,4]))

extract = 7
#extractnames = list(X.columns) 
# MASS UNIVARIATE REGRESSION
result = mutools3D.murHC4m(X,Y,extract)
print(result.shape)

beta = np.delete(result, np.s_[1:3], axis=1)
print(beta)
pv = np.delete(result, np.s_[0:2], axis=1)
print(pv)

#MULTIPLE TESTING CORRECTION

corrected = multtest.mt_rawp2adjp(FloatVector(pv), proc="BH", na_rm = False)
#print(corrected)
cr=corrected[0]
ci=base.order(corrected[1])
crdf = pd.DataFrame(cr[:,1])
BHpvalues = np.asarray(crdf.loc[ci])
print(BHpvalues)
print(len(BHpvalues))

#### TFCE ####
#sign = mutools3D.permFL(X,Y,extract,A,NNmatrix,nPermutations, True, True, nofCores, E = 0.5, H = 2)
#print(sign)
#pv2 = np.delete(sign, np.s_[1:2], axis=1)
#print(pv2)
#pfdr5TSBH = multtest.mt_rawp2adjp(FloatVector(pv2), proc="BH", na_rm = False)
#cr2=pfdr5TSBH[0]
#ci2=base.order(pfdr5TSBH[1])
#crdf2 = pd.DataFrame(cr2[:,1])
#BHpvaluesTFCE = np.asarray(crdf2.loc[ci2])
#print(BHpvaluesTFCE)
#print(len(BHpvaluesTFCE))


# prepare the data
Data = pd.concat([meshCoordinates, pd.DataFrame(beta)], axis=1)
Data.columns = ["x", "y", "z", "w"]

Data_pvalues = pd.concat([meshCoordinates, pd.DataFrame(BHpvalues)], axis=1)
Data_pvalues.columns = ["x", "y", "z", "p"]

Data=Data[Data["w"] < 9999]
Data_pvalues=Data_pvalues[Data_pvalues["p"] < 9999]
print(Data)
print(Data_pvalues)
print(Data.shape)
print(Data_pvalues.shape)
#print(Data.shape)
#print(Data_pvalues.shape)
pvaluesTFCE=Data_pvalues["p"]
pos=np.where(pvaluesTFCE <=0.05)
print(len(Data.x))
print(len(pos[0]))
print(pos)
df=Data.loc[pos]
df.columns = ["x", "y", "z", "w"]

print(df.shape)
sig=round((len(pos[0])/len(Data.x))*100)
print(sig)
average_beta=round(df["w"].mean(),2)
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
        color= "lightgrey",#Data.w,
        colorbar=dict(
            title="beta (a.u.)"
        ),                
        colorscale='RdBu', showscale = False,reversescale=True
    ),showlegend=False
)])
#fig.update_layout(title='Beta coefficient for WT vs SBP', autosize=True,
 #                 scene = dict(xaxis = ax, yaxis = ax,zaxis = ax),
 #                 scene_camera_eye=dict(x=math.cos(2.8)*2, y= math.sin(2.8)*2, z=-0.25)
#)        
#fig.show()
fig.add_trace(go.Scatter3d(x=df["x"], y=df["y"], z=df["z"],
                           marker=dict(size=10,cauto=False,
                                       cmax=df["w"].max(),
                                       cmin=-df["w"].max(),
                                       color=df["w"],
                                       colorbar=dict(
                                               title="beta_pvalues (a.u.)"
                                               ),
                                               colorscale='RdBu', 
                                               showscale = True,reversescale=True),
                                               mode="markers",
                                               showlegend=False
    ))
                                              

fig.update_layout(title='Beta coefficient for WT vs SBP', autosize=True,
                  scene = dict(xaxis = ax, yaxis = ax,zaxis = ax),
                  scene_camera_eye=dict(x=math.cos(2.8)*2, y= math.sin(2.8)*2, z=-0.25)
)
fig.show()
if not os.path.exists("images"):
    os.mkdir("images")
    fig.write_image("images/SBP_beta.png")  
if not os.path.exists("data_results"):
    os.mkdir("data_results")
    Data.to_csv(r'data_results/Data_beta.txt', header=None, index=None, sep=' ', mode='a')
    Data_pvalues.to_csv(r'data_results/Data_pvalues.txt', header=None, index=None, sep=' ', mode='a')
