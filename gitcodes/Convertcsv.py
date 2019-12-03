# -*- coding: utf-8 -*-
"""
Created on Mon Oct 14 10:39:12 2019

@author: mthanaj
"""

# pip install vtk


import numpy as np  

import vtk
import csv
import os

os.chdir("/home/mthanaj@isd.csc.mrc.ac.uk")
path_data = "./cardiac/UKBB_40616/meshes/biobank/1350978/motion"
path_out  = "./cardiac/Experiments_of_Maria/output_31103019_Plotmotion/1350978"
# debug 
filenames = [f for f in os.listdir(path_data) if f.endswith('.vtk')]
filenames.sort(reverse = False)
i=0

for fileIn in filenames:
    print("\n ... " + fileIn)
    reader    = vtk.vtkGenericDataObjectReader()
    reader.SetFileName(os.path.join(path_data,fileIn))
    reader.Update()
    point_obj = reader.GetOutput()
    points    = point_obj.GetPoints()
    table     = vtk.vtkDataObjectToTable()
    table.SetInputData(point_obj)
    table.Update()
    table.GetOutput().AddColumn(points.GetData())
    table.Update()
    writer    = vtk.vtkDelimitedTextWriter()
    writer.SetInputConnection(table.GetOutputPort())
    writer.SetFileName(os.path.join(path_out,'RV_fr' + str(i)))
    writer.Update()
    writer.Write()
    i+=1
    
    