# -*- coding: utf-8 -*-
"""
Created on Mon Oct 14 10:39:12 2019

@author: mthanaj
"""
import numpy as np  

import vtk
import csv
import os
path = 'Z:/UKBB_40616/meshes/biobank/1/motion/'
filenames = [f for f in os.listdir(path) if f.endswith('.vtk')]
filenames.sort(reverse = False)
int=0

for fileIn in filenames:
    #fileIn  = 'RV_fr00.vtk'
    #fileOut  = 'RV_fr.csv'
    
    reader = vtk.vtkGenericDataObjectReader()
    reader.SetFileName(fileIn)
    reader.Update()
    point_obj = reader.GetOutput()
    points = point_obj.GetPoints()
    
    table = vtk.vtkDataObjectToTable()
    table.SetInputData(point_obj)
    table.Update()
    table.GetOutput().AddColumn(points.GetData())
    table.Update()
    
    fileOut='RV_fr' + str(int)
    writer = vtk.vtkDelimitedTextWriter()
    writer.SetInputConnection(table.GetOutputPort())
    writer.SetFileName(fileOut)
    writer.Update()
    writer.Write()
    int=int+1
    