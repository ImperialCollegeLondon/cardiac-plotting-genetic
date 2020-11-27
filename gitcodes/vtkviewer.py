# -*- coding: utf-8 -*-
"""
Created on Wed Oct 28 12:37:36 2020

@author: mthanaj
"""
import numpy as np  
import os 
import vtk
from vtk.util.numpy_support import vtk_to_numpy

# to run in frankie
test_path = "/mnt/storage/home/mthanaj/cardiac/xyz"
dir_list = os.listdir(test_path)
import meshio
mesh = meshio.read(test_path+'LVed_1001106.vtk')
