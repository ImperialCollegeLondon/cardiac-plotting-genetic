# -*- coding: utf-8 -*-
"""
Created on Mon Oct 14 10:39:12 2019

@author: mthanaj
"""


import imageio
import os 

# to run in trillian
os.chdir("/home/mthanaj@isd.csc.mrc.ac.uk")
path_data = "./cardiac/xyz_inputpng"
path_out  = "./cardiac/xyz_output"

filenames = [f for f in os.listdir(path_data) if f.endswith('.png')]
images = []
for filename in filenames:
    images.append(imageio.imread(path_data + filename))
imageio.mimsave(path_out + 'LV_xyz.gif', images, duration=0.05)