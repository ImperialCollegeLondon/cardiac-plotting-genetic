# -*- coding: utf-8 -*-
"""
Created on Mon Oct 14 10:39:12 2019

@author: mthanaj
"""


import imageio
import os 


os.chdir("/home/mthanaj@isd.csc.mrc.ac.uk")
path_data = "./cardiac/Experiments_of_Maria/10102019_Motion_plots/pngs/pngsd/"
path_out  = "./cardiac/Experiments_of_Maria/10102019_Motion_plots/pngs/"
#path = '/mnt/storage/home/mthanaj/cardiac/Experiments_of_Maria/10102019_Motion_plots/pngs/'

filenames = [f for f in os.listdir(path_data) if f.endswith('.png')]
#print(filenames)
images = []
for filename in filenames:
    images.append(imageio.imread(path_data + filename))
imageio.mimsave(path_out + 'RVsd.gif', images, duration=0.05)