# -*- coding: utf-8 -*-
"""
Created on Mon Oct 14 10:39:12 2019

@author: mthanaj
"""


import imageio
import os 
path = 'Z:/Experiments_of_Maria/10102019_Motion_plots/pngs/'

filenames = [f for f in os.listdir(path) if f.endswith('.png')]
images = []
for filename in filenames:
    images.append(imageio.imread(path + filename))
imageio.mimsave(path + 'movie.gif', images, duration=[0.2, 0.2, 0.5, 0.2, 0.2, 0.2], loop=1)