import os 
import glob
import sys
from vtk import *
from ReadPoints import *

# to run in frankie
test_path = "/mnt/storage/home/mthanaj/cardiac/xyz"
dir_list = os.listdir(test_path)

def txt2vtk(pathin,pathout):
    data=vtk.vtkUnstructuredGrid()
    data.SetPoints(readPoints(pathin))
    Data=vtk.vtkUnstructuredGridWriter()
    Data.SetInputData(data)
    Data.SetFileName(pathout)
    Data.Update()
    Data.Write()


for dir_pat in dir_list:
    p_folder = os.path.join(test_path,dir_pat)
    path_in = glob.glob(p_folder+'/*.txt')
    txt2vtk(path_in[0],os.path.join(p_folder,"LVed_"+str(dir_pat)+".vtk"))
