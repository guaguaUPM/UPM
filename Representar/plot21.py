import matplotlib.pyplot as plt
from mpl_toolkits.mplot3d import Axes3D
import numpy as np

X, Y, Z = [], [], []
for line in open('valores.dat', 'r'):
  values = [float(s) for s in line.split()]
  X.append(values[0])
  Y.append(values[1])
  Z.append(values[2])

fig = plt.figure()
# version 1.0.x syntax:
#ax = fig.add_subplot(111, projection='3d')
# version 0.99.x syntax: (accepted by 1.0.x as well)
ax = Axes3D(fig)


fig.plot_surface(X,Y,Z)