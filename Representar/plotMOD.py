import matplotlib.pyplot as plt

X, Y = [], []
for line in open('valores.dat', 'r'):
  values = [float(s) for s in line.split()]
  X.append(values[0])
  Y.append(values[1])

U, V = [], []
for line in open('puntos.rawK.dat', 'r'):
  values = [float(s) for s in line.split()]
  U.append(values[0])
  V.append(values[1])

plt.rc('grid', linestyle="dotted", color='grey')
plt.plot(X, Y, 'r-', U, V, 'bs', markersize=3)
plt.grid(True)
# plt.gca().invert_xaxis()
plt.show()