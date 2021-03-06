import matplotlib.pyplot as plt

X, Y = [], []
for line in open('puntos.dat', 'r'):
  values = [float(s) for s in line.split()]
  X.append(values[0])
  Y.append(values[1])

plt.rc('grid', linestyle='dotted', color='grey')
plt.plot(X, Y)
plt.grid(True)
plt.savefig('plot.png')
plt.show()