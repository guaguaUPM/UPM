import matplotlib.pyplot as plt

X, Y = [], []
for line in open('error.dat', 'r'):
  values = [float(s) for s in line.split()]
  X.append(values[0])
  Y.append(values[1])

U, V = [], []
for line in open('ref.dat', 'r'):
  values = [float(s) for s in line.split()]
  U.append(values[0])
  V.append(values[1])

plt.plot(X, Y, 'b')
plt.plot(U, V, 'r')
plt.savefig('desviacion.pdf', format='pdf', dpi=900)
plt.show()