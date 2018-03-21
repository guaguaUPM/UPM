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

plt.plot(U, V, 'r')
plt.plot(X, Y, 'b')

plt.gca().invert_xaxis()
ax = plt.gca()
ax.set_xscale('log')
# ax.set_yscale('log')
# plt.savefig('desviacion.pdf', format='pdf', dpi=900)
plt.show()