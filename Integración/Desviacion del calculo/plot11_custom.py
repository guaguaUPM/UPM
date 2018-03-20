import matplotlib.pyplot as plt

X, Y = [], []
for line in open('errorR.dat', 'r'):
  values = [float(s) for s in line.split()]
  X.append(values[0])
  Y.append(values[1])

U, V = [], []
for line in open('errorT.dat', 'r'):
  values = [float(s) for s in line.split()]
  U.append(values[0])
  V.append(values[1])

A, B = [], []
for line in open('errorS.dat', 'r'):
  values = [float(s) for s in line.split()]
  A.append(values[0])
  B.append(values[1])

plt.plot(X, Y, 'r')
plt.plot(U, V, 'g')
plt.plot(A, B, 'b')
# plt.gca().invert_xaxis()
ax = plt.gca()
ax.set_yscale('log')
# ax.set_yscale('log')
# plt.savefig('desviacion.pdf', format='pdf', dpi=900)
plt.show()