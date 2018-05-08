import matplotlib.pyplot as plt

U, V = [], []
for line in open('T_Z.dat', 'r'):
  values = [float(s) for s in line.split()]
  U.append(values[0])
  V.append(values[1])


plt.plot(U,V,'b-')
plt.show()