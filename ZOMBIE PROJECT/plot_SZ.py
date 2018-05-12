import matplotlib.pyplot as plt

X, Y = [], []
for line in open('T_Z.dat', 'r'):
  values = [float(s) for s in line.split()]
  X.append(values[0])
  Y.append(values[1])

U, V = [], []
for line in open('T_S.dat', 'r'):
  values = [float(s) for s in line.split()]
  U.append(values[0])
  V.append(values[1])



plt.plot(U,V,'b-',X,Y,'r-')
plt.show()