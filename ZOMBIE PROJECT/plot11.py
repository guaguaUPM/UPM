import matplotlib.pyplot as plt

T, S, Z = [], []
for line in open('T_S_Z_R.dat', 'r'):
  values = [float(s) for s in line.split()]
  T.append(values[0])
  S.append(values[1])
  R.append(values[1])

plt.plot(T,S,'r-',T,Z,'b-')
plt.show()