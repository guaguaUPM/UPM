import matplotlib.pyplot as plt

T, S, R, Z = [], [], [], []
for line in open('T_SZR.dat', 'r'):
  values = [float(s) for s in line.split()]
  T.append(values[0])
  S.append(values[1])
  R.append(values[2])
  Z.append(values[3])

plt.plot(T,Z,'b-',T,S,'r-')
plt.show()