import numpy as np
import matplotlib.pyplot as plt

d=np.loadtxt('snap_00000',skiprows=2)
x=np.sort(d[:,0])
dx=np.diff(x)

print(dx[:10])
plt.plot(x[:-1],dx,'-')
plt.plot(x[:-1],dx,'.')
plt.show()
