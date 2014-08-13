#/usr/local/python-2.7.2/bin/python

import scipy.special
import matplotlib.pyplot as plt
import numpy as np
import math as m

bw = 400.0
fcentre = 1.382
ndms = 1
ndeltadms = 50
sensitivity = np.zeros(shape=(ndms,ndeltadms))

#for i in range(0,ndms):
#dm = (i+1)*100.0
dm = 1000.0
w_ms = 0.001*m.sqrt(1000**2 + 64**2 + (8.3*dm*0.39625/(1.352**3))**2)

for delta_dm in range(0,ndeltadms):
    zeta = 6.91*0.001*(delta_dm+0.1)*bw/(fcentre**3*w_ms)
    sensitivity[0][delta_dm] = (m.sqrt(m.pi)/2.0)*scipy.special.erf(zeta)/zeta
#    print dm+0.1, sensitivity

plt.plot(sensitivity[0])
plt.show()
