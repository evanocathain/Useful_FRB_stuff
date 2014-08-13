#/usr/bin/python

# 07/08/2014
# Evan Keane
# A script to calculate the relative S/N for an incorrect DM

# Import some useful stuff
#from scipy.special import erf
import math as m
import sys
if (len(sys.argv) != 3):
    print "Usage: python dDM_S.py <Wint_ms> <DM>"
    sys.exit()
Wint = float(sys.argv[1])
DM   = float(sys.argv[2])

# Parameters for the Archival Parkes Data wherein FRB010125 was found
BW      = 288.0                           # BW in MHz
fcentre = 1.374                           # fcentre in GHz
nchans  = 96
tsamp   = 0.125                           # tsamp in ms
tBW     = 0.000651                        # tBW in ms

# Parameters for the HTRU Data wherein the Thornton bursts were found
BW      = 400.0                           # BW in MHz
fcentre = 1.382                           # fcentre in GHz
nchans  = 1024
tsamp   = 0.064                           # tsamp in ms
tBW     = 0.000333                        # tBW in ms

# 'Typical' FRB DM
DM      = 1000.0                          # DM in cm**-3*pc

# Work out a few other parameters
fchan   = BW/nchans                       # fchan in MHz

# Define function to calculate fractional SNR as a function of dDM
def dDM_fracSNR (Wint, DM, fchan, fcentre) :
    """Calculate the relative S/N for a range of incorrect DM values for a
    given DM, intrinsic width, observing frequency and frequency
    resolution"""
    tDM     = 0.001*8.3*DM*fchan/(fcentre**3) # tDM in ms
    print "Wint:",Wint,"ms, DM:",DM
    print "dDM\tFrac_SNR"
    dDM_stepsize = 0.25
    fcentre = 1000*fcentre # in MHz
    for i in range(1,20):
        dDM      = dDM_stepsize*i
        # tdDM in ms, approx 1
        #    tdDM     = 0.001*8.3*dDM*BW/((fcentre*0.001)**3)
        # tdDM in ms, approx 2
        tdDM     = 1000*4150*dDM*((fcentre-0.5*BW)**(-2) - (fcentre+0.5*BW)**(-2))
        # tdDM in ms, approx3
        #    tdDM = 0.0
        #    for j in range(0,nchans):
        #        f = fcentre + 0.5*BW - 0.5*(BW/nchans) - j*(BW/nchans)
        #        tdDM += (fcentre/f)**2*(0.001*8.3*dDM*(BW/nchans)/((f*0.001)**3))
        
        betaSq   = Wint*Wint + tsamp*tsamp + tBW*tBW
        Wobs_dDM = m.sqrt( betaSq + tDM*tDM + tdDM*tdDM )
        Wobs_DM  = m.sqrt( betaSq + tDM*tDM )
        frac_SNR = m.sqrt(Wobs_DM/Wobs_dDM)
        # And can check the theoretical value from CM03 eqns. 12 & 13
        #    zeta     = 0.00691*dDM*BW/(Wobs_DM*(fcentre*0.001)**3)
        #    S        = 0.5*m.sqrt(m.pi)*erf(zeta)/zeta
        print "%.4f\t%.4f" % (dDM,frac_SNR)

# Calculate the sensitivity loss, for various dDM values, for 2
# different intrinsic pulse widths. Firstly the default (in heimdall
# and dedisperse_all) narrow width which is 40 us.
#Wint    = 0.040                           # W in ms
#dDM_fracSNR(Wint, DM, fchan, fcentre)
#
# Secondly for a perhaps more realistic intrinsic pulse width for an
# FRB, 1 ms.
#Wint = 1.000
#dDM_fracSNR(Wint, DM, fchan, fcentre)

dDM_fracSNR(Wint, DM, fchan, fcentre)
