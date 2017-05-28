import sys
from types import *
import numpy as np
import f90nml

# import pyfft1mod as pfm

import string


def rightType(val):
    t_int, t_real, t_plex = pfm.fft1d.pycal()
    ints = 'int' + str(t_int * 8)
    reals = 'float' + str(t_real * 8)
    complexs = 'complex' + str(t_plex * 8)
    if type(val) is IntType:
        return np.array([val], ints)
    elif type(val) is FloatType:
        return np.array([val], reals)
    elif type(val) is ComplexType:
        return np.array([val], complexs)


def fType(tn):
    t_int, t_real, t_plex = pfm.fft1d.pycal()
    ints = 'int' + str(t_int * 8)
    reals = 'float' + str(t_real * 8)
    complexs = 'complex' + str(t_plex * 8)
    if tn is 'int':
        return ints
    elif tn is 'real':
        return reals
    elif tn is 'complex':
        return complexs


def loadNamelist(mod, fname, makemissing=False):
    nl = f90nml.read(fname)
    for k in nl[fname].iterkeys():
        d = nl[fname]
        setattr(mod, k, rightType(d[k]))


def loadNamelistRaw(mod, fname, makemissing=False):
    nl = f90nml.read(fname)
    for k in nl[fname].iterkeys():
        d = nl[fname]
        setattr(mod, k, d[k])


def genSig(arg):  # Return a tuple with shape dimensions and data type
    shape = len(arg.shape)  # 0 scaler, 1 >, array
    if np.size(arg) == 1:  # An array with 1 element is a scaler
        shape = 0
    return (shape, str(arg.dtype).rstrip(string.digits))


def findSig(arglist):
    ap = [genSig(arg) for arg in arglist]
    asp = [str(a[0]) + a[1][0] for a in ap]
    return (string.join(asp, ','))


def myFft(dt, tax, data, om, N):
    wax = np.linspace(0, om, N)
    res = np.zeros(N, dtype=np.complex)
    for i, w in enumerate(wax):
        res[i] = np.abs(np.dot(data * dt, np.exp(-1.0j * w * tax)))
    return res, wax


def unpackViktor(data):
    N = np.size(data)
    udata = np.zeros(N + 1, dtype=np.complex)
    ldp = np.imag(data[0])
    udata[0:N] = data[:]
    udata[N] = ldp
    return ldp


# Mathematical assistance functions

# Fourier transforms the time component of phikw.  Requires time axis, tax
# dta is the time step used
def ftPhikw(tax, phikw, dta, omegamax, N):
    pkw = np.zeros((N, np.size(phikw[0])), dtype=np.complex)
    frs = np.zeros((N // 2 + 1, np.size(phikw[0])))  # result
    for i, e in enumerate(phikw):  # copy in to array
        pkw[i, :] = e[:]
        # Fourier transform along x axis
    for i, e in enumerate(phikw[0]):
        print dta
        print tax
        print pkw[:, i]
        ftr, wax = myFft(dta, np.array(tax), pkw[:, i], omegamax, N)
        # frs[:,i] = ftr[:]
        # frs[:,i] = np.abs(np.fft.rfft( np.abs(pkw[:,i]) ) )/np.size(pkw[:,i])

    return frs, wax


if __name__ == "__main__":
    tmax = 10
    wm = 2.0 * np.pi * 50 / tmax
    print wm
    tax = np.linspace(0, tmax, 5000)
    data = np.sin(wm * tax)
