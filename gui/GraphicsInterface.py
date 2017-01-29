import numpy


class GraphicsInterface:
    def __init__(self, pc):
        self.pc = pc
        self.dt = 1

    def dscaler1(self, f, label, itime, isc, ist, nx, irc):
        """
        ! displays 1d scalar field in real space
        ! f = 1d scalar field in real space
        ! label = field label
        ! itime = current time step
        ! isc = power of 2 scale of range of values of f
        ! if abs(isc) < 116, then the isc value passed is used for scale.
        ! if abs(isc) > 116, then the program finds the minimum value of isc
        ! ist = flag for choosing positive and/or negative values
        ! the plot has a scale in y given by ymax and ymin.
        ! if ist = 0, then ymax = 2**isc and ymin = -2**isc.
        ! if ist = 1, then ymax = 2**isc and ymin = 0.
        ! if ist = -1, then ymax = 0 and ymin = -2**isc.
        ! if ist = 2, then ymin = fmin, ymax = fmin + 2**ir,
        ! where fmin/fmax are the function minimum/maximum, 
        ! and ir = power of 2 scale for (fmax - fmin)
        ! nx = system length in x direction
        ! irc = return code (0 = normal return)
        """
        pc = self.pc
        edenx = numpy.array(range(nx))
        edeny = numpy.array(f[0:nx])
        pc.showSimple([label, label], [edenx], [edeny], "Time=" + str(itime))

    def displayfv1(self, fv, fvm, label, itime, nmv, idt, irc):
        """
        ! displays velocity distribution functions
        ! fv = velocity distribution
        ! fvm = velocity moments
        ! label = long character string label for plot
        ! itime = current time step
        ! nmv = number of velocity intervals
        ! idt = (1,2,3) = display (individual,composite,both) functions
        ! irc = return code (0 = normal return)
        """
        w, h = numpy.shape(fv)
        s = ["x", "y", "z"]
        self.pc.showVelocity(fv, s[:h], fvm=fvm, plottype=label)

    def dpmgrasp1(self, ppart, kpic, label, itime, isc, nx, iyp, ixp, ntsc, irc):
        a, b, c = numpy.shape(ppart)
        phasearr = numpy.empty((2, b, c), dtype=ppart.dtype)
        phasearr[1, :, :] = ppart[iyp - 1, :, :]
        phasearr[0, :, :] = ppart[ixp - 1, :, :]
        self.pc.showPhase(phasearr, kpic, plottype=label)

    def dvector1(self, f, label, itime, isc, ist, idm, nx, irc, axislabels=["y", "z"]):
        edenx = range(nx)
        w, h = numpy.shape(f)
        assert (w == len(axislabels))
        self.pc.showSimple([label] + axislabels, [edenx, edenx], [f[0, :nx], f[1, :nx]], "Time=" + str(itime * self.dt))


"""
pc.showSimple(["VECPOTENTIAL","y","z"],[edenx,edenx],[sb1.vfield[0,:nx],sb1.vfield[1,:nx]],"Time="+str(ntime*in1.dt))
            graf1.dvector1(sb1.vfield,' VECTOR POTENTIAL',ntime,999,0,2, nx,irc)

"""
