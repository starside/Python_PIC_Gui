# -----------------------------------------------------------------------
# 1-2/2D Electromagnetic OpenMP PIC code
# written by Viktor K. Decyk and Joshua Kelly, UCLA
# copyright 2016, regents of the university of california
import sys
import math
import numpy

sys.path.append('./mbeps1.source')
from libmpush1 import *
from libmbpush1 import *
from fomplib import *
from fgraf1 import *
from dtimer import *
from PopMenus import *

# override default input data
in1.emf = 1
in1.relativity = 1
# read namelist
iuin = 8
in1.readnml1(iuin)
# override input data
in1.idcode = 2
in1.ndim = 3

# import modules after namelist has been read
import s1
import sb1

"""
This imports the gui code
"""
from types import *  # This is required for the rightType function
import sys

sys.path.append('./gui')
from ProceduralInterface import *
from GraphicsInterface import GraphicsInterface

int_type = numpy.int32
double_type = numpy.float64
float_type = numpy.float32
complex_type = numpy.complex64

"""
Define function that initializes menus
"""


def initialize_menus(pc):
    # initialize all diagnostics from namelist input parameters
    # ntime = current time step
    # initialize energy diagnostic: allocates wt
    pc.addGraph("NOPLOT", "No Plot", autoadd=False)
    if (in1.ntw > 0):
        pc.addGraph("ENERGY", "Energy")  # Enable electron velocity

    if in1.nts > 0:
        labels = ["Vx-x Phase Plot", "Vy-x Phase Plot", "Vz-x Phase Plot"]
        nn = in1.nsxv
        for i in xrange(0, 3):
            if ((nn % 2) == 1):
                pc.addGraph("EPHASE" + str(i), "Electron/" + labels[i])  # Enable electron velocity
                if (in1.movion == 1):  # Ions
                    if ((in1.nds == 2) or (in1.nds == 3)):
                        pc.addGraph("IPHASE" + str(i), "Ions/" + labels[i])  # Enable electron velocity
            nn = int(nn / 2)
        # vx-vy, vx-vz or vy-vz
        labels = ["Vx-Vy Phase Plot", "Vx-Vz Phase Plot", "Vy-Vz Phase Plot"]
        nn = in1.nsvv
        for i in xrange(0, 3):
            if ((nn % 2) == 1):
                pc.addGraph("EPHASEV" + str(i), "Electron/" + labels[i])  # Enable electron velocity
                if (in1.movion == 1):  # Ions
                    if ((in1.nds == 2) or (in1.nds == 3)):
                        pc.addGraph("IPHASEV" + str(i), "Ions/" + labels[i])  # Enable electron velocity
            nn = int(nn / 2)

    if (in1.movion == 1):
        if (in1.ntdi > 0):
            pc.addGraph("IDENSITY", "Density/Ion Density")  # Enable ion velocity
            if ((in1.nddi == 2) or (in1.nddi == 3)):
                pc.addGraph("ION DENSITY OMEGA VS MODE+", "Ion Dispersion/Ion Density Dispersion +")
                pc.addGraph("ION DENSITY OMEGA VS MODE-", "Ion Dispersion/Ion Density Dispersion -", autoadd=False)
                #pc.addGraph("ION DENSITY OMEGA VS MODE LINE", "Ion Dispersion/Ion Density Dispersion Trace")

    if (in1.movion == 1):
        if (in1.ntji > 0):
            if ((in1.ndji == 1) or (in1.ndji == 3)):
                pc.addGraph(' ION CURRENT', "Ions/Ion Current Density")
            if ((in1.ndji == 2) or (in1.ndji == 3)):
                # display frequency spectrum
                pc.addGraph("ION CURRENT OMEGA VS MODEY+", "Ion Dispersion/Ion Current Dispersion Y+")
                pc.addGraph("ION CURRENT OMEGA VS MODEZ+", "Ion Dispersion/Ion Current Dispersion Z+", autoadd=False)
                pc.addGraph("ION CURRENT OMEGA VS MODEY-", "Ion Dispersion/Ion Current Dispersion Y-", autoadd=False)
                pc.addGraph("ION CURRENT OMEGA VS MODEZ-", "Ion Dispersion/Ion Current Dispersion Z-", autoadd=False)

    if (in1.ntde > 0):
        pc.addGraph("EDENSITY", "Density/Electron Density")  # Enable electron velocity

    if (in1.ntp > 0):
        if in1.ndp == 1 or in1.ndp == 3:
            pc.addGraph("DRAWPOT", "Potential/Potential")  # Enable electron velocity
        if in1.ndp == 2 or in1.ndp == 3:
            pc.addGraph("POTENTIAL OMEGA VS MODE+", "Potential/Potential Omega vs Mode +")
            pc.addGraph("POTENTIAL OMEGA VS MODE-", "Potential/Potential Omega vs Mode -", autoadd=False)
            #pc.addGraph("POTENTIAL OMEGA VS MODE LINE", "Potential/Potential Omega vs Mode Trace", autoadd=False)

    if (in1.ntel > 0):
        pc.addGraph("ELFIELD", "E-Field/Longitudinal E-Field")

    if (in1.nta > 0):
        if ((in1.nda == 1) or (in1.nda == 3)):
            pc.addGraph(' VECTOR POTENTIAL', "Vector Potential Diagnostic/Vector Potential")  # Enable electron velocity
        if ((in1.nda == 2) or (in1.nda == 3)):
            pc.addGraph("VECTOR POTENTIAL OMEGA VS MODE Y+",
                        "Vector Potential Diagnostic/Vector Potential:Y+ OMEGA vs MODE")
            pc.addGraph("VECTOR POTENTIAL OMEGA VS MODE Y-",
                        "Vector Potential Diagnostic/Vector Potential:Y- OMEGA vs MODE", autoadd=False)
            pc.addGraph("VECTOR POTENTIAL OMEGA VS MODE Z+",
                        "Vector Potential Diagnostic/Vector Potential:Z+ OMEGA vs MODE", autoadd=False)
            pc.addGraph("VECTOR POTENTIAL OMEGA VS MODE Z-",
                        "Vector Potential Diagnostic/Vector Potential:Z- OMEGA vs MODE", autoadd=False)

        """pc.addGraph("VECTOR POTENTIAL OMEGA VS MODE Y LINE",
                    "Vector Potential Diagnostic/Vector Potential:Y OMEGA vs MODE Trace")
        pc.addGraph("VECTOR POTENTIAL OMEGA VS MODE Z LINE",
                    "Vector Potential Diagnostic/Vector Potential:Z OMEGA vs MODE Trace", autoadd=False)"""

    if (in1.ntet > 0):
        if ((in1.ndet == 1) or (in1.ndet == 3)):
            pc.addGraph("TRANSVERSE E FIELD",
                        "Transverse Electric Field/Transverse Electric Field")  # Enable electron velocity
        if ((in1.ndet == 2) or (in1.ndet == 3)):
            pc.addGraph("FT TRANSVERSE E.F. Y +OMEGA VS MODE",
                        "Transverse Electric Field/Transverse Y Electric Field, +Omega vs Mode")
            pc.addGraph("FT TRANSVERSE E.F. Y -OMEGA VS MODE",
                        "Transverse Electric Field/Transverse Y Electric Field, -Omega vs Mode", autoadd=False)
            pc.addGraph("FT TRANSVERSE E.F. Z +OMEGA VS MODE",
                        "Transverse Electric Field/Transverse Z Electric Field, +Omega vs Mode")
            pc.addGraph("FT TRANSVERSE E.F. Z -OMEGA VS MODE",
                        "Transverse Electric Field/Transverse Z Electric Field, -Omega vs Mode", autoadd=False)

    if (in1.ntb > 0):
        pc.addGraph("BFIELD", "Magnetic Field")  # Enable electron velocity
    if (in1.ntv > 0):
        pc.addGraph("EVELOCITY", "Electron Velocity")  # Enable electron velocity

    if (in1.ntt > 0):
        pc.addGraph("TRAJECTORY", "Particle Trajectory")  # Enable electron velocity

    if (in1.ntar > 0):
        if ((in1.ndar == 1) or (in1.ndar == 3)):
            pc.addGraph(' RADIATIVE VPOTENTIAL',
                        "Radiative Vector Potential Diagnostic/Radiative Vector Potential")  # Enable electron velocity
        if ((in1.ndar == 2) or (in1.ndar == 3)):
            pc.addGraph("RADVECPOT Y +OMEGA", "Radiative Vector Potential Diagnostic/OMEGA VS MODE Fourier: Y +Omega")
            pc.addGraph("RADVECPOT Y -OMEGA", "Radiative Vector Potential Diagnostic/OMEGA VS MODE Fourier: Y -Omega",
                        autoadd=False)
            pc.addGraph("RADVECPOT Z +OMEGA", "Radiative Vector Potential Diagnostic/OMEGA VS MODE Fourier: Z +Omega",
                        autoadd=False)
            pc.addGraph("RADVECPOT Z -OMEGA", "Radiative Vector Potential Diagnostic/OMEGA VS MODE Fourier: Z -Omega",
                        autoadd=False)

def main(*args):
    # init GUI
    pc = PlasmaContext(in1, *args)  # Create GUI
    pc.showGraphs(True)  # enable graphics.  Setting to false will disable graphics
    pc.clearGraphList()  # remove all default graph options
    graf2 = GraphicsInterface(pc)
    in1.timedirection = 0  # default state of the GUI.  MUST BE 0

    # declare scalars for standard code
    npi = 0
    ws = numpy.zeros((1), float_type)

    # declare scalars for OpenMP code
    irc = numpy.zeros((1), int_type)
    ierr = numpy.zeros((1), int_type)

    # declare and initialize timing data
    tinit = 0.0;
    tloop = 0.0
    itime = numpy.empty((4), numpy.int32)
    ltime = numpy.empty((4), numpy.int32)
    dtime = numpy.empty((1), double_type)

    # start timing initialization
    dtimer(dtime, itime, -1)

    # text output file
    fname = "output1." + s1.cdrun
    iuot = open(fname, "w")

    # in1.nvp = number of shared memory nodes (0=default)
    # in1.nvp = int(input("enter number of nodes: "))
    # initialize for shared memory parallel processing
    omplib.init_omp(in1.nvp)

    # open graphics device
    irc[0] = graf1.open_graphs(in1.nplot)

    # initialize scalars for standard code
    # np = total number of electrons in simulation
    np = s1.np
    # nx = number of grid points in x direction
    nx = s1.nx;
    spectrum_scale = 2.0*numpy.pi/float(nx)
    nxh = int(nx / 2)
    # npi = total number of ions in simulation
    if (in1.movion > 0):
        npi = s1.npi
    nxe = nx + 2;
    nxeh = int(nxe / 2)
    # nloop = number of time steps in simulation
    # nstart = initial time loop index
    # ntime = current time step
    nloop = s1.nloop;
    nstart = 0;
    ntime = 0

    # allocate field data for standard code:
    # qe/qi = electron/ion charge density with guard cells
    # fxe = smoothed electric field with guard cells
    # ffc = form factor array for poisson solver
    # mixup = bit reverse table for FFT
    # sct = sine/cosine table for FFT
    # cue = electron current density with guard cells
    # fxyze/byze = smoothed electric/magnetic field with guard cells
    # eyz/byz = transverse electric/magnetic field in fourier space
    sb1.init_fields13()

    # prepare fft tables
    mfft1.mfft1_init(s1.mixup, s1.sct, in1.indx)
    # calculate form factors
    mfield1.mpois1_init(s1.ffc, in1.ax, s1.affp, nx)
    # initialize different ensemble of random numbers
    if (in1.nextrand > 0):
        minit1.mnextran1(in1.nextrand, in1.ndim, np + npi)

    # open reset and restart files; iur, iurr, iur0
    s1.open_restart1()

    # new start
    if (in1.nustrt == 1):
        # initialize electrons: updates ppart, kpic
        # ppart = tiled electron particle arrays
        # kpic = number of electrons in each tile
        sb1.init_electrons13()

        # initialize background charge density: updates qi
        if (in1.movion == 0):
            s1.qi.fill(0.0)
            qmi = -in1.qme
            mpush1.mpost1(s1.ppart, s1.qi, s1.kpic, qmi, s1.tdpost, in1.mx)
            mgard1.maguard1(s1.qi, s1.tguard, nx)

        # initialize ions:  updates pparti, kipic, cui
        # pparti = tiled on particle arrays
        # kipic = number of ions in each tile
        # cui = ion current density with guard cells
        if (in1.movion == 1):
            sb1.init_ions13()

        # initialize transverse electromagnetic fields
        sb1.eyz.fill(numpy.complex(0.0, 0.0))
        sb1.byz.fill(numpy.complex(0.0, 0.0))

    # restart to continue a run which was interrupted
    elif (in1.nustrt == 2):
        sb1.bread_restart13(s1.iur)
        ntime = s1.ntime
        if ((ntime + s1.ntime0) > 0):
            sb1.dth = 0.5 * in1.dt
        nstart = ntime
    # start a new run with data from a previous run
    elif (in1.nustrt == 0):
        sb1.bread_restart13(s1.iur0)
        ntime = s1.ntime
        if ((ntime + s1.ntime0) > 0):
            sb1.dth = 0.5 * in1.dt

    # initialize current fields
    sb1.cue.fill(0.0)

    # set magnitude of external transverse magnetic field
    omt = numpy.sqrt(in1.omy * in1.omy + in1.omz * in1.omz)

    # reverse simulation at end back to start
    if (in1.treverse == in1.timedirection):
        nloop = 2 * nloop
        sb1.nloop = nloop

    # initialize all diagnostics from namelist input parameters
    # wt = energy time history array=
    # pkw = power spectrum for potential
    # pkwdi = power spectrum for ion density
    # wk = maximum frequency as a function of k for potential
    # wkdi = maximum frequency as a function of k for ion density
    # fmse/fmsi = electron/ion fluid moments 
    # fv/fvi = global electron/ion velocity distribution functions
    # fvm/fvmi = electron/ion vdrift, vth, entropy for global distribution
    # fvtm/fvtmi = time history of electron/ion vdrift, vth, and entropy
    # fvtp = velocity distribution function for test particles
    # fvmtp = vdrift, vth, and entropy for test particles
    # partd = trajectory time history array
    # vpkwji = power spectrum for ion current density
    # vwkji = maximum frequency as a function of k for ion current
    # vpkwr = power spectrum for radiative vector potential
    # vwkr = maximum frequency as a function of k for radiative vector
    #        potential
    # vpkw = power spectrum for vector potential
    # vwk = maximum frequency as a function of k for vector potential
    # vpkwet = power spectrum for transverse efield
    # vwket = maximum frequency as a function of k for transverse efield
    # oldcu = previous current density with guard cells
    sb1.initialize_diagnostics13(ntime)

    # read in restart diagnostic file to continue interrupted run
    if (in1.nustrt == 2):
        sb1.dread_restart13(s1.iur)

    # write reset file
    sb1.bwrite_restart13(s1.iurr, ntime)

    # initialization time
    dtimer(dtime, itime, 1)
    tinit = tinit + float(dtime)
    # start timing loop
    dtimer(dtime, ltime, -1)

    if (in1.dt > 0.64 * in1.ci):
        print "Warning: Courant condition may be exceeded!"

    print >> iuot, "program mbbeps1"

    """
    Initialize default windows
    """
    initialize_menus(pc)
    graf2.dt = in1.dt
    PopMenus(pc, in1)

    # sends data the GUI may want to know about the simulation
    pc.updateSimInfo({"tend": in1.tend})
    # * * * start main iteration loop * * *

    for ntime in xrange(nstart, nloop):
        print >> iuot, "ntime = ", ntime
        """
        The following 4 lines process events from the GUI.
        Nothing will happen without calling getEvents
        """
        if ntime == nstart:
            pc.runOnce()
        curtime = ntime * in1.dt
        pc.setTime(curtime, in1.dt)
        pc.getEvents()
        pc.fastForward()

        # debug reset
        #  if (ntime==nloop/2):
        #     sb1.bread_restart13(s1.iurr)
        #     sb1.reset_diags13()
        #     ntime = 0; sb1.dth = 0.0

        # save previous current in fourier space for radiative vector potential
        if (in1.ntar > 0):
            it = ntime / in1.ntar
            if (ntime == in1.ntar * it):
                sb1.oldcu[:] = numpy.copy(sb1.cue)
        # fluid moments diagnostic
        if (in1.ntfm > 0):
            it = int(ntime/in1.ntfm)
            if (ntime==in1.ntfm*it):
                # updates fmse
                sb1.efluidms_diag13(s1.fmse)
                if (in1.movion==1):
                    # updates fmsi
                    sb1.ifluidms_diag13(s1.fmsi) 

        # deposit electron current with OpenMP: updates ppart, kpic, cue
        dtimer(dtime, itime, -1)
        sb1.cue.fill(0.0)
        dtimer(dtime, itime, 1)
        sb1.tdjpost[0] += float(dtime)
        sb1.deposit_ecurrent13(s1.ppart, s1.kpic)

        # deposit ion current with OpenMP: updates pparti, kipic, cui
        if (in1.movion == 1):
            dtimer(dtime, itime, -1)
            sb1.cui.fill(0.0)
            dtimer(dtime, itime, 1)
            sb1.tdjpost[0] += float(dtime)
            sb1.deposit_icurrent13(s1.pparti, s1.kipic)

            # ion current density diagnostic:
            # updates vfield=ion current, vpkwji, vwkji
        if (in1.movion == 1):
            if (in1.ntji > 0):
                it = ntime / in1.ntji
                if (ntime == in1.ntji * it):
                    sb1.icurrent_diag13(sb1.vfield, sb1.vpkwji, sb1.vwkji, ntime)
                    if ((in1.ndji == 1) or (in1.ndji == 3)):
                        # display smoothed ion current
                        edenx = numpy.array(range(nx))
                        # pc.showSimple(["ICURRENTD","Y", "Z"],[edenx, edenx],[sb1.vfield[0,0:nx], sb1.vfield[1,0:nx] ],"Time="+str(ntime*in1.dt)+" Ion Current")
                        graf2.dvector1(sb1.vfield, ' ION CURRENT', ntime, 999, 0, 2, nx, irc, early=in1.ntji)
                        if (irc[0] == 1):
                            break
                        irc[0] = 0
                        # ion spectral analysis
                    if ((in1.ndji == 2) or (in1.ndji == 3)):
                        # display frequency spectrum
                        pc.showSimpleImage("ION CURRENT OMEGA VS MODEY+", sb1.vpkwji[0, ::, :, 0],
                                           "Time=" + str(ntime * in1.dt), extent=(0, in1.modesxji, in1.wimin, in1.wimax), early=in1.ntji,
                                           ticks_scale=spectrum_scale)
                        pc.showSimpleImage("ION CURRENT OMEGA VS MODEY-", sb1.vpkwji[0, ::, :, 1],
                                           "Time=" + str(ntime * in1.dt), extent=(0, in1.modesxji, in1.wimin, in1.wimax), early=in1.ntji,
                                           ticks_scale=spectrum_scale)
                        pc.showSimpleImage("ION CURRENT OMEGA VS MODEZ+", sb1.vpkwji[1, ::, :, 0],
                                           "Time=" + str(ntime * in1.dt), extent=(0, in1.modesxji, in1.wimin, in1.wimax), early=in1.ntji,
                                           ticks_scale=spectrum_scale)
                        pc.showSimpleImage("ION CURRENT OMEGA VS MODEZ-", sb1.vpkwji[1, ::, :, 1],
                                           "Time=" + str(ntime * in1.dt), extent=(0, in1.modesxji, in1.wimin, in1.wimax), early=in1.ntji,
                                           ticks_scale=spectrum_scale)
                        graf1.dmvector1(sb1.vwkji, 'ION CURRENT OMEGA VS MODE',
                                        ntime, 999, 2, 2, in1.modesxji, s1.cwk, irc)
                        if (irc[0] == 1):
                            break
                        irc[0] = 0

                        # deposit charge with OpenMP: updates qe
        dtimer(dtime, itime, -1)
        s1.qe.fill(0.0)
        dtimer(dtime, itime, 1)
        s1.tdpost[0] += float(dtime)
        mpush1.mpost1(s1.ppart, s1.qe, s1.kpic, in1.qme, s1.tdpost, in1.mx)
        # add guard cells: updates qe
        mgard1.maguard1(s1.qe, s1.tguard, nx)

        # electron density diagnostic: updates sfield=electron density
        if (in1.ntde > 0):
            it = int(ntime / in1.ntde)
            if (ntime == in1.ntde * it):
                s1.edensity_diag1(s1.sfield)
                # display smoothed electron density
                edenx = numpy.array(range(nx))
                edeny = numpy.array(s1.sfield[0:nx], copy=True)
                pc.showSimple(["EDENSITY", "Electron Density"], [edenx], [edeny], "Time=" + str(ntime * in1.dt), early=in1.ntde)
                graf1.dscaler1(s1.sfield, ' EDENSITY', ntime, 999, 0, nx, irc)
                if (irc[0] == 1):
                    break
                irc[0] = 0

                # deposit ion charge with OpenMP: updates qi
        if (in1.movion == 1):
            dtimer(dtime, itime, -1)
            s1.qi.fill(0.0)
            dtimer(dtime, itime, 1)
            s1.tdpost[0] += float(dtime)
            mpush1.mpost1(s1.pparti, s1.qi, s1.kipic, in1.qmi, s1.tdpost, in1.mx)
            # add guard cells: updates qi
            mgard1.maguard1(s1.qi, sb1.tguard, nx)

            # ion density diagnostic: updates sfield=ion density, pkwdi, wkdi
        if (in1.movion == 1):
            if (in1.ntdi > 0):
                it = int(ntime / in1.ntdi)
                if (ntime == in1.ntdi * it):
                    s1.idensity_diag1(s1.sfield, s1.pkwdi, s1.wkdi, ntime)
                    if ((in1.nddi == 1) or (in1.nddi == 3)):
                        # display smoothed ion density
                        edenx = numpy.array(range(nx))
                        pc.showSimple(["IDENSITY", "Ion Density", "Electron Density"], [edenx, edenx],
                                      [s1.sfield[0:nx], edeny], "Time=" + str(ntime * in1.dt), early=in1.ntdi)
                        graf1.dscaler1(s1.sfield, ' ION DENSITY', ntime, 999, 1, nx,
                                       irc)
                        if (irc[0] == 1):
                            break
                        irc[0] = 0
                        # ion spectral analysis
                    if ((in1.nddi == 2) or (in1.nddi == 3)):
                        # display frequency spectrum
                        pc.showSimpleImage("ION DENSITY OMEGA VS MODE+", s1.pkwdi[::, :, 0], "Time=" + str(ntime * in1.dt),
                                           extent=(0, in1.modesxdi, in1.wimin, in1.wimax), early=in1.ntdi,
                                           ticks_scale=spectrum_scale)
                        pc.showSimpleImage("ION DENSITY OMEGA VS MODE-", s1.pkwdi[::, :, 1], "Time=" + str(ntime * in1.dt),
                                           extent=(0, in1.modesxdi, in1.wimin, in1.wimax), early=in1.ntdi,
                                           ticks_scale=spectrum_scale)
                        wax = numpy.array(range(in1.modesxdi))
                        pc.showSimple(["ION DENSITY OMEGA VS MODE LINE", "+OMEGA", "-OMEGA"], [wax, wax],
                                      [s1.wkdi[0:in1.modesxdi, 0], s1.wkdi[0:in1.modesxdi, 1]],
                                      "Time=" + str(ntime * in1.dt), early=in1.ntdi)
                        graf1.dmscaler1(s1.wkdi, 'ION DENSITY OMEGA VS MODE',
                                        ntime, 999, 1, in1.modesxdi, s1.cwk, irc)
                        if (irc[0] == 1):
                            break
                        irc[0] = 0

                        # add electron and ion densities: updates qe
        mfield1.maddqei1(s1.qe, s1.qi, s1.tfield, nx)

        # add electron and ion current densities: updates cue
        if (in1.movion == 1):
            mfield1.maddcuei1(sb1.cue, sb1.cui, sb1.tfield, nx)

            # transform charge to fourier space: updates qe
        isign = -1
        mfft1.mfft1r(s1.qe, isign, s1.mixup, s1.sct, s1.tfft, in1.indx)

        # transform current to fourier space: updates cue
        isign = -1
        mfft1.mfft1rn(sb1.cue, isign, s1.mixup, s1.sct, s1.tfft, in1.indx)

        # radiative vector potential diagnostic:
        # updates vfield=radiative vector potential, vpkwr, vwkr
        if (in1.ntar > 0):
            it = ntime / in1.ntar
            if (ntime == in1.ntar * it):
                sb1.vrpotential_diag13(sb1.vfield, sb1.vpkwr, sb1.vwkr, ntime)
                if ((in1.ndar == 1) or (in1.ndar == 3)):
                    # display radiative vector potential
                    xax = numpy.array(range(len(sb1.vfield[0, :-2])))
                    # pc.showSimple(["RADVECPOTENTIAL","y","z"],[xax,xax],[sb1.vfield[0,:-2],sb1.vfield[1,:-2]],"Time="+str(ntime*in1.dt))
                    print numpy.amin(sb1.vfield)
                    graf2.dvector1(sb1.vfield, ' RADIATIVE VPOTENTIAL', ntime, 999, 0, 2, nx, irc, early=in1.ntar)
                    if (irc[0] == 1):
                        break
                    irc[0] = 0
                    # spectral analysis
                if ((in1.ndar == 2) or (in1.ndar == 3)):
                    # display frequency spectrum
                    pc.showSimpleImage("RADVECPOT Y +OMEGA", sb1.vpkwr[0, :, :, 0], "Time=" + str(ntime * in1.dt),
                                       extent=(0, in1.modesxar, in1.wrmin, in1.wrmax), early=in1.ntar,
                                       ticks_scale=spectrum_scale)
                    pc.showSimpleImage("RADVECPOT Y -OMEGA", sb1.vpkwr[0, :, :, 1], "Time=" + str(ntime * in1.dt),
                                       extent=(0, in1.modesxar, in1.wrmin, in1.wrmax), early=in1.ntar,
                                       ticks_scale=spectrum_scale)
                    pc.showSimpleImage("RADVECPOT Z +OMEGA", sb1.vpkwr[1, :, :, 0], "Time=" + str(ntime * in1.dt),
                                       extent=(0, in1.modesxar, in1.wrmin, in1.wrmax), early=in1.ntar,
                                       ticks_scale=spectrum_scale)
                    pc.showSimpleImage("RADVECPOT Z -OMEGA", sb1.vpkwr[1, :, :, 1], "Time=" + str(ntime * in1.dt),
                                       extent=(0, in1.modesxar, in1.wrmin, in1.wrmax), early=in1.ntar,
                                       ticks_scale=spectrum_scale)

                    graf1.dmvector1(sb1.vwkr,
                                    'RADIATIVE VPOTENTIAL OMEGA VS MODE', ntime,
                                    999, 2, 2, in1.modesxar, s1.cwk, irc)
                    if (irc[0] == 1):
                        break
                    irc[0] = 0

                    # calculate electromagnetic fields in fourier space: updates eyz, byz
        if ((ntime + s1.ntime0) == 0):
            # initialize electromagnetic fields from darwin fields
            # calculate initial darwin magnetic field
            mfield1.mibpois1(sb1.cue, sb1.byz, s1.ffc, in1.ci, sb1.wb, sb1.tfield,
                             nx)
            sb1.wf[0] = 0.0
            # calculate initial darwin electric field with approximate value
            sb1.init_detfield13()
            sb1.dth = 0.5 * in1.dt
        else:
            mfield1.mmaxwel1(sb1.eyz, sb1.byz, sb1.cue, s1.ffc, in1.ci, in1.dt,
                             sb1.wf, sb1.wb, sb1.tfield, nx)

            # calculate longitudinal force/charge in fourier space:
            # updates fxe, we
        mfield1.mpois1(s1.qe, s1.fxe, s1.ffc, s1.we, s1.tfield, nx)

        # add longitudinal and transverse electric fields: updates fxyze
        mfield1.memfield1(sb1.fxyze, s1.fxe, sb1.eyz, s1.ffc, sb1.tfield, nx)
        # copy magnetic field: updates byze
        mfield1.mbmfield1(sb1.byze, sb1.byz, s1.ffc, sb1.tfield, nx)

        # transform electric force to real space: updates fxyze
        isign = 1
        mfft1.mfft1rn(sb1.fxyze, isign, s1.mixup, s1.sct, s1.tfft, in1.indx)

        # transform magnetic force to real space: updates byze
        isign = 1
        mfft1.mfft1rn(sb1.byze, isign, s1.mixup, s1.sct, s1.tfft, in1.indx)

        # add constant to magnetic field with OpenMP: updates bxyze
        if (omt > 0.0):
            mfield1.mbaddext1(sb1.byze, sb1.tfield, in1.omy, in1.omz, nx)

            # add external traveling wave field
        ts = in1.dt * float(ntime)
        mfield1.meaddext13(sb1.fxyze, sb1.tfield, in1.amodex, in1.freq, ts,
                           in1.trmp, in1.toff, in1.el0, in1.er0, nx)

        # copy guard cells: updates fxyze, byze
        mgard1.mcguard1(sb1.fxyze, sb1.tguard, nx)
        mgard1.mcguard1(sb1.byze, sb1.tguard, nx)

        # potential diagnostic: updates sfield=potential, pkw, wk
        if (in1.ntp > 0):
            it = int(ntime / in1.ntp)
            if (ntime == in1.ntp * it):
                s1.potential_diag1(s1.sfield, s1.pkw, s1.wk, ntime)
                if ((in1.ndp == 1) or (in1.ndp == 3)):
                    # display potential
                    edenx = numpy.array(range(nx))
                    pc.showSimple(["DRAWPOT", "Potential"], [edenx], [s1.sfield[0:nx]],
                                  "Time=" + str(ntime * in1.dt) + " Potential", early=in1.ntp)
                    graf1.dscaler1(s1.sfield, ' POTENTIAL', ntime, 999, 0, nx, irc)
                    if (irc[0] == 1):
                        break
                    irc[0] = 0
                    # spectral analysis
                if ((in1.ndp == 2) or (in1.ndp == 3)):
                    # display frequency spectrum
                    pc.showSimpleImage("POTENTIAL OMEGA VS MODE+", s1.pkw[::, :, 0], "Time=" + str(ntime * in1.dt),
                                       extent=(0, in1.modesxp, in1.wmin, in1.wmax), early=in1.ntp,
                                       ticks_scale=spectrum_scale)
                    pc.showSimpleImage("POTENTIAL OMEGA VS MODE-", s1.pkw[::, :, 1], "Time=" + str(ntime * in1.dt),
                                       extent=(0, in1.modesxp, in1.wmin, in1.wmax), early=in1.ntp,
                                       ticks_scale=spectrum_scale)
                    wax = numpy.array(range(in1.modesxp))
                    pc.showSimple(["POTENTIAL OMEGA VS MODE LINE", "+OMEGA", "-OMEGA"], [wax, wax],
                                  [s1.wk[0:in1.modesxdi, 0], s1.wk[0:in1.modesxdi, 1]], "Time=" + str(ntime * in1.dt), early=in1.ntp)
                    graf1.dmscaler1(s1.wk, 'POTENTIAL OMEGA VS MODE', ntime, 999, 2,
                                    in1.modesxp, s1.cwk, irc)
                    if (irc[0] == 1):
                        break
                    irc[0] = 0

                    # longitudinal efield diagnostic: updates sfield=longitudinal efield
        if (in1.ntel > 0):
            it = int(ntime / in1.ntel)
            if (ntime == in1.ntel * it):
                s1.elfield_diag1(s1.sfield)
                # display longitudinal efield
                try:
                    edenx
                except:
                    edenx = numpy.array(range(nx))
                pc.showSimple(["ELFIELD", "Longitudinal E-Field"], [edenx], [s1.sfield[0:nx]],
                              "Time=" + str(ntime * in1.dt) + " Lon.  E-Field", early=in1.ntel)
                graf1.dscaler1(s1.sfield, ' ELFIELD', ntime, 999, 0, nx, irc)
                if (irc[0] == 1):
                    break
                irc[0] = 0

            # vector potential diagnostic:updates vfield=vector potential, vpkw, vwk
        if (in1.nta > 0):
            it = ntime / in1.nta
            if (ntime == in1.nta * it):
                sb1.vpotential_diag13(sb1.vfield, sb1.vpkw, sb1.vwk, ntime)
                if ((in1.nda == 1) or (in1.nda == 3)):
                    # display vector potential
                    try:
                        edenx
                    except:
                        edenx = numpy.array(range(nx))
                    # pc.showSimple(["VECPOTENTIAL","y","z"],[edenx,edenx],[sb1.vfield[0,:nx],sb1.vfield[1,:nx]],"Time="+str(ntime*in1.dt))
                    graf2.dvector1(sb1.vfield, ' VECTOR POTENTIAL', ntime, 999, 0, 2,
                                   nx, irc, early=in1.nta)
                    if (irc[0] == 1):
                        break
                    irc[0] = 0
                # spectral analysis
                if ((in1.nda == 2) or (in1.nda == 3)):
                    # display frequency spectrum
                    pc.showSimpleImage("VECTOR POTENTIAL OMEGA VS MODE Y+", sb1.vpkw[0, :, :, 0],
                                       "Time=" + str(ntime * in1.dt), extent=(0, in1.modesxa, in1.wrmin, in1.wrmax), early=in1.nta,
                                       ticks_scale=spectrum_scale)
                    pc.showSimpleImage("VECTOR POTENTIAL OMEGA VS MODE Y-", sb1.vpkw[0, :, :, 1],
                                       "Time=" + str(ntime * in1.dt), extent=(0, in1.modesxa, in1.wrmin, in1.wrmax), early=in1.nta,
                                       ticks_scale=spectrum_scale)
                    pc.showSimpleImage("VECTOR POTENTIAL OMEGA VS MODE Z+", sb1.vpkw[1, :, :, 0],
                                       "Time=" + str(ntime * in1.dt), extent=(0, in1.modesxa, in1.wrmin, in1.wrmax), early=in1.nta,
                                       ticks_scale=spectrum_scale)
                    pc.showSimpleImage("VECTOR POTENTIAL OMEGA VS MODE Z-", sb1.vpkw[1, :, :, 1],
                                       "Time=" + str(ntime * in1.dt), extent=(0, in1.modesxa, in1.wrmin, in1.wrmax), early=in1.nta,
                                       ticks_scale=spectrum_scale)

                    wax = numpy.array(range(in1.modesxa))
                    pc.showSimple(["VECTOR POTENTIAL OMEGA VS MODE Y LINE", "+OMEGA", "-OMEGA"], [wax, wax],
                                  [sb1.vwk[0, 0:in1.modesxa, 0], sb1.vwk[0, 0:in1.modesxa, 1]],
                                  "Time=" + str(ntime * in1.dt), early=in1.nta)
                    pc.showSimple(["VECTOR POTENTIAL OMEGA VS MODE Z LINE", "-OMEGA", "-OMEGA"], [wax, wax],
                                  [sb1.vwk[1, 0:in1.modesxa, 0], sb1.vwk[1, 0:in1.modesxa, 1]],
                                  "Time=" + str(ntime * in1.dt), early=in1.nta)
                    graf1.dmvector1(sb1.vwk, 'VECTOR POTENTIAL OMEGA VS MODE',
                                    ntime, 999, 2, 2, in1.modesxa, s1.cwk, irc)
                    if (irc[0] == 1):
                        break
                    irc[0] = 0

                    # transverse efield diagnostic:
                    # updates vfield=transverse efield, vpkwet, vwket
        if (in1.ntet > 0):
            it = ntime / in1.ntet
            if (ntime == in1.ntet * it):
                sb1.etfield_diag13(sb1.vfield, sb1.vpkwet, sb1.vwket, ntime)
                if ((in1.ndet == 1) or (in1.ndet == 3)):
                    # display transverse efield
                    try:
                        edenx
                    except:
                        edenx = numpy.array(range(nx))
                    # pc.showSimple(["TRANSVERSE E FIELD","Y","Z"],[edenx,edenx],[ sb1.vfield[0,0:nx], sb1.vfield[1,0:nx] ],"Time="+str(ntime*in1.dt))
                    graf2.dvector1(sb1.vfield, 'TRANSVERSE E FIELD', ntime, 999, 0,
                                   2, nx, irc, early=in1.ntet)
                    if (irc[0] == 1):
                        break
                    irc[0] = 0
                # spectral analysis
                if ((in1.ndet == 2) or (in1.ndet == 3)):
                    # display frequency spectrum
                    pc.showSimpleImage("FT TRANSVERSE E.F. Y +OMEGA VS MODE", sb1.vpkwet[0, :, :, 0],
                                       "Time=" + str(ntime * in1.dt), extent=(0, in1.modesxet, in1.wrmin, in1.wrmax), early=in1.ntet,
                                       ticks_scale=spectrum_scale)
                    pc.showSimpleImage("FT TRANSVERSE E.F. Y -OMEGA VS MODE", sb1.vpkwet[0, :, :, 1],
                                       "Time=" + str(ntime * in1.dt), extent=(0, in1.modesxet, in1.wrmin, in1.wrmax), early=in1.ntet,
                                       ticks_scale=spectrum_scale)
                    pc.showSimpleImage("FT TRANSVERSE E.F. Z +OMEGA VS MODE", sb1.vpkwet[1, :, :, 0],
                                       "Time=" + str(ntime * in1.dt), extent=(0, in1.modesxet, in1.wrmin, in1.wrmax), early=in1.ntet,
                                       ticks_scale=spectrum_scale)
                    pc.showSimpleImage("FT TRANSVERSE E.F. Z -OMEGA VS MODE", sb1.vpkwet[1, :, :, 1],
                                       "Time=" + str(ntime * in1.dt), extent=(0, in1.modesxet, in1.wrmin, in1.wrmax), early=in1.ntet,
                                       ticks_scale=spectrum_scale)

                    wax = numpy.array(range(in1.modesxet))
                    pc.showSimple(["TRANSVERSE E.F. Y OMEGA VS MODE", "+OMEGA", "-OMEGA"], [wax, wax],
                                  [sb1.vwket[0, 0:in1.modesxet, 0], sb1.vwket[0, 0:in1.modesxet, 1]],
                                  "Time=" + str(ntime * in1.dt), early=in1.ntet)
                    pc.showSimple(["TRANSVERSE E.F. Z OMEGA VS MODE", "-OMEGA", "-OMEGA"], [wax, wax],
                                  [sb1.vwket[1, 0:in1.modesxet, 0], sb1.vwket[1, 0:in1.modesxet, 1]],
                                  "Time=" + str(ntime * in1.dt), early=in1.ntet)
                    graf1.dmvector1(sb1.vwket, 'TRANSVERSE EFIELD OMEGA VS MODE',
                                    ntime, 999, 2, 2, in1.modesxet, s1.cwk, irc)
                    if (irc[0] == 1):
                        break
                    irc[0] = 0

                # magnetic field diagnostic: updates vfield=bfield
        if (in1.ntb > 0):
            it = ntime / in1.ntb
            if (ntime == in1.ntb * it):
                sb1.bfield_diag13(sb1.vfield)
                # display magnetic field
                try:
                    edenx
                except:
                    edenx = numpy.array(range(nx))
                # pc.showSimple([" MAGNETIC FIELD","Y","Z"],[edenx,edenx],[ sb1.vfield[0,0:nx], sb1.vfield[1,0:nx] ],"Time="+str(ntime*in1.dt))
                graf2.dvector1(sb1.vfield, 'BFIELD', ntime, 999, 0, 2, nx,
                               irc, early=in1.ntb)
                if (irc[0] == 1):
                    break
                irc[0] = 0

            # velocity diagnostic
        if (in1.ntv > 0):
            it = int(ntime / in1.ntv)
            if (ntime == in1.ntv * it):
                # updates ppart, kpic, fv, fvm, fvtm
                sb1.evelocity_diag13(s1.ppart, s1.kpic, s1.fv, s1.fvm, s1.fvtm)
                # display electron velocity distributions
                if ((in1.ndv == 1) or (in1.ndv == 3)):
                    pc.showVelocity(s1.fv[:, :], ["x", "y", "z"], fvm=s1.fvm, plottype="EVELOCITY", early=in1.ntv)
                    graf1.displayfv1(s1.fv, s1.fvm, ' ELECTRON', ntime, in1.nmv, 2,
                                     irc)
                    if (irc[0] == 1):
                        break
                    irc[0] = 0
                # ion distribution function
                if (in1.movion == 1):
                    # updates pparti, kipic, fvi, fvmi, fvtmi
                    sb1.ivelocity_diag13(s1.pparti, s1.kipic, s1.fvi, s1.fvmi,
                                         s1.fvtmi)
                    # display ion velocity distributions
                    if ((in1.ndv == 2) or (in1.ndv == 3)):
                        pc.showVelocity(s1.fvi[:, :], ["x", "y", "z"], fvm=s1.fvmi, plottype="IVELOCITY", early=in1.ntv)
                        graf1.displayfv1(s1.fvi, s1.fvmi, ' ION', ntime, in1.nmv, 2,
                                         irc)
                        if (irc[0] == 1):
                            break
                        irc[0] = 0

                    # trajectory diagnostic: updates ppart, kpic, partd, fvtp, fvmtp
        if (in1.ntt > 0):
            it = int(ntime / in1.ntt)
            if (ntime == in1.ntt * it):
                sb1.traj_diag13(s1.ppart, s1.kpic, s1.partd, s1.fvtp, s1.fvmtp)
                if (in1.nst == 3):
                    # display velocity distributions
                    pc.showVelocity(s1.fvtp[:, :], ["x", "y", "z"], fvm=s1.fvmtp[0], plottype="TRAJECTORY", early=in1.ntt)
                    graf1.displayfv1(s1.fvtp, s1.fvmtp, ' ELECTRON', ntime, in1.nmv,
                                     2, irc)
                    if (irc[0] == 1):
                        break
                    irc[0] = 0

                # phase space diagnostic
        if (in1.nts > 0):
            it = int(ntime / in1.nts)
            if (ntime == in1.nts * it):
                # plot electrons
                if ((in1.nds == 1) or (in1.nds == 3)):
                    # vx, vy, or vz versus x
                    nn = in1.nsxv;
                    ierr[0] = 0
                    for i in xrange(0, 3):
                        if ((nn % 2) == 1):
                            a, b, c = numpy.shape(s1.ppart)
                            phasearr = numpy.empty((2, b, c), dtype=s1.ppart.dtype)
                            phasearr[1, :, :] = s1.ppart[i + 2 - 1, :, :]
                            phasearr[0, :, :] = s1.ppart[1 - 1, :, :]
                            pc.showPhase(phasearr, s1.kpic, plottype="EPHASE" + str(i), early=in1.nts)
                            graf1.dpmgrasp1(s1.ppart, s1.kpic, ' ELECTRON', ntime,
                                            999, nx, i + 2, 1, in1.ntsc, irc)
                            if (irc[0] == 1):
                                ierr[0] = 1
                                break
                            irc[0] = 0
                        nn = int(nn / 2)
                    if (ierr[0] == 1):
                        break
                    # vx-vy, vx-vz or vy-vz
                    nn = in1.nsvv;
                    ierr[0] = 0
                    for i in xrange(0, 3):
                        if ((nn % 2) == 1):
                            a, b, c = numpy.shape(s1.ppart)
                            phasearr = numpy.empty((2, b, c), dtype=s1.ppart.dtype)
                            phasearr[1, :, :] = s1.ppart[min(i + 3, 4) - 1, :, :]
                            phasearr[0, :, :] = s1.ppart[max(i + 1, 2) - 1, :, :]
                            pc.showPhase(phasearr, s1.kpic, plottype="EPHASEV" + str(i), early=in1.nts)
                            graf1.dpmgrasp1(s1.ppart, s1.kpic, ' ELECTRON', ntime,
                                            999, nx, min(i + 3, 4), max(i + 1, 2), in1.ntsc,
                                            irc)
                            if (irc[0] == 1):
                                ierr[0] = 1
                                break
                            irc[0] = 0
                        nn = int(nn / 2)
                    if (ierr[0] == 1):
                        break
                    # ion phase space
                if (in1.movion == 1):
                    # plot ions
                    if ((in1.nds == 2) or (in1.nds == 3)):
                        # vx, vy, or vz versus x
                        nn = in1.nsxv;
                        ierr[0] = 0
                        for i in xrange(0, 3):
                            if ((nn % 2) == 1):
                                a, b, c = numpy.shape(s1.pparti)
                                phasearr = numpy.empty((2, b, c), dtype=s1.pparti.dtype)
                                phasearr[1, :, :] = s1.pparti[i + 2 - 1, :, :]
                                phasearr[0, :, :] = s1.pparti[1 - 1, :, :]
                                pc.showPhase(phasearr, s1.kipic, plottype="IPHASE" + str(i), early=in1.nts)
                                graf1.dpmgrasp1(s1.pparti, s1.kipic, ' ION', ntime,
                                                999, nx, i + 2, 1, in1.ntsc, irc)
                                if (irc[0] == 1):
                                    ierr[0] = 1
                                    break
                                irc[0] = 0
                            nn = int(nn / 2)
                        if (ierr[0] == 1):
                            break
                        # vx-vy, vx-vz or vy-vz
                        nn = in1.nsvv;
                        ierr[0] = 0
                        for i in xrange(0, 3):
                            if ((nn % 2) == 1):
                                a, b, c = numpy.shape(s1.pparti)
                                phasearr = numpy.empty((2, b, c), dtype=s1.pparti.dtype)
                                phasearr[1, :, :] = s1.pparti[min(i + 3, 4) - 1, :, :]
                                phasearr[0, :, :] = s1.pparti[max(i + 1, 2) - 1, :, :]
                                pc.showPhase(phasearr, s1.kipic, plottype="IPHASEV" + str(i), early=in1.nts)
                                graf1.dpmgrasp1(s1.pparti, s1.kipic, ' ION', ntime,
                                                999, nx, min(i + 3, 4), max(i + 1, 2),
                                                in1.ntsc, irc)
                                if (irc[0] == 1):
                                    ierr[0] = 1
                                    break
                                irc[0] = 0
                            nn = int(nn / 2)
                        if (ierr[0] == 1):
                            break

                        # push electrons with OpenMP: updates ppart, wke, kpic
        sb1.push_electrons13(s1.ppart, s1.kpic)

        # push ions with OpenMP: updates pparti, wki, kipic
        if (in1.movion == 1):
            sb1.push_ions13(s1.pparti, s1.kipic)

        # start running simulation backwards:
        # need to advance maxwell field solver one step ahead
        if (in1.treverse == 1):
            if (((ntime + 1) == (nloop / 2)) or ((ntime + 1) == nloop)):
                sb1.em_time_reverse1()

            # energy diagnostic
        if (in1.ntw > 0):
            it = int(ntime / in1.ntw)
            if (ntime == in1.ntw * it):
                pc.showEnergy(numpy.array(range(ntime)) * in1.dt, s1.wt, ntime,
                              ["Total Field", "Kinetic", "Kinetic Ions", "Total Energy", "Electric(l)", "Electric(t)",
                               "Magnetic"], early=in1.ntw)
                sb1.energy_diag13(s1.wt, ntime, iuot)

            # restart file
        if (in1.ntr > 0):
            n = ntime + 1
            it = int(n / in1.ntr)
            if (n == in1.ntr * it):
                dtimer(dtime, itime, -1)
                sb1.bwrite_restart13(s1.iur, n)
                sb1.dwrite_restart13(s1.iur)
                dtimer(dtime, itime, 1)
                s1.tfield[0] += float(dtime)

    ntime = ntime + 1

    # loop time
    dtimer(dtime, ltime, 1)
    tloop = tloop + float(dtime)

    # * * * end main iteration loop * * *

    print >> iuot
    print >> iuot, "ntime, relativity = ", ntime, ",", in1.relativity
    if (in1.treverse == 1):
        print >> iuot, "treverse = ", in1.treverse

    # print timing summaries
    sb1.print_timings13(tinit, tloop, iuot)

    if ((in1.ntw > 0) or (in1.ntt > 0)):
        graf1.reset_graphs()

    # trajectory diagnostic
    if (in1.ntt > 0):
        if ((in1.nst == 1) or (in1.nst == 2)):
            if (in1.nplot > 0):
                irc[0] = graf1.open_graphs(1)
            ts = in1.t0
            graf1.displaytr1(sb1.partd, ts, in1.dt * float(in1.ntt), sb1.itt, 2, 3,
                             irc)
            if (irc[0] == 1):
                exit(0)
            graf1.reset_nplot(in1.nplot, irc)

    # energy diagnostic
    if (in1.ntw > 0):
        ts = in1.t0
        # display energy histories
        graf1.displayw1(s1.wt, ts, in1.dt * float(in1.ntw), s1.itw, irc)
        if (irc[0] == 1):
            exit(0)
        # print energy summaries
        sb1.print_energy13(s1.wt, iuot)

    # velocity diagnostic
    if (in1.ntv > 0):
        ts = in1.t0
        graf1.displayfvt1(s1.fvtm, ' ELECTRON', ts, in1.dt * float(in1.ntv),
                          s1.itv, irc)
        if (irc[0] == 1):
            exit(0)
        # ions
        if (in1.movion == 1):
            graf1.displayfvt1(s1.fvtmi, ' ION', ts, in1.dt * float(in1.ntv),
                              s1.itv, irc)
            if (irc[0] == 1):
                exit(0)

    # display final spectral analysis for ion density
    if (in1.movion == 1):
        if (in1.ntdi > 0):
            if ((in1.nddi == 2) or (in1.nddi == 3)):
                # display frequency spectrum
                graf1.dmscaler1(s1.wkdi, 'ION DENSITY OMEGA VS MODE', ntime, 999,
                                1, in1.modesxdi, s1.cwk, irc)
                if (irc[0] == 1):
                    exit(0)

    # display final spectral analysis for potential
    if (in1.ntp > 0):
        if ((in1.ndp == 2) or (in1.ndp == 3)):
            # display frequency spectrum
            graf1.dmscaler1(s1.wk, 'POTENTIAL OMEGA VS MODE', ntime, 999, 2,
                            in1.modesxp, s1.cwk, irc)
            if (irc[0] == 1):
                exit(0)

    # display final spectral analysis for ion current density
    if (in1.movion == 1):
        if (in1.ntji > 0):
            if ((in1.ndji == 2) or (in1.ndji == 3)):
                # display frequency spectrum
                graf1.dmvector1(sb1.vwkji, 'ION CURRENT OMEGA VS MODE', ntime,
                                999, 2, 2, in1.modesxji, s1.cwk, irc)
                if (irc[0] == 1):
                    exit(0)

    # display final spectral analysis for radiative vector potential
    if (in1.ntar > 0):
        if ((in1.ndar == 2) or (in1.ndar == 3)):
            # display frequency spectrum
            graf1.dmvector1(sb1.vwkr, 'RADIATIVE VPOTENTIAL OMEGA VS MODE',
                            ntime, 999, 2, 2, in1.modesxar, s1.cwk, irc)
            if (irc[0] == 1):
                exit(0)

    # display final spectral analysis for vector potential
    if (in1.nta > 0):
        if ((in1.nda == 2) or (in1.nda == 3)):
            # display frequency spectrum
            graf1.dmvector1(sb1.vwk, 'VECTOR POTENTIAL OMEGA VS MODE', ntime,
                            999, 2, 2, in1.modesxa, s1.cwk, irc)
            if (irc[0] == 1):
                exit(0)

    # display final spectral analysis for transverse efield
    if (in1.ntet > 0):
        if ((in1.ndet == 2) or (in1.ndet == 3)):
            # display frequency spectrum
            graf1.dmvector1(sb1.vwket, 'TRANSVERSE EFIELD OMEGA VS MODE', ntime,
                            999, 2, 2, in1.modesxet, s1.cwk, irc)
            if (irc[0] == 1):
                exit(0)

    # close diagnostics
    sb1.close_diags13(s1.iudm)
    # close reset and restart files: iur, iurr, iur0
    s1.close_restart1()
    # close output file
    print >> iuot, " * * * q.e.d. * * *"
    iuot.close()
    # close graphics device
    graf1.close_graphs()

PlasmaContext.runMain(main)