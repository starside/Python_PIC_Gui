class RunSimulation:
    def __init__(self):
        loadNamelist(in1, 'input1')  # Read into a fortran module
        loadNamelist(C, 'declares')  # read in to python
        # Line 58
        diag.copyint(1, in1.idcode)
        diag.copyint(1, in1.psolve)
        diag.copyint(1, in1.ndim)
        # line 63
        self.cdrun = int(in1.idrun)
        self.dfname = open("output1." + str(int(self.cdrun)), 'w')
        # Line 70
        # np = total number of electrons in simulation.
        C.np[0] = in1.npx + in1.npxb
        C.npx1[0] = in1.npx + 1
        C.nx[0] = 2 ** in1.indx
        C.nxh[0] = C.nx[0] / 2
        C.nxe[0] = C.nx[0] + 4
        if in1.inorder == gl.linear:
            C.nxe[0] = C.nx[0] + 2
        elif in1.inorder == gl.cubic:
            C.nxe[0] = C.nx[0] + 6
        # dimension for index and sorting arrays
        C.nx1[0] = C.nx[0] + 1
        # nloop = number of time steps in simulation
        C.nloop[0] = in1.tend / in1.dt + .0001
        # part(1,n) = position x of particle n
        # part(2,n) = velocity vx of particle n
        self.part = NP.empty((C.idimp, C.np), dtype=fType('real'), order='F')
        self.qe = NP.empty(C.nxe, dtype=fType('real'))
        self.fxe = NP.empty(C.nxe, dtype=fType('real'))
        # ffc = form factor array for poisson solver
        self.ffc = NP.empty(C.nxh, dtype=fType('complex'))
        # mixup = array of bit reversed addresses for fft
        # sct = sine/cosine table for fft
        self.mixup = NP.empty(C.nxh, dtype=fType('int'))
        self.sct = NP.empty(C.nxh, dtype=fType('complex'))
        # open graphics device
        C.irc[0] = diag.open_graphs(in1.nplot)
        # initialize timer
        self.tt = diag.wtimer(C.ltime, -1)
        C.time[0] = self.tt
        # initialize constants
        C.itime[0] = C.itime0[0]
        C.ntime[0] = C.itime[0] + C.itime0[0]
        C.qbme[0] = in1.qme
        C.affp[0] = 1.0 * C.nx[0] / (1.0 * C.np[0])
        # set initial time
        diag.copyint(in1.dt * C.itime0, in1.t0)
        # set default diagnostic file names
        if in1.ntp > 0:
            self.fpname = 'potk1.'  # //cdrun
        # energy time history
        if (in1.ntw > 0):
            self.wt = NP.empty(((C.nloop - 1) / in1.ntw - (C.itime0 / in1.ntw) + 1, 4), dtype=fType('real'), order='F')
            C.itw[0] = 0
            # prepare fft tables
        pyfft1mod.fft1d.ifft1rxinit(self.mixup, self.sct, in1.indx)
        # calculate form factors
        fld.ipois1init(self.ffc, in1.ax, C.affp, C.nx)
        # initialize density profile and velocity distribution
        # background electrons
        if in1.npx > 0:
            distr(self.part, rightType(1), in1.npx, in1.vtx, in1.vx0, in1.npx, C.nx, C.ipbc)
        # beam electrons
        if in1.npxb > 0:
            distr(self.part, C.npx1, in1.npxb, in1.vtdx, in1.vdx, in1.npxb, C.nx, C.ipbc)
        # initialize charge density to background
        C.qi0 = -in1.qme / C.affp
        # ! calculate initial electron momentum
        if (in1.ntm > 0):
            psm.initmomt1(self.part, C.np, C.pxe, C.pye, C.pze, in1.ndim)
        # sorting arrays
        if (in1.sortime > 0):
            self.pt = NP.empty(C.np, dtype=fType('real'))
            self.ip = NP.empty(C.np, dtype=fType('int'))
            self.npic = NP.empty(C.nx1, dtype=fType('int'))
        self.tres = diag.get_funit(C.iudm)
        diag.copyint(self.tres, C.iudm)  # Not doing it this way causes some weird python error
        # velocity diagnostic
        if in1.ntv > 0:
            self.fv = NP.empty((2 * C.nmv + 2, 1), dtype=fType('real'), order='F')
            self.fvm = NP.empty((3, 1), dtype=fType('real'), order='F')
            self.fv[0, :] = 8.0 * in1.vtx
        # potential diagnostic
        if (in1.ntp > 0):
            self.sfield = NP.empty(C.nxe, dtype=fType('real'))
            if (in1.modesxp > C.nxh):
                diag.copyint(C.nxh, in1.modesxp)
            self.pott = NP.empty(in1.modesxp, dtype=fType('complex'))
            # 162: Skipped a bunch of stuff record time
        self.tt = diag.wtimer(C.ltime, 1)
        C.time[0] = self.tt
        # write (iuot,*) 'initialization wall clock time = ', time, 'sec'
        self.dfname.write('initialization wall clock time = ' + str(C.time[0]) + "sec\n")

        while C.nloop >= C.ntime:
            # ! initialize charge density to background
            fld.isguard1(self.qe, C.qi0, C.nx, in1.inorder)
            # ! deposit electron charge
            psm.igpost1(self.part, self.qe, C.np, in1.qme, C.tdpost, in1.inorder, in1.dopt)
            # ! add guard cells
            fld.iaguard1(self.qe, C.nx, in1.inorder)
            # ! velocity diagnostic
            if (in1.ntv > 0):
                C.it[0] = C.ntime / in1.ntv
                if (C.ntime == in1.ntv * C.it):
                    # ! calculate particle distribution function and moments
                    diag.ivdist1(self.part, self.fv, self.fvm, C.np, C.nmv)
                    # ! display velocity distributions
                    diag.idisplayfv1(self.fv, self.fvm, ' ELECTRON', C.ntime, C.nmv, 2, C.irc)
                    # plt.plot(fv)
                    # plt.show()
                    if (C.irc == 1):
                        break
                        # ! phase space diagnostic
            if (in1.nts > 0):
                C.it[0] = C.ntime / in1.nts
                if (C.ntime == in1.nts * C.it):
                    # ! plot particles vx versus x
                    diag.igrasp13(self.part, C.np, ' ELECTRON', C.ntime, 999, C.nx, 2, 1, in1.npx, C.irc)
                    if (C.irc == 1):
                        break
                        # ! transform charge to fourier space
            C.isign[0] = -1
            C.ws[0] = 0.0
            fft(self.qe, C.isign, self.mixup, self.sct, C.tfft, in1.indx, in1.inorder)
            # ! potential diagnostic
            if (in1.ntp > 0):
                C.it[0] = C.ntime / in1.ntp
                if (C.ntime == in1.ntp * C.it):
                    # ! calculate potential in fourier space
                    C.isign[0] = 1
                    fld.ipois1(self.qe, self.sfield, C.isign, self.ffc, C.ws, C.nx, in1.inorder)
                    # ! store selected fourier modes
                    gtmodes(self.sfield, self.pott, C.nx, in1.modesxp, in1.inorder)
                    # ! write diagnostic output
                    # writebf(pott,modesxp,iup,nprec,order=LINEAR)
                    # ! transform potential to real space
                    fft(self.sfield, C.isign, self.mixup, self.sct, C.tfft, in1.indx, in1.inorder)
                    cguard(self.sfield, C.nx, in1.inorder)
                    # ! display potential
                    diag.idscaler1(self.sfield, ' POTENTIAL', C.ntime, 999, 0, C.nx, C.irc, in1.inorder)
                    # plt.plot(sfield)
                    # plt.show()
                    if (C.irc[0] == 1):
                        break
                        # ! calculate force/charge in fourier space
            C.isign[0] = -1
            fld.ipois1(self.qe, self.fxe, C.isign, self.ffc, C.we, C.nx, in1.inorder)
            # ! transform force/charge to real space
            C.isign[0] = 1
            fft(self.fxe, C.isign, self.mixup, self.sct, C.tfft, in1.indx, in1.inorder)
            cguard(self.fxe, C.nx, in1.inorder)
            # ! push particles
            C.wke[0] = 0.0
            # modifies part, wke
            psm.igpush1(self.part, self.fxe, C.np, C.qbme, in1.dt, C.wke, C.tpush, C.nx, C.ipbc, in1.inorder, in1.popt)
            # ! momentum diagnostic
            if (in1.ntm > 0):  # ntm is zero.  Not fixing this code
                C.it[0] = C.ntime / in1.ntm
                # ! calculate and print electron momentum
                C.it[0] = C.ntime - in1.ntm * C.it + 1
                if (C.it[0] > 1):
                    C.it[0] = C.it[0] - in1.ntm
                if (C.it[0] >= 0):
                    premoment1(self.part, C.ntime, C.np, C.ium, C.pxe, C.pye, C.pze, C.zero, C.zero, C.zero \
                               , self.wx, self.wy, self.wz, in1.ndim, nprint=it)
            # sort electrons
            if (in1.sortime > 0):
                if (C.ntime[0] % in1.sortime == 0):
                    psm.isortp1x(self.part, self.pt, self.ip, C.np, npic, C.tsort, in1.inorder)
            C.itime[0] = C.itime + 1
            C.ntime[0] = C.itime + C.itime0
            self.tloop = diag.wtimer(C.ltime, 1)
            C.time[0] = C.time + self.tloop
