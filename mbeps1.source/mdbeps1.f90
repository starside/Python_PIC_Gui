!-----------------------------------------------------------------------
! 1-2/2D Darwin OpenMP PIC code
! written by Viktor K. Decyk, UCLA
! copyright 1999-2016, regents of the university of california
      program mdbeps1
      use f1
      use fb1
      use fd1
      use graf1
      use omplib
      implicit none
!
      integer :: nn, ierr
!
! idimp = number of particle coordinates = 4
! ipbc = particle boundary condition: 1 = periodic
      idimp = 4; ipbc = 1
!
! override default input data
      emf = 2
! read namelist
      call readnml1(iuin)
! override input data
      idcode = 3
      ndim = 3
      ntar = 0
!
! start timing initialization
      call dtimer(dtime,itime,-1)
!
! create string from idrun
      write (cdrun,'(i10)') idrun
      cdrun = adjustl(cdrun)
!
! text output file
      fname = 'output1.'//cdrun
      open(unit=iuot,file=trim(fname),form='formatted',status='replace')
!
      irc = 0
! nvp = number of shared memory nodes (0=default)
!     write (*,*) 'enter number of nodes:'
!     read (5,*) nvp
! initialize for shared memory parallel processing
      call INIT_OMP(nvp)
!
! open graphics device
      irc = open_graphs(nplot)
!
! initialize scalars for standard code
! increase number of coordinates for particle tag
      if ((ntt > 0).or.((nts > 0).and.(ntsc > 0))) then
         idimp = idimp + 1
      endif
! np = total number of particles in simulation
! nx = number of grid points in x direction
      np = npx + npxb; nx = 2**indx; nxh = nx/2
      nxe = nx + 2; nxeh = nxe/2
! npi = total number of ions in simulation
      if (movion > 0) npi = npxi + npxbi
! mx1 = number of tiles in x direction
      mx1 = (nx - 1)/mx + 1
! nloop = number of time steps in simulation
! nstart = initial time loop index
! ntime = current time step
      nloop = tend/dt + .0001; nstart = 1; ntime = 0
      qbme = qme
      affp = real(nx)/real(np)
      if (movion==1) then
         qbmi = qmi/rmass
         vtxi = vtx/sqrt(rmass*rtempxi)
         vtyi = vty/sqrt(rmass*rtempyi)
         vtzi = vtz/sqrt(rmass*rtempzi)
         vtdxi = vtdx/sqrt(rmass*rtempdxi)
         vtdyi = vtdy/sqrt(rmass*rtempdyi)
         vtdzi = vtdz/sqrt(rmass*rtempdzi)
      endif
!
! check for unimplemented features
      if (ipbc.ne.1) plist = .false.
!
! allocate field data for standard code:
! qe/qi = electron/ion charge density with guard cells
! fxe = smoothed electric field with guard cells
! ffc = form factor array for poisson solver
! mixup = bit reverse table for FFT
! sct = sine/cosine table for FFT
! cue = electron current density with guard cells
! fxyze/byze = smoothed electric/magnetic field with guard cells
! dcu/dcui = electron/ion acceleration density with guard cells
! amu/amui = electron/ion momentum flux with guard cells
! cus = transverse electric field
! ffe = form factor array for iterative poisson solver
      call init_dfields13()
!
! prepare fft tables
      call mfft1_init(mixup,sct,indx)
! calculate form factor: ffc
      call mpois1_init(ffc,ax,affp,nx)
! initialize different ensemble of random numbers
      if (nextrand > 0) call mnextran1(nextrand,ndim,np+npi)
!
! open reset and restart files
      call open_restart1()
!
! new start
      if (nustrt==1) then
! initialize electrons: updates ppart, kpic
! ppart = tiled electron particle arrays
! kpic = number of electrons in each tile
         call init_electrons13()
!
! initialize background charge density: updates qi
         if (movion==0) then
            qi = 0.0
            call mpost1(ppart,qi,kpic,-qme,tdpost,mx)
            call maguard1(qi,tguard,nx)
         endif
!
! initialize ions: updates pparti, kipic, cui
! pparti = tiled on particle arrays
! kipic = number of ions in each tile
! cui = ion current density with guard cells
         if (movion==1) then
            call init_ions13()
         endif
!
! calculate shift constant for iteration: update wpm, q2m0
         call calc_shift13(iuot)
!
! initialize darwin electric field
         cus = 0.0
!
! restart to continue a run which was interrupted
      else if (nustrt==2) then
         call bread_drestart13(iur)
         nstart = ntime + 1
! start a new run with data from a previous run
      else if (nustrt==0) then
         call bread_drestart13(iur0)
      endif
!
! calculate form factor: ffe
      call mepois1_init(ffe,ax,affp,wpm,ci,nx)
!
! initialize longitudinal electric field
      fxe = 0.0
!
! set magnitude of external transverse magnetic field
      omt = sqrt(omy*omy + omz*omz)
!
! reverse simulation at end back to start
      if (treverse==1) nloop = 2*nloop
!
! initialize all diagnostics from namelist input parameters
! wt = energy time history array=
! pkw = power spectrum for potential
! pkwdi = power spectrum for ion density
! wk = maximum frequency as a function of k for potential
! wkdi = maximum frequency as a function of k for ion density
! fv/fvi = global electron/ion velocity distribution functions
! fvm/fvmi = electron/ion vdrift, vth, entropy for global distribution
! fvtm/fvtmi = time history of electron/ion vdrift, vth, and entropy
! fvtp = velocity distribution function for test particles
! fvmtp = vdrift, vth, and entropy for test particles
! partd = trajectory time history array
! vpkwji = power spectrum for ion current density
! vwkji = maximum frequency as a function of k for ion current
! vpkw = power spectrum for vector potential
! vwk = maximum frequency as a function of k for vector potential
! vpkwet = power spectrum for transverse efield
! vwket = maximum frequency as a function of k for transverse efield
! oldcue = previous current density with guard cells
      call initialize_ddiagnostics13(ntime)
!
! read in restart diagnostic file to continue interrupted run
      if (nustrt==2) call dread_drestart13(iur)
!
! write reset file
      call bwrite_drestart13(iurr,ntime)
!
! initialization time
      call dtimer(dtime,itime,1)
      tinit = tinit + real(dtime)
! start timing loop
      call dtimer(dtime,ltime,-1)
!
      write (iuot,*) 'program mdbeps1'
!
! debug reset
   10 if (irc==6) then
         irc = 0
         call bread_drestart13(iurr)
         call reset_ddiags13()
      endif
!
! * * * start main iteration loop * * *
!
      do n = nstart, nloop 
      ntime = n - 1
      write (iuot,*) 'ntime = ', ntime
!
! debug reset
!     if (ntime==nloop/2) then
!        irc = 6
!        go to 10
!     endif
!
! deposit electron current with OpenMP: updates cue
      call dtimer(dtime,itime,-1)
      cue = 0.0
      call dtimer(dtime,itime,1)
      tdjpost = tdjpost + real(dtime)
      call wmdjpost1(ppart,cue,kpic,ncl,ihole,qme,zero,ci,tdjpost,nx,mx,&
     &ipbc,relativity,.false.,irc)
! add guard cells: updates cue
      call macguard1(cue,tguard,nx)
!
! save electron current for electron current diagnostic later
      if (ndc==0) then
         if (ntje > 0) then
            it = ntime/ntje
            if (ntime==ntje*it) oldcue = cue
         endif
      endif
!
! deposit ion current with OpenMP: updates cui
      if (movion==1) then
         call dtimer(dtime,itime,-1)
         cui = 0.0
         call dtimer(dtime,itime,1)
         tdjpost = tdjpost + real(dtime)
         call wmdjpost1(pparti,cui,kipic,ncl,ihole,qmi,zero,ci,tdjpost, &
     &nx,mx,ipbc,relativity,.false.,irc)
! add guard cells: updates cui
         call macguard1(cui,tguard,nx)
      endif
!
! deposit electron charge with OpenMP: updates qe
      call dtimer(dtime,itime,-1)
      qe = 0.0
      call dtimer(dtime,itime,1)
      tdpost = tdpost + real(dtime)
      call mpost1(ppart,qe,kpic,qme,tdpost,mx)
! add guard cells: updates qe, cue
      call maguard1(qe,tguard,nx)
!
! electron density diagnostic: updates sfield
      if (ntde > 0) then
         it = ntime/ntde
         if (ntime==ntde*it) then
            call edensity_diag1(sfield)
! display smoothed electron density
            call dscaler1(sfield,' EDENSITY',ntime,999,1,nx,irc)
            if (irc==1) exit; irc = 0
         endif
      endif
!
! deposit ion charge with OpenMP: updates qi
      if (movion==1) then
         call dtimer(dtime,itime,-1)
         qi = 0.0
         call dtimer(dtime,itime,1)
         tdpost = tdpost + real(dtime)
         call mpost1(pparti,qi,kipic,qmi,tdpost,mx)
! add guard cells: updates qi
         call maguard1(qi,tguard,nx)
      endif
!
! ion density diagnostic: updates sfield, pkwdi, wkdi
      if (movion==1) then
         if (ntdi > 0) then
            it = ntime/ntdi
            if (ntime==ntdi*it) then
               call idensity_diag1(sfield,pkwdi,wkdi,ntime)
               if ((nddi==1).or.(nddi==3)) then
! display smoothed ion density
                  call dscaler1(sfield,' ION DENSITY',ntime,999,1,nx,irc&
     &)
                  if (irc==1) exit; irc = 0
               endif
! ion spectral analysis
               if ((nddi==2).or.(nddi==3)) then
! display frequency spectrum
                  call dmscaler1(wkdi,'ION DENSITY OMEGA VS MODE',ntime,&
     &999,1,modesxdi,cwk,irc)
                  if (irc==1) exit; irc = 0
               endif
            endif
         endif
      endif
!
! add electron and ion densities: updates qe
      call maddqei1(qe,qi,tfield,nx)
!
! add electron and ion current densities: updates cue
      if (movion==1) call maddcuei1(cue,cui,tfield,nx)
!
! transform charge to fourier space: updates qe
      isign = -1
      call mfft1r(qe,isign,mixup,sct,tfft,indx)
!
! calculate longitudinal force/charge in fourier space:
! updates fxe, we
      call mpois1(qe,fxe,ffc,we,tfield,nx)
!
! transform longitudinal electric force to real space: updates fxe
      isign = 1
      call mfft1r(fxe,isign,mixup,sct,tfft,indx)
!
! copy guard cells: updates fxe
      call mdguard1(fxe,tguard,nx)
!
! initialize electron deposit data
      call dtimer(dtime,itime,-1)
      dcu = 0.0; amu = 0.0
      call dtimer(dtime,itime,1)
      tdcjpost = tdcjpost + real(dtime)
! initialize ion deposit data
      if (movion==1) then
         call dtimer(dtime,itime,-1)
         dcui = 0.0; amui = 0.0
         call dtimer(dtime,itime,1)
         tdcjpost = tdcjpost + real(dtime)
      endif
! predictor for darwin iteration: updates: cue, cus, byze, fxyze
      call darwin_predictor13(q2m0)
!
! darwin iteration loop
      do k = 1, ndc
!
! initialize electron deposit data
      call dtimer(dtime,itime,-1)
      cue = 0.0; dcu = 0.0; amu = 0.0
      call dtimer(dtime,itime,1)
      tdcjpost = tdcjpost + real(dtime)
! initialize ion deposit data
      if (movion==1) then
         call dtimer(dtime,itime,-1)
         cui = 0.0; dcui = 0.0; amui = 0.0
         call dtimer(dtime,itime,1)
         tdcjpost = tdcjpost + real(dtime)
      endif
! updates: cue, dcu, cus, byze, fxyze
      call darwin_iteration13(q2m0)
!
      enddo
!
! add external traveling wave field
      ts = dt*real(ntime)
      call meaddext13(fxyze,tfield,amodex,freq,ts,trmp,toff,el0,er0,nx)
!
! copy guard cells: updates fxyze
      call mcguard1(fxyze,tguard,nx)
!
! darwin electron current density diagnostic: updates vfield
      if (ntje > 0) then
         it = ntime/ntje
         if (ntime==ntje*it) then
            call edcurrent_diag13(vfield)
! display smoothed electron current
            call dvector1(vfield,' ELECTRON CURRENT',ntime,999,0,2,nx,  &
     &irc)
            if (irc==1) exit; irc = 0
         endif
      endif
!
! ion current density diagnostic: updates vfield, vpkwji, vwkji
      if (movion==1) then
         if (ntji > 0) then
            it = ntime/ntji
            if (ntime==ntji*it) then
               call icurrent_diag13(vfield,vpkwji,vwkji,ntime)
               if ((ndji==1).or.(ndji==3)) then
! display smoothed ion current
                  call dvector1(vfield,' ION CURRENT',ntime,999,0,2,nx, &
     &irc)
                  if (irc==1) exit; irc = 0
               endif
! ion spectral analysis
               if ((ndji==2).or.(ndji==3)) then
! display frequency spectrum
                  call dmvector1(vwkji,'ION CURRENT OMEGA VS MODE',ntime&
     &,999,2,2,modesxji,cwk,irc)
                  if (irc==1) exit; irc = 0
               endif
            endif
         endif
      endif
!
! potential diagnostic: updates sfield, pkw, wk
      if (ntp > 0) then
         it = ntime/ntp
         if (ntime==ntp*it) then
            call potential_diag1(sfield,pkw,wk,ntime)
            if ((ndp==1).or.(ndp==3)) then
! display potential
               call dscaler1(sfield,' POTENTIAL',ntime,999,0,nx,irc)
               if (irc==1) exit; irc = 0
            endif
! spectral analysis
            if ((ndp==2).or.(ndp==3)) then
! display frequency spectrum
               call dmscaler1(wk,'POTENTIAL OMEGA VS MODE',ntime,999,2, &
     &modesxp,cwk,irc)
               if (irc==1) exit; irc = 0
            endif
         endif
      endif
!
! longitudinal efield diagnostic: updates sfield
      if (ntel > 0) then
         it = ntime/ntel
         if (ntime==ntel*it) then
            call elfield_diag1(sfield)
! display longitudinal efield 
            call dscaler1(sfield,' ELFIELD',ntime,999,0,nx,irc)
            if (irc==1) exit; irc = 0
         endif
      endif
!
! vector potential diagnostic: updates vfield, vpkw, vwk
      if (nta > 0) then
         it = ntime/nta
         if (ntime==nta*it) then
            call vdpotential_diag13(vfield,vpkw,vwk,ntime)
            if ((nda==1).or.(nda==3)) then
! display vector potential
               call dvector1(vfield,' VECTOR POTENTIAL',ntime,999,0,2,nx&
     &,irc)
               if (irc==1) exit; irc = 0
            endif
! spectral analysis
            if ((nda==2).or.(nda==3)) then
! display frequency spectrum
               call dmvector1(vwk,'VECTOR POTENTIAL OMEGA VS MODE',ntime&
     &,999,2,2,modesxa,cwk,irc)
               if (irc==1) exit; irc = 0
            endif
         endif
      endif
!
! transverse efield diagnostic: updates vfield, vpkwet, vwket
      if (ntet > 0) then
         it = ntime/ntet
         if (ntime==ntet*it) then
            call detfield_diag13(vfield,vpkwet,vwket,ntime)
            if ((ndet==1).or.(ndet==3)) then
! display transverse efield
               call dvector1(vfield,' TRANSVERSE EFIELD',ntime,999,0,2, &
     &nx,irc)
               if (irc==1) exit; irc = 0
            endif
! spectral analysis
            if ((ndet==2).or.(ndet==3)) then
! display frequency spectrum-
               call dmvector1(vwket,'TRANSVERSE EFIELD OMEGA VS MODE',  &
     &ntime,999,2,2,modesxet,cwk,irc)
               if (irc==1) exit; irc = 0
            endif
         endif
      endif
!
! magnetic field diagnostic: updates vfield
      if (ntb > 0) then
         it = ntime/ntb
         if (ntime==ntb*it) then
            call dbfield_diag13(vfield)
! display magnetic field
            call dvector1(vfield,' MAGNETIC FIELD',ntime,999,0,2,nx,irc)
            if (irc==1) exit; irc = 0
         endif
      endif
!
! fluid moments diagnostic
      if (ntfm > 0) then
         it = ntime/ntfm
         if (ntime==ntfm*it) then
! updates fmse
            call edfluidms_diag13(fmse)
            if (movion==1) then
! updates fmsi
               call idfluidms_diag13(fmsi)
            endif
         endif
      endif
!
! velocity diagnostic
      if (ntv > 0) then
         it = ntime/ntv
         if (ntime==ntv*it) then
! updates ppart, kpic, fv, fvm, fvtm
            call evelocity_diag13(ppart,kpic,fv,fvm,fvtm)
! display electron velocity distributions
            call displayfv1(fv,fvm,' ELECTRON',ntime,nmv,2,irc)
            if (irc==1) exit; irc = 0
! ion distribution function
            if (movion==1) then
! updates pparti, kipic, fvi, fvmi, fvtmi
               call ivelocity_diag13(pparti,kipic,fvi,fvmi,fvtmi)
! display ion velocity distributions
               call displayfv1(fvi,fvmi,' ION',ntime,nmv,2,irc)
               if (irc==1) exit; irc = 0
            endif
         endif
      endif
!
! trajectory diagnostic: updates ppart, kpic, partd, fvtp, fvmtp
      if (ntt > 0) then
         it = ntime/ntt
         if (ntime==ntt*it) then
            call traj_diag13(ppart,kpic,partd,fvtp,fvmtp)
            if (nst==3) then
! display test particle velocity distributions
               call displayfv1(fvtp,fvmtp,' ELECTRON',ntime,nmv,2,irc)
               if (irc==1) exit; irc = 0
            endif
         endif
      endif
!
! phase space diagnostic
      if (nts > 0) then
         it = ntime/nts
         if (ntime==nts*it) then
! plot electrons
            if ((nds==1).or.(nds==3)) then
! vx, vy, or vz versus x
               nn = nsxv; ierr = 0
               do i = 1, 3
                  if (mod(nn,2)==1) then
                     call dpmgrasp1(ppart,kpic,' ELECTRON',ntime,999,nx,&
     &i+1,1,ntsc,irc)
                     if (irc==1) then
                        ierr = 1
                        exit
                     endif
                     irc = 0
                  endif
                  nn = nn/2
               enddo
               if (ierr==1) exit
! vx-vy, vx-vz or vy-vz
               nn = nsvv; ierr = 0
               do i = 1, 3
                  if (mod(nn,2)==1) then
                     call dpmgrasp1(ppart,kpic,' ELECTRON',ntime,999,nx,&
     &min(i+2,4),max(i,2),ntsc,irc)
                     if (irc==1) then
                        ierr = 1
                        exit
                     endif
                     irc = 0
                  endif
                  nn = nn/2
               enddo
               if (ierr==1) exit
            endif
! ion phase space
            if (movion==1) then
! plot ions
               if ((nds==2).or.(nds==3)) then
! vx, vy, or vz versus x
                  nn = nsxv; ierr = 0
                  do i = 1, 3
                     if (mod(nn,2)==1) then
                        call dpmgrasp1(pparti,kipic,' ION',ntime,999,nx,&
     &i+1,1,ntsc,irc)

                        if (irc==1) then
                           ierr = 1
                           exit
                        endif
                        irc = 0
                     endif
                     nn = nn/2
                  enddo
                  if (ierr==1) exit
! vx-vy, vx-vz or vy-vz
                  nn = nsvv; ierr = 0
                  do i = 1, 3
                     if (mod(nn,2)==1) then
                        call dpmgrasp1(pparti,kipic,' ION',ntime,999,nx,&
     &min(i+2,4),max(i,2),ntsc,irc)
                        if (irc==1) then
                           ierr = 1
                           exit
                        endif
                        irc = 0
                     endif
                     nn = nn/2
                  enddo
                  if (ierr==1) exit
               endif
            endif
         endif
      endif
!
! push electrons with OpenMP: updates ppart, wke, kpic
      call dpush_electrons13(ppart,kpic)
!
! push ions with OpenMP:: updates pparti, wki, kipic
      if (movion==1) then
         call dpush_ions13(pparti,kipic)
      endif
!
! start running simulation backwards:
! need to reverse time lag in leap-frog integration scheme
      if (treverse==1) then
         if (((ntime+1)==(nloop/2)).or.((ntime+1)==nloop)) then
            call d_time_reverse1()
         endif
      endif
!
! energy diagnostic
      if (ntw > 0) then
         it = ntime/ntw
         if (ntime==ntw*it) then
            call denergy_diag13(wt,ntime,iuot)
         endif
      endif
!
! restart file
      if (ntr > 0) then
         it = n/ntr
         if (n==ntr*it) then
            call dtimer(dtime,itime,-1)
            call bwrite_drestart13(iur,n)
            call dwrite_drestart13(iur)
            call dtimer(dtime,itime,1)
            tfield = tfield + real(dtime)
         endif
      endif
!
      enddo
      ntime = ntime + 1
!
! loop time
      call dtimer(dtime,ltime,1)
      tloop = tloop + real(dtime)
!
! * * * end main iteration loop * * *
!
      write (iuot,*)
      write (iuot,*) 'ntime, relativity, ndc = ', ntime, relativity, ndc
      if (treverse==1) write (iuot,*) 'treverse = ', treverse
!
! print timing summaries
      call print_dtimings13(tinit,tloop,iuot)
!
      if ((ntw > 0).or.(ntt > 0)) then
         if (nplot > 0) call reset_graphs
      endif
!
! trajectory diagnostic
      if (ntt > 0) then
         if ((nst==1).or.(nst==2)) then
            if (nplot > 0) irc = open_graphs(1)
            call displaytr1(partd,t0,dt*real(ntt),itt,2,3,irc)
            if (irc==1) stop
            call reset_nplot(nplot,irc)
         endif
      endif
!
! energy diagnostic
      if (ntw > 0) then
! display energy histories
         call displayw1(wt,t0,dt*real(ntw),itw,irc)
! print energy summaries
         call print_energy13(wt,iuot)
      endif
!
! velocity diagnostic
      if (ntv > 0) then
         call displayfvt1(fvtm,' ELECT',t0,dt*real(ntv),itv,irc)
         if (irc==1) stop
! ions
         if (movion==1) then
            call displayfvt1(fvtmi,' ION',t0,dt*real(ntv),itv,irc)
            if (irc==1) stop
         endif
      endif
!
! display final spectral analysis for ion density
      if (movion==1) then
         if (ntdi > 0) then
            if ((nddi==2).or.(nddi==3)) then
! display frequency spectrum
               call dmscaler1(wkdi,'ION DENSITY OMEGA VS MODE',ntime,999&
     &,1,modesxdi,cwk,irc)
               if (irc==1) stop
            endif
         endif
      endif
!
! display final spectral analysis for potential
      if (ntp > 0) then
         if ((ndp==2).or.(ndp==3)) then
! display frequency spectrum
            call dmscaler1(wk,'POTENTIAL OMEGA VS MODE',ntime,999,2,    &
     &modesxp,cwk,irc)
            if (irc==1) stop
         endif
      endif
!
! display final spectral analysis for ion current density
      if (movion==1) then
         if (ntji > 0) then
            if ((ndji==2).or.(ndji==3)) then
! display frequency spectrum
               call dmvector1(vwkji,'ION CURRENT OMEGA VS MODE',ntime,  &
     &999,2,2,modesxji,cwk,irc)
               if (irc==1) stop
            endif
         endif
      endif
!
! display final spectral analysis for vector potential
      if (nta > 0) then
         if ((nda==2).or.(nda==3)) then
! display frequency spectrum
            call dmvector1(vwk,'VECTOR POTENTIAL OMEGA VS MODE',ntime,  &
     &999,2,2,modesxa,cwk,irc)
            if (irc==1) stop
         endif
      endif
!
! display final spectral analysis for transverse efield
      if (ntet > 0) then
         if ((ndet==2).or.(ndet==3)) then
! display frequency spectrum
            call dmvector1(vwket,'TRANSVERSE EFIELD OMEGA VS MODE',ntime&
     &,999,2,2,modesxet,cwk,irc)
            if (irc==1) stop
         endif
      endif
!
! close darwin diagnostics
      call close_ddiags13(iudm)
! close reset and restart files
      call close_restart1()
! close output file
      write (iuot,*) ' * * * q.e.d. * * *'
      close(unit=iuot)
! close graphics device
      call close_graphs
!
      stop
      end program
