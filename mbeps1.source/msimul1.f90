!-----------------------------------------------------------------------
! High Level library for 1D Electrostatic OpenMP PIC code
! init_fields1: allocate field data for standard code
! del_fields1: delete field data for standard code
! init_electrons1: initialize electrons
! reorder_electrons1: recover from electron buffer overflow errors
! push_electrons1: push electrons with OpenMP:
! del_electrons1: delete electrons
! init_ions1: initialize ions
! reorder_ions1: recover from ion buffer overflow errors
! push_ions1: push ions with OpenMP:
! del_ions1: delete ions
! es_time_reverse1: start running simulation backwards
! init_energy_diag1: initialize energy diagnostic
! energy_diag1: energy diagnostic
! print_energy1: print energy summaries
! del_energy_diag1: delete energy diagnostic
! init_spectrum1: allocate scratch arrays for scalar fields
! del_spectrum1: delete scratch arrays for scalar fields
! init_edensity_diag1: initialize electron density diagnostic
! edensity_diag1: electron density diagnostic
! del_edensity_diag1: delete electron density diagnostic data
! init_idensity_diag1: initialize ion density diagnostic
! idensity_diag1: ion density diagnostic
! del_idensity_diag1: delete ion density diagnostic
! init_potential_diag1: initialize potential diagnostic
! potential_diag1: potential diagnostic
! del_potential_diag1: delete potential diagnostic
! init_elfield_diag1: initialize longitudinal efield diagnostic
! elfield_diag1: longitudinal efield diagnostic
! del_elfield_diag1: delete longitudinal efield diagnostic
! init_evelocity_diag1: initialize electron velocity diagnostic
! evelocity_diag1: electron velocity diagnostic
! del_evelocity_diag1: delete electron velocity diagnostic
! init_ivelocity_diag1: initialize ion velocity diagnostic
! ivelocity_diag1: ion velocity diagnostic
! del_ivelocity_diag1: delete ion velocity diagnostic
! init_traj_diag1: initialize trajectory diagnostic
! traj_diag1: trajectory diagnostic
! del_traj_diag1: delete trajectory diagnostic
! print_timings1: print timing summaries
! close_diags1: close diagnostics
! initialize_diagnostics1: initialize all diagnostics from namelist
!                          input parameters
! open_restart1: open reset and restart files
! bwrite_restart1: write out basic restart file for electrostatic code
! bread_restart1: read in basic restart file for electrostatic code
! dwrite_restart1: write out restart diagnostic file for electrostatic
!                  code
! dread_restart1: read in restart diagnostic file for electrostatic code
! close_restart1: close reset and restart files
! reset_diags1: reset electrostatic diagnostics
! written by Viktor K. Decyk, UCLA
! copyright 1999-2016, regents of the university of california
! update: december 20, 2016
      module f1
      use in1
      use minit1
      use mpush1
      use msort1
      use mgard1
      use mfft1
      use mfield1
      use mdiag1
      implicit none
! idimp = number of particle coordinates = 2
! ipbc = particle boundary condition: 1 = periodic
      integer :: idimp = 2, ipbc = 1
! wke/wki/we = electron/ion kinetic energies and electric field energy
      real :: wke = 0.0, wki = 0.0, we = 0.0
! plist = (true,false) = list of particles leaving tiles found in push
      logical :: plist = .true.
!
! declare scalars for standard code
      integer :: n
      integer :: np, nx, nxh, nxe, nxeh
      integer :: mx1, nstart, nloop, ntime, isign
      integer :: ntime0 = 0, npi = 0
      real :: qbme, affp, ws
      real :: qbmi, vtxi, vtdxi
!
! declare scalars for OpenMP code
      integer :: nppmx, nppmx0, nppmx1, ntmax, npbmx
      integer :: irc = 0
      integer, dimension(2) :: irc2 = 0
!
! declare scalars for diagnostics
      integer :: it, iw, mtw, mtp, mtv, mtt
      integer :: itw = 0, itp = 0, itv = 0, itt = 0
      integer :: iwi, mtdi
      integer :: itdi = 0
! default Fortran unit numbers
      integer :: iuin = 8, iurr = 9, iuot = 18, iudm = 19
      integer :: iur = 17, iur0 = 27
      integer :: iude = 10, iup = 11, iuel = 12
      integer :: iudi = 20
      real :: ts
      character(len=10) :: cdrun
      character(len=32) :: fname
!
! declare and initialize timing data
      integer, dimension(4) :: itime, ltime
      real :: tinit = 0.0, tloop = 0.0
      real :: tdpost = 0.0, tguard = 0.0, tfft = 0.0, tfield = 0.0
      real :: tpush = 0.0, tsort = 0.0, tdiag = 0.0
      double precision :: dtime
!
! declare arrays for standard code:
! part = particle array
      real, dimension(:,:), allocatable :: part
! qe/qi = electron/ion charge density with guard cells
      real, dimension(:), allocatable :: qe, qi
! fxe = smoothed electric field with guard cells
      real, dimension(:), allocatable :: fxe
! ffc = form factor array for poisson solver
      complex, dimension(:), allocatable :: ffc
! mixup = bit reverse table for FFT
      integer, dimension(:), allocatable :: mixup
! sct = sine/cosine table for FFT
      complex, dimension(:), allocatable :: sct
!
! declare arrays for OpenMP (tiled) code:
! ppbuff = buffer array for reordering tiled particle array
      real, dimension(:,:,:), allocatable :: ppbuff
! ncl = number of particles departing tile in each direction
      integer, dimension(:,:), allocatable :: ncl
! ihole = location/destination of each particle departing tile
      integer, dimension(:,:,:), allocatable :: ihole
!
! ppart/pparti = tiled electron/ion particle arrays
      real, dimension(:,:,:), allocatable :: ppart, pparti
! kpic/kipic = number of electrons/ions in each tile
      integer, dimension(:), allocatable :: kpic, kipic
!
! public diagnostic arrays
! wt = energy time history array
      real, dimension(:,:), allocatable :: wt
! sfield = scratch array for scalar field
      real, dimension(:), allocatable :: sfield
! pkw = power spectrum for potential
      real, dimension(:,:,:), allocatable :: pkw
! pkwdi = power spectrum for ion density
      real, dimension(:,:,:), allocatable :: pkwdi
! wk = maximum frequency as a function of k for potential
      real, dimension(:,:), allocatable :: wk
! wkdi = maximum frequency as a function of k for ion density
      real, dimension(:,:), allocatable :: wkdi
! fv/fvi = global electron/ion velocity distribution functions
      real, dimension(:,:), allocatable :: fv, fvi
! fvm/fvmi = electron/ion vdrift, vth, entropy for global distribution
      real, dimension(:,:), allocatable :: fvm, fvmi
! fvtm/fvtmi = time history of electron/ion vdrift, vth, and entropy
      real, dimension(:,:,:), allocatable :: fvtm, fvtmi
! fvtp = velocity distribution function for test particles
! fvmtp = vdrift, vth, and entropy for test particles
      real, dimension(:,:), allocatable :: fvtp, fvmtp
! partd = trajectory time history array
      real, dimension(:,:,:), allocatable :: partd
! scratch arrays for spectral analysis
      real, dimension(:), allocatable :: wm, wmi
! cwk = labels for power spectrum display
      character(len=10), dimension(2) :: cwk = (/'   W > 0  ',          &
     &                                           '   W < 0  ' /)
!
! private diagnostic arrays
! s = scratch array for energies
      double precision, dimension(:), allocatable :: s
! scratch array for scalar field
      complex, dimension(:), allocatable :: sfieldc
! denet/denit = store selected fourier modes for electron/ion density
      complex, dimension(:), allocatable :: denet, denit
! pksdi = accumulated complex spectrum for ion density
      double precision, dimension(:,:,:), allocatable :: pksdi
! pott = store selected fourier modes for potential
      complex, dimension(:), allocatable :: pott
! pks = accumulated complex spectrum for potential
      double precision, dimension(:,:,:), allocatable :: pks
! elt = store selected fourier modes for longitudinal efield
      complex, dimension(:), allocatable :: elt
! sfv/sfvi = electron/ion velocity distribution functions in tile
      real, dimension(:,:,:), allocatable :: sfv, sfvi
! iprobt = scratch array 
      integer, dimension(:), allocatable :: iprobt
! partt = particle trajectories tracked
      real, dimension(:,:), allocatable :: partt
!
      save
!
      public :: qe, qi, fxe, ffc, mixup, sct
      public :: part, ppbuff, ncl, ihole
      public :: ppart, pparti, kpic, kipic
      public :: wt, sfield, pkw, pkwdi, wk, wkdi
      public :: fv, fvi, fvm, fvmi, fvtm, fvtmi, fvtp, fvmtp, partd
      public :: wm, wmi, cwk
      private :: s, sfieldc
      private :: denet, denit, pksdi, pott, pks, elt
      private :: sfv, sfvi, iprobt, partt
!
      contains
!
!-----------------------------------------------------------------------
      subroutine init_fields1()
! allocate field data for standard code
      implicit none
      allocate(qe(nxe),qi(nxe),fxe(nxe))
      allocate(ffc(nxh),mixup(nxh),sct(nxh))
      end subroutine
!
!-----------------------------------------------------------------------
      subroutine del_fields1()
! delete field data for standard code
      implicit none
      deallocate(qe,qi,fxe,ffc,mixup,sct)
      end subroutine
!
!-----------------------------------------------------------------------
      subroutine init_electrons1()
! initialize electrons
      implicit none
! part = particle array
      allocate(part(idimp,max(np,npi)))
! background electrons
      if (npx > 0) then
! calculates initial electron co-ordinates with various density profiles
         call mfdistr1(part,ampdx,scaledx,shiftdx,1,npx,nx,ipbc,ndprof)
! initialize electron velocities
         call wmvdistr1(part,1,vtx,vx0,npx,nvdist)
      endif
! beam electrons
      if (npxb > 0) then
         it = npx + 1
! calculates initial electron co-ordinates with various density profiles
         call mfdistr1(part,ampdx,scaledx,shiftdx,it,npxb,nx,ipbc,ndprof&
     &)
! initialize electron velocities
         call wmvdistr1(part,it,vtdx,vdx,npxb,nvdist)
      endif
!
! mark electron beam particles
      if ((nts > 0).and.(ntsc > 0)) then
         call setmbeam1(part,npx)
      endif
!
! kpic = number of electrons in each tile
      allocate(kpic(mx1))
!
! find number of electrons in each of mx, tiles: updates kpic, nppmx
      call mdblkp2(part,kpic,nppmx,np,mx,irc)
!
! allocate vector electron data
      nppmx0 = (1.0 + xtras)*nppmx
      ntmax = xtras*nppmx
      npbmx = xtras*nppmx
      allocate(ppart(idimp,nppmx0,mx1))
      allocate(ppbuff(idimp,npbmx,mx1))
      allocate(ncl(2,mx1))
      allocate(ihole(2,ntmax+1,mx1))
! copy ordered electron data for OpenMP: updates ppart and kpic
      call mpmovin1(part,ppart,kpic,mx,irc)
!
! sanity check for electrons
      call mcheck1(ppart,kpic,nx,mx,irc)
      if (irc /= 0) stop
      end subroutine
!
!-----------------------------------------------------------------------
      subroutine reorder_electrons1(irc2)
! recover from electron wmporder1 buffer overflow errors
! reallocates ihole, ppbuff, and ppart as necessary
      implicit none
! irc2 = error codes, returned only if error occurs, when irc2(1) > 0
      integer, dimension(2), intent(inout) :: irc2
! local data
      integer :: iter, nter = 2
      iter = 0
      do while (irc2(1) > 0)
! ihole overflow
         if (irc2(1)==1) then
            ntmax = (1.0 + xtras)*irc2(2)
            deallocate(ihole)
            allocate(ihole(2,ntmax+1,mx1))
            irc2 = 0
            call wmporder1(ppart,ppbuff,kpic,ncl,ihole,tsort,nx,mx,     &
     &.false.,irc2)
! ppbuff overflow
         else if (irc2(1)==2) then
            npbmx = (1.0 + xtras)*irc2(2)
            deallocate(ppbuff)
            allocate(ppbuff(idimp,npbmx,mx1))
            irc2 = 0
            call wmporder1(ppart,ppbuff,kpic,ncl,ihole,tsort,nx,mx,     &
     &.false.,irc2)
! ppart overflow
         else if (irc2(1)==3) then
            iter = iter + 1
            if (iter > nter) then
               write (*,*) 'reorder_electrons1: iteration exceeded'
               exit
            endif
! restores particle coordinates from ppbuff: updates ppart, ncl
            call mprstor1(ppart,ppbuff,ncl,ihole,tsort)
! copy ordered particles to linear array: updates part
            call mpcopyout1(part,ppart,kpic,it,irc)
            deallocate(ppart)
            nppmx0 = (1.0 + xtras)*irc2(2)
            allocate(ppart(idimp,nppmx0,mx1))
! copies unordered particles to ordered array: updates ppart
            call mpcopyin1(part,ppart,kpic,irc)
            irc2 = 0
            call wmporder1(ppart,ppbuff,kpic,ncl,ihole,tsort,nx,mx,     &
     &.false.,irc2)
         endif
      enddo
!
! sanity check for electrons
      if (monitor > 0) then
         call mcheck1(ppart,kpic,nx,mx,irc)
         if (irc /= 0) stop
      endif
!
      end subroutine
!
!-----------------------------------------------------------------------
      subroutine push_electrons1(ppart,kpic)
! push electrons with OpenMP:
      implicit none
! ppart = tiled electron particle array
      real, dimension(:,:,:), intent(inout) :: ppart
! kpic = number of electrons in each tile
      integer, dimension(:), intent(inout) :: kpic
      wke = 0.0
! updates ppart, wke and possibly ncl, ihole, and irc
      if (mzf==0) then
         call wmpush1(ppart,fxe,kpic,ncl,ihole,qbme,dt,ci,wke,tpush,nx, &
     &mx,ipbc,relativity,plist,irc)
! zero force: updates ppart, wke and possibly ncl, ihole, and irc
      else
         call wmpush1zf(ppart,kpic,ncl,ihole,dt,ci,wke,tpush,nx,mx,ipbc,&
     &relativity,plist,irc)
      endif
!
! reorder electrons by tile with OpenMP:
! updates ppart, ppbuff, kpic, ncl, irc2, and possibly ihole
      if (irc==0) then
         call wmporder1(ppart,ppbuff,kpic,ncl,ihole,tsort,nx,mx,plist,  &
     &irc2)
      else
         irc2(1) = 1; irc2(2) = irc; irc = 0
      endif
!
! sanity check for electrons
      if (irc2(1)==0) then
         if (monitor > 0) then
            call mcheck1(ppart,kpic,nx,mx,irc)
            if (irc /= 0) stop
         endif
! recover from wmporder1 buffer overflow errors: updates ppart
      else if (irc2(1) > 0) then
         call reorder_electrons1(irc2)
      endif
      end subroutine
!
!-----------------------------------------------------------------------
      subroutine del_electrons1()
! delete electrons
      implicit none
      deallocate(ppart,kpic)
      end subroutine
!
!-----------------------------------------------------------------------
      subroutine init_ions1()
! initialize ions
      implicit none
! part = particle array
! background ions
      if (npxi > 0) then
         call mfdistr1(part,ampdxi,scaledxi,shiftdxi,1,npxi,nx,ipbc,    &
     &ndprofi)
         call wmvdistr1(part,1,vtxi,vxi0,npxi,nvdist)
      endif
! beam ions
      if (npxbi > 0) then
         it = npxi + 1
         call mfdistr1(part,ampdxi,scaledxi,shiftdxi,it,npxbi,nx,ipbc,  &
     &ndprofi)
         call wmvdistr1(part,it,vtdxi,vdxi,npxbi,nvdist)
      endif
!
! mark ion beam particles
      if ((nts > 0).and.(ntsc > 0)) then
         call setmbeam1(part,npxi)
      endif
!
! kipic = number of ions in each tile
      allocate(kipic(mx1))
!
! find number of ions in each of mx, tiles: updates kipic, nppmx
      call mdblkp2(part,kipic,nppmx,npi,mx,irc)
!
! allocate vector ion data
      nppmx1 = (1.0 + xtras)*nppmx
      allocate(pparti(idimp,nppmx1,mx1))
! copy ordered ion data for OpenMP: updates pparti and kipic
      call mpmovin1(part,pparti,kipic,mx,irc)
!
! sanity check for ions
      call mcheck1(pparti,kipic,nx,mx,irc)
      if (irc /= 0) stop
      end subroutine
!
!-----------------------------------------------------------------------
      subroutine reorder_ions1(irc2)
! recover from ion wmporder1 buffer overflow errors
! reallocates ihole, ppbuff, and pparti as necessary
      implicit none
! irc2 = error codes, returned only if error occurs, when irc2(1) > 0
      integer, dimension(2), intent(inout) :: irc2
! local data
      integer :: iter, nter = 2
      iter = 0
      do while (irc2(1) > 0)
! ihole overflow
         if (irc2(1)==1) then
            ntmax = (1.0 + xtras)*irc2(2)
            deallocate(ihole)
            allocate(ihole(2,ntmax+1,mx1))
            irc2 = 0
            call wmporder1(pparti,ppbuff,kipic,ncl,ihole,tsort,nx,mx,   &
     &.false.,irc2)
! ppbuff overflow
         else if (irc2(1)==2) then
            npbmx = (1.0 + xtras)*irc2(2)
            deallocate(ppbuff)
            allocate(ppbuff(idimp,npbmx,mx1))
            irc2 = 0
            call wmporder1(pparti,ppbuff,kipic,ncl,ihole,tsort,nx,mx,   &
     &.false.,irc2)
! pparti overflow
         else if (irc2(1)==3) then
            iter = iter + 1
            if (iter > nter) then
               write (*,*) 'reorder_ions1: iteration exceeded'
               exit
            endif
! restores particle coordinates from ppbuff: updates ppart, ncl
            call mprstor1(pparti,ppbuff,ncl,ihole,tsort)
! copy ordered particles to linear array: updates part
            call mpcopyout1(part,pparti,kipic,it,irc)
            deallocate(pparti)
            nppmx1 = (1.0 + xtras)*irc2(2)
            allocate(pparti(idimp,nppmx1,mx1))
! copies unordered particles to ordered array: updates ppart
            call mpcopyin1(part,pparti,kipic,irc)
            irc2 = 0
            call wmporder1(pparti,ppbuff,kipic,ncl,ihole,tsort,nx,mx,   &
     &.false.,irc2)
         endif
      enddo
!
! sanity check for ions
      if (monitor > 0) then
         call mcheck1(pparti,kipic,nx,mx,irc)
         if (irc /= 0) stop
      endif
!
      end subroutine
!
!-----------------------------------------------------------------------
      subroutine push_ions1(pparti,kipic)
! push ions with OpenMP:
      implicit none
! pparti = tiled electron/ion particle arrays
      real, dimension(:,:,:), intent(inout) :: pparti
! kipic = number of electrons/ions in each tile
      integer, dimension(:), intent(inout) :: kipic
      wki = 0.0
! updates pparti, wki and possibly ncl, ihole, and irc
      if (mzf==0) then
         call wmpush1(pparti,fxe,kipic,ncl,ihole,qbmi,dt,ci,wki,tpush,nx&
     &,mx,ipbc,relativity,plist,irc)
! zero force: updates pparti, wki and possibly ncl, ihole, and irc
      else
         call wmpush1zf(pparti,kipic,ncl,ihole,dt,ci,wki,tpush,nx,mx,   &
     &ipbc,relativity,plist,irc)
      endif
      wki = wki*rmass
!
! reorder ions by tile with OpenMP:
! updates pparti, ppbuff, kipic, ncl, irc2, and possibly ihole
      if (irc==0) then
         call wmporder1(pparti,ppbuff,kipic,ncl,ihole,tsort,nx,mx,plist,&
     &irc2)
      else
         irc2(1) = 1; irc2(2) = irc; irc = 0
      endif
!
! sanity check for ions
      if (irc2(1)==0) then
         if (monitor > 0) then
            call mcheck1(pparti,kipic,nx,mx,irc)
            if (irc /= 0) stop
         endif
! recover from wmporder1 buffer overflow errors: updates pparti
      else if (irc2(1) > 0) then
         call reorder_ions1(irc2)
      endif
      end subroutine
!
!-----------------------------------------------------------------------
      subroutine del_ions1()
! delete ions
      implicit none
      deallocate(pparti,kipic)
      end subroutine
!
!-----------------------------------------------------------------------
      subroutine es_time_reverse1()
! start running simulation backwards:
      implicit none
! need to reverse time lag in leap-frog integration scheme
      dt = -dt
      ws = 0.0
      call wmpush1zf(ppart,kpic,ncl,ihole,dt,ci,ws,tpush,nx,mx,ipbc,    &
     &relativity,plist,irc2(1))
      call wmporder1(ppart,ppbuff,kpic,ncl,ihole,tsort,nx,mx,plist,irc2)
      if (irc2(1) /= 0) stop
      if (movion==1) then
         call wmpush1zf(pparti,kipic,ncl,ihole,dt,ci,ws,tpush,nx,mx,ipbc&
     &,relativity,plist,irc2(1))
         call wmporder1(pparti,ppbuff,kipic,ncl,ihole,tsort,nx,mx,plist,&
     &irc2)
         if (irc2(1) /= 0) stop
      endif
      end subroutine
!
!-----------------------------------------------------------------------
      subroutine init_energy_diag1()
! initialize energy diagnostic
      implicit none
! wt = energy time history array
      mtw = (nloop - 1)/ntw + 1; itw = 0
      allocate(wt(mtw,4),s(4))
      wt = 0.0; s = 0.0d0
      end subroutine
!
!-----------------------------------------------------------------------
      subroutine energy_diag1(wt,ntime,iuot)
! energy diagnostic
      implicit none
! wt = energy time history array
! ntime = current time step
! iuot = output file descriptor
      real, dimension(:,:), intent(inout) :: wt
      integer, intent(in) :: ntime, iuot
      ws = we + wke + wki
      if (ntime==0) s(3) = ws
      write (iuot,*) 'Field, Kinetic and Total Energies:'
      if (movion==0) then
         write (iuot,'(3e14.7)') we, wke, ws
      else
         write (iuot,'(4e14.7)') we, wke, wki, ws
      endif
      itw = itw + 1
! store energies in time history array
      wt(itw,:) = (/we,wke,wki,ws/)
      s(1) = s(1) + we
      s(2) = s(2) + wke
      s(3) = min(s(3),dble(ws))
      s(4) = max(s(4),dble(ws))
      end subroutine
!
!-----------------------------------------------------------------------
      subroutine print_energy1(wt,iuot)
! print energy summaries
      implicit none
! wt = energy time history array
! iuot = output file descriptor
      real, dimension(:,:), intent(in) :: wt
! iuot = output file descriptor
      integer, intent(in) :: iuot
      s(3) = (s(4) - s(3))/wt(1,4)
      write (iuot,*) 'Energy Conservation = ', real(s(3))
      s(1) = s(1)/real(itw)
      write (iuot,*) 'Average Field Energy <WE> = ', real(s(1))
      s(2) = s(2)/real(itw)
      write (iuot,*) 'Average Electron Kinetic Energy <WKE> = ',        &
     &real(s(2))
      write (iuot,*) 'Ratio <WE>/<WKE>= ', real(s(1)/s(2))
      write (iuot,*)
      end subroutine
!
!-----------------------------------------------------------------------
      subroutine del_energy_diag1()
! delete energy diagnostic
      implicit none
! wt = energy time history array
      deallocate(wt,s)
      end subroutine
!
!-----------------------------------------------------------------------
      subroutine init_spectrum1()
! allocate scratch arrays for scalar fields
      implicit none
      allocate(sfield(nxe),sfieldc(nxh))
! allocate and initialize frequency array for spectral analysis
      if (ntp > 0) then
         iw = (wmax - wmin)/dw + 1.5
         allocate(wm(iw))
         do it = 1, iw
            wm(it) = wmin + dw*real(it-1)
         enddo
      endif
! allocate and initialize frequency array for ion spectral analysis
      if (movion==1) then
         if (ntdi > 0) then
            iwi = (wimax - wimin)/dwi + 1.5
            allocate(wmi(iwi))
            do it = 1, iwi
               wmi(it) = wimin + dwi*real(it-1)
            enddo
          endif
      endif
      end subroutine
!
!-----------------------------------------------------------------------
      subroutine del_spectrum1()
! delete scratch arrays for scalar fields
      implicit none
      deallocate(sfield,sfieldc)
! deallocate frequency array for spectral analysis
      if (ntp > 0) deallocate(wm)
! deallocate frequency array for ion spectral analysis
      if (movion==1) then
         if (ntdi > 0) deallocate(wmi)
      endif
      end subroutine
!
!-----------------------------------------------------------------------
      subroutine init_edensity_diag1()
! initialize electron density diagnostic
      implicit none
      fdename = 'denek1.'//cdrun
      modesxde = min(modesxde,nxh+1)
      allocate(denet(modesxde))
! open file: updates nderec and possibly iude
      if (nderec==0) call dafopenc1(denet,iude,nderec,trim(fdename))
      end subroutine
!
!-----------------------------------------------------------------------
      subroutine edensity_diag1(sfield)
! electron density diagnostic
      implicit none
! sfield = scratch array for scalar field
      real, dimension(:), intent(inout) :: sfield
      sfield = -qe
! transform electron density to fourier space: updates sfield
      isign = -1
      call mfft1r(sfield,isign,mixup,sct,tfft,indx)
! calculate smoothed density in fourier space: updates sfieldc
      call msmooth1(sfield,sfieldc,ffc,tfield,nx)
! store selected fourier modes: updates denet
      call mrdmodes1(sfieldc,denet,tfield,nx,modesxde)
! write diagnostic output: updates nderec
      call dafwritec1(denet,tdiag,iude,nderec,modesxde)
! transform smoothed electron density to real space: updates sfield
      call mfft1cr(sfieldc,sfield,mixup,sct,tfft,indx)
      call mdguard1(sfield,tguard,nx)
      end subroutine
!
!-----------------------------------------------------------------------
      subroutine del_edensity_diag1()
! delete electron density diagnostic data
      implicit none
      if (nderec > 0) then
         close(unit=iude)
         nderec = nderec - 1
      endif
      deallocate(denet)
      end subroutine
!
!-----------------------------------------------------------------------
      subroutine init_idensity_diag1()
! initialize ion density diagnostic
      implicit none
      fdiname = 'denik1.'//cdrun
      modesxdi = min(modesxdi,nxh+1)
      allocate(denit(modesxdi))
! open file: updates ndirec and possibly iudi
      if (ndirec==0) call dafopenc1(denit,iudi,ndirec,trim(fdiname))
! ion spectral analysis
      if ((nddi==2).or.(nddi==3)) then
         mtdi = (nloop - 1)/ntdi + 1; itdi = 0
         allocate(pkwdi(modesxdi,iwi,2),wkdi(modesxdi,2))
         allocate(pksdi(4,modesxdi,iwi))
         pksdi = 0.0d0
      endif
      end subroutine
!
!-----------------------------------------------------------------------
      subroutine idensity_diag1(sfield,pkwdi,wkdi,ntime)
! ion density diagnostic
      implicit none
! sfield = scratch array for scalar field
      real, dimension(:), intent(inout) :: sfield
! pkwdi = power spectrum for ion density
      real, dimension(:,:,:), intent(inout) :: pkwdi
! wkdi = maximum frequency as a function of k for ion density
      real, dimension(:,:), intent(inout) :: wkdi
! ntime = current time step
      integer, intent(in) :: ntime
      sfield = qi
! transform ion density to fourier space: updates sfield
      isign = -1
      call mfft1r(sfield,isign,mixup,sct,tfft,indx)
! calculate smoothed density in fourier space: updates sfieldc
      call msmooth1(sfield,sfieldc,ffc,tfield,nx)
! store selected fourier modes: updates denit
      call mrdmodes1(sfieldc,denit,tfield,nx,modesxdi)
! write diagnostic output: updates ndirec
      call dafwritec1(denit,tdiag,iudi,ndirec,modesxdi)
! transform smoothed ion density to real space: updates sfield
      if ((nddi==1).or.(nddi==3)) then
         call mfft1cr(sfieldc,sfield,mixup,sct,tfft,indx)
         call mdguard1(sfield,tguard,nx)
      endif
! ion spectral analysis
      if ((nddi==2).or.(nddi==3)) then
         itdi = itdi + 1
         ts = dt*real(ntime)
! performs frequency analysis of accumulated complex time series
         call micspect1(denit,wmi,pkwdi,pksdi,ts,t0,tdiag,mtdi,iwi,     &
     &modesxdi,nx,-1)
! find frequency with maximum power for each mode
         wkdi(:,1) = wmi(maxloc(pkwdi(:,:,1),dim=2))
         wkdi(:,2) = wmi(maxloc(pkwdi(:,:,2),dim=2))
      endif
      end subroutine
!
!-----------------------------------------------------------------------
      subroutine del_idensity_diag1()
! delete ion density diagnostic
      implicit none
      if (ndirec > 0) then
         close(unit=iudi)
         ndirec = ndirec - 1
      endif
      deallocate(denit)
! spectral analysis
      if ((nddi==2).or.(nddi==3)) then
         deallocate(pkwdi,wkdi,pksdi)
      endif
      end subroutine
!
!-----------------------------------------------------------------------
      subroutine init_potential_diag1()
! initialize potential diagnostic
      implicit none
      fpname = 'potk1.'//cdrun
      modesxp = min(modesxp,nxh+1)
      allocate(pott(modesxp))
! open file: updates nprec and possibly iup
      if (nprec==0) call dafopenc1(pott,iup,nprec,trim(fpname))
! spectral analysis
      if ((ndp==2).or.(ndp==3)) then
         mtp = (nloop - 1)/ntp + 1; itp = 0
         allocate(pkw(modesxp,iw,2),wk(modesxp,2))
         allocate(pks(4,modesxp,iw))
         pks = 0.0d0
      endif
      end subroutine
!
!-----------------------------------------------------------------------
      subroutine potential_diag1(sfield,pkw,wk,ntime)
! potential diagnostic
      implicit none
! sfield = scratch array for scalar field
      real, dimension(:), intent(inout) :: sfield
! pkw = power spectrum for potential
      real, dimension(:,:,:), intent(inout) :: pkw
! wk = maximum frequency as a function of k for potential
      real, dimension(:,:), intent(inout) :: wk
! ntime = current time step
      integer, intent(in) :: ntime
! calculate potential in fourier space: updates sfieldc
       call mpot1(qe,sfieldc,ffc,ws,tfield,nx)
! store selected fourier modes: updates pott
      call mrdmodes1(sfieldc,pott,tfield,nx,modesxp)
! write diagnostic output: updates nprec
      call dafwritec1(pott,tdiag,iup,nprec,modesxp)
! transform potential to real space: updates sfield
      if ((ndp==1).or.(ndp==3)) then
         call mfft1cr(sfieldc,sfield,mixup,sct,tfft,indx)
         call mdguard1(sfield,tguard,nx)
      endif
! spectral analysis
      if ((ndp==2).or.(ndp==3)) then
         itp = itp + 1
         ts = dt*real(ntime)
! performs frequency analysis of accumulated complex time series
         call micspect1(pott,wm,pkw,pks,ts,t0,tdiag,mtp,iw,modesxp,nx,1)
! find frequency with maximum power for each mode
         wk(:,1) = wm(maxloc(pkw(:,:,1),dim=2))
         wk(:,2) = wm(maxloc(pkw(:,:,2),dim=2))
      endif
      end subroutine
!
!-----------------------------------------------------------------------
      subroutine del_potential_diag1()
! delete potential diagnostic
      implicit none
      if (nprec > 0) then
         close(unit=iup)
         nprec = nprec - 1
      endif
      deallocate(pott)
! spectral analysis
      if ((ndp==2).or.(ndp==3)) then
         deallocate(pkw,wk,pks)
      endif
      ceng = affp
      end subroutine
!
!-----------------------------------------------------------------------
      subroutine init_elfield_diag1()
! initialize longitudinal efield diagnostic
      implicit none
      felname = 'elk1.'//cdrun
      modesxel = min(modesxel,nxh+1)
      allocate(elt(modesxel))
! open file: updates nelrec and possibly iuel
      if (nelrec==0) call dafopenc1(elt,iuel,nelrec,trim(felname))
      end subroutine
!
!-----------------------------------------------------------------------
      subroutine elfield_diag1(sfield)
! longitudinal efield diagnostic
      implicit none
! sfield = scratch array for scalar field
      real, dimension(:), intent(inout) :: sfield
! calculate longitudinal efield in fourier space: updates sfieldc
      call melfield1(qe,sfieldc,ffc,ws,tfield,nx)
! store selected fourier modes: updates elt
      call mrdmodes1(sfieldc,elt,tfield,nx,modesxel)
! write diagnostic output: updates nelrec
      call dafwritec1(elt,tdiag,iuel,nelrec,modesxel)
! transform longitudinal efield to real space: updates sfield
      call mfft1cr(sfieldc,sfield,mixup,sct,tfft,indx)
      call mdguard1(sfield,tguard,nx)
      end subroutine
!
!-----------------------------------------------------------------------
      subroutine del_elfield_diag1()
! delete longitudinal efield diagnostic
      implicit none
      if (nelrec > 0) then
         close(unit=iuel)
         nelrec = nelrec - 1
      endif
      deallocate(elt)
      ceng = affp
      end subroutine
!
!-----------------------------------------------------------------------
      subroutine init_evelocity_diag1()
! initialize electron velocity diagnostic
      implicit none
      allocate(fv(2*nmv+2,ndim),fvm(ndim,3))
      allocate(sfv(2*nmv+2,ndim,mx1+1))
      mtv = (nloop - 1)/ntv + 1; itv = 0
      allocate(fvtm(mtv,ndim,3))
      sfv(1,:,:) = 2.0*max(4.0*vtx+abs(vx0),4.0*vtdx+abs(vdx))
      fvtm = 0.0
      end subroutine
!
!-----------------------------------------------------------------------
      subroutine evelocity_diag1(ppart,kpic,fv,fvm,fvtm)
! electron velocity diagnostic
      implicit none
! ppart = tiled electron particle arrays
      real, dimension(:,:,:), intent(inout) :: ppart
! kpic = number of electrons in each tile
      integer, dimension(:), intent(inout) :: kpic
! fv = global electron velocity distribution functions
      real, dimension(:,:), intent(inout) :: fv
! fvmi = electron vdrift, vth, entropy for global distribution
      real, dimension(:,:), intent(inout)  :: fvm
! fvtm = time history of electron vdrift, vth, and entropy
      real, dimension(:,:,:), intent(inout) :: fvtm
! calculate electron distribution function and moments
      call mvpdist1(ppart,kpic,sfv,fvm,tdiag,np,nmv)
      fv = sfv(:,:,mx1+1)
! store time history electron vdrift, vth, and entropy
      itv = itv + 1
      fvtm(itv,:,:) = fvm
      end subroutine
!
!-----------------------------------------------------------------------
      subroutine del_evelocity_diag1()
! delete electron velocity diagnostic
      implicit none
      deallocate(fv,fvm,fvtm)
      if (allocated(sfv)) deallocate(sfv)
      end subroutine
!
!-----------------------------------------------------------------------
      subroutine init_ivelocity_diag1()
! initialize ion velocity diagnostic
      implicit none
      allocate(fvi(2*nmv+2,ndim),fvmi(ndim,3))
      allocate(sfvi(2*nmv+2,ndim,mx1+1))
      allocate(fvtmi(mtv,ndim,3))
      sfvi(1,:,:) = 2.0*max(4.0*vtxi+abs(vxi0),4.0*vtdxi+abs(vdxi))
      fvtmi = 0.0
      end subroutine
!
!-----------------------------------------------------------------------
      subroutine ivelocity_diag1(pparti,kipic,fvi,fvmi,fvtmi)
! ion velocity diagnostic
      implicit none
! pparti = tiled ion particle arrays
      real, dimension(:,:,:), intent(inout) :: pparti
! kipic = number of ions in each tile
      integer, dimension(:), intent(inout) :: kipic
! fvi = global ion velocity distribution functions
      real, dimension(:,:), intent(inout) :: fvi
! fvmi = ion vdrift, vth, entropy for global distribution
      real, dimension(:,:), intent(inout)  :: fvmi
! fvtmi = time history of ion vdrift, vth, and entropy
      real, dimension(:,:,:), intent(inout) :: fvtmi
! calculate ion distribution function and moments
      call mvpdist1(pparti,kipic,sfvi,fvmi,tdiag,npi,nmv)
      fvi = sfvi(:,:,mx1+1)
! update time step if electrons have not been calculated
      if (ndv==2) itv = itv + 1
! store time history of ion vdrift, vth, and entropy
      fvtmi(itv,:,:) = fvmi
      end subroutine
!
!-----------------------------------------------------------------------
      subroutine del_ivelocity_diag1()
! delete ion velocity diagnostic
      implicit none
      deallocate(fvi,fvmi,fvtmi)
      if (allocated(sfvi)) deallocate(sfvi)
      end subroutine
!
!-----------------------------------------------------------------------
      subroutine init_traj_diag1(ntime)
! initialize trajectory diagnostic
      implicit none
! ntime = current time step
      integer, intent(in) :: ntime
! set initial test trajectories
      if ((ntime+ntime0)==0) then
         allocate(iprobt(nprobt))
! sets test charge distribution: updates ppart, iprobt, nprobt
         call setptraj1(ppart,kpic,iprobt,nst,vtx,vtsx,dvtx,np,nprobt)
         if (nprobt.gt.16777215) then
            write(*,*) 'nprobt overflow = ', nprobt
            stop
         endif
         deallocate(iprobt)
! find number of existing test tractories: updates nprobt
      else
         call mfnptraj1(ppart,kpic,nprobt)
      endif
      allocate(partt(idimp,nprobt))
      if ((nst==1).or.(nst==2)) then
         mtt = (nloop - 1)/ntt + 1; itt = 0
         allocate(partd(mtt,idimp,nprobt))
      else if (nst==3) then
         allocate(fvtp(2*nmv+2,ndim),fvmtp(ndim,3))
         fvtp(1,:) = 2.0*max(4.0*vtx+abs(vx0),4.0*vtdx+abs(vdx))
      endif
      end subroutine
!
!-----------------------------------------------------------------------
      subroutine traj_diag1(ppart,kpic,partd,fvtp,fvmtp)
! trajectory diagnostic
      implicit none
! ppart = tiled electron particle array
      real, dimension(:,:,:), intent(inout) :: ppart
! kpic = number of electrons/ in each tile
      integer, dimension(:), intent(inout) :: kpic
! partd = trajectory time history array
      real, dimension(:,:,:), intent(inout) :: partd
! fvtp = velocity distribution function for test particles
! fvmtp = vdrift, vth, and entropy for test particles
      real, dimension(:,:), intent(inout) :: fvtp, fvmtp
! copies trajectories to array partt
      call mptraj1(ppart,kpic,partt,tdiag)
      itt = itt + 1
      if ((nst==1).or.(nst==2)) then
         partd(itt,:,:) = partt
      else if (nst==3) then
! calculate test particle distribution function and moments
         call mvdist1(partt,fvtp,fvmtp,tdiag,nprobt,nmv)
      endif
      end subroutine
!
!-----------------------------------------------------------------------
      subroutine del_traj_diag1()
! delete trajectory diagnostic
      implicit none
      if (allocated(partt)) deallocate(partt)
      if ((nst==1).or.(nst==2)) then
         deallocate(partd)
      else if (nst==3) then
         deallocate(fvtp,fvmtp)
      endif
      end subroutine
!
!-----------------------------------------------------------------------
      subroutine print_timings1(tinit,tloop,iuot)
! print timing summaries
      implicit none
! iuot = output file descriptor
      integer, intent(in) :: iuot
      real, intent(in) :: tinit
      real, intent(inout) :: tloop
! local data
      real :: time
      write (iuot,*)
      write (iuot,*) 'initialization time = ', tinit
      write (iuot,*) 'deposit time = ', tdpost
      write (iuot,*) 'guard time = ', tguard
      write (iuot,*) 'solver time = ', tfield
      write (iuot,*) 'fft time = ', tfft
      write (iuot,*) 'push time = ', tpush
      write (iuot,*) 'sort time = ', tsort
      tfield = tfield + tguard + tfft
      write (iuot,*) 'total solver time = ', tfield
      time = tdpost + tpush + tsort
      write (iuot,*) 'total particle time = ', time
      write (iuot,*) 'total diagnostic time = ', tdiag
      ws = time + tfield + tdiag
      tloop = tloop - ws
      write (iuot,*) 'total and additional time = ', ws, tloop
      write (iuot,*)
! summarize particle timings
      ws = 1.0e+09/(real(nloop)*real(np))
      write (iuot,*) 'Push Time (nsec) = ', tpush*ws
      write (iuot,*) 'Deposit Time (nsec) = ', tdpost*ws
      write (iuot,*) 'Sort Time (nsec) = ', tsort*ws
      write (iuot,*) 'Total Particle Time (nsec) = ', time*ws
      write (iuot,*)
      end subroutine
!
!-----------------------------------------------------------------------
      subroutine close_diags1(iudm)
! close diagnostics
! delete data, close fortran files, and write out diagnostic metafile
      implicit none
! iudm = diagnostic metafile file descriptor
      integer, intent(in) :: iudm
! electron density diagnostic
      if (ntde > 0) call del_edensity_diag1()
! potential diagnostic
      if (ntp > 0) call del_potential_diag1()
! longitudinal efield diagnostic
      if (ntel > 0) call del_elfield_diag1()
! ion density diagnostic
      if (movion==1) then
         if (ntdi > 0) call del_idensity_diag1()
      endif
! write final diagnostic metafile
      call writnml1(iudm)
      close(unit=iudm)
! deallocate arrays
      call del_fields1()
      call del_electrons1()
      if (movion==1) call del_ions1()
      deallocate(part,ppbuff,ncl,ihole)
      if ((ntde > 0).or.(ntp > 0).or.(ntel > 0).or.(ntdi > 0)) then
         call del_spectrum1()
      endif
      if (ntw > 0) call del_energy_diag1()
      if (ntv > 0) then
         call del_evelocity_diag1()
         if (movion==1) call del_ivelocity_diag1()
      endif
      if (ntt > 0) call del_traj_diag1()
      end subroutine
!
!-----------------------------------------------------------------------
      subroutine initialize_diagnostics1(ntime)
! initialize all diagnostics from namelist input parameters
      implicit none
! ntime = current time step
      integer, intent(in) :: ntime
! initialize energy diagnostic: updates wt
      if (ntw > 0) then
         call init_energy_diag1()
      endif
!
! allocate and initialize scratch arrays for scalar fields:
! allocates sfield
      if ((ntde > 0).or.(ntp > 0).or.(ntel > 0).or.(ntdi > 0)) then
         call init_spectrum1()
      endif
!
! initialize electron density diagnostic
      if (ntde > 0) then
         call init_edensity_diag1()
      endif
!
! initialize ion density diagnostic: allocates pkwdi, wkdi
      if (movion==1) then
         if (ntdi > 0) then
            call init_idensity_diag1()
         endif
      endif
!
! initialize potential diagnostic: allocates pkw, wk
      if (ntp > 0) then
         call init_potential_diag1()
      endif
!
! initialize longitudinal efield diagnostic
      if (ntel > 0) then
         call init_elfield_diag1()
      endif
!
! initialize velocity diagnostic:
      if (ntv > 0) then
! electrons: allocates fv, fvm, fvtm
         call init_evelocity_diag1()
! ions: allocates fvi, fvmi, fvtmi
         if (movion==1) call init_ivelocity_diag1()
      endif
!
! initialize trajectory diagnostic: allocates partd, fvtp, fvmtp
      if (ntt > 0) then
         call init_traj_diag1(ntime)
      endif
      end subroutine
!
!-----------------------------------------------------------------------
      subroutine open_restart1()
! open reset and restart files
      implicit none
! iur, iurr = restart, reset, old restart file descriptors
! local data
      character(len=10) :: cdrun0
      character(len=32) :: fname
! reset file
      fname = 'reset1'
      iurr = get_funit(iurr)
      open(iurr,file=trim(fname),form='unformatted',status='unknown')
! restart file
      fname = 'rstrt1.'//cdrun
      iur = get_funit(iur)
! start a new run from random numbers
      if (nustrt==1) then
         if (ntr > 0) then
            open(iur,file=trim(fname),form='unformatted',               &
     &status= 'unknown')
         endif
! continue a run which was interrupted
      else if (nustrt==2) then
         open(iur,file=trim(fname),form='unformatted',status='old')
! start a new run with data from a previous run
      else if (nustrt==0) then
         if (ntr > 0) then
            open(iur,file=trim(fname),form='unformatted',               &
     &status= 'unknown')
         endif
         if (idrun /= idrun0) then
            write (cdrun0,'(i10)') idrun0
            cdrun0 = adjustl(cdrun0)
            fname = 'rstrt1.'//cdrun0
            open(iur0,file=trim(fname),form='unformatted',status='old')
         else
            write (*,*) 'restart warning: old, new idruns identical'
         endif
      endif
      end subroutine
!
!-----------------------------------------------------------------------
      subroutine bwrite_restart1(iur,ntime)
! write out basic restart file for electrostatic code
      implicit none
! iur = restart file descriptor
! ntime = current time step
      integer, intent(in) :: iur, ntime
! local data
      integer :: i, j
      integer :: idimp, npp, nxv
      idimp = size(part,1)
      rewind iur
! write out current and initial time
      write (iur) ntime, ntime0
! copy ordered particles to linear array: updates part, npp
      call mpcopyout1(part,ppart,kpic,npp,irc)
! write out size of electron array
      write (iur) npp, idimp
! write out electrons, if non-zero
      if (npp > 0) then
         write (iur) ((part(i,j),i=1,idimp),j=1,npp)
      endif
! write out if ions are moving
      write (iur) movion
      if (movion==1) then
! copy ordered particles to linear array: updates part, npp
         call mpcopyout1(part,pparti,kipic,npp,irc)
! write out size of ion array
         write (iur) npp, idimp
! write out ions, if non-zero
         if (npp > 0) then
            write (iur) ((part(i,j),i=1,idimp),j=1,npp)
         endif
! write out ion density, if ions are not moving
      else
         nxv = size(qi)
         write (iur) nxv
         if (nxv > 0) then
            write (iur) (qi(j),j=1,nxv)
         endif
      endif
! write out electric field parameter
      write (iur) emf
      end subroutine
!
!-----------------------------------------------------------------------
      subroutine bread_restart1(iur)
! read in basic restart file for electrostatic code
      implicit none
! iur = restart file descriptor
      integer, intent(in) :: iur
! local data
      integer :: i, j
      integer :: ndimp, npp, ios, it
      integer :: nppmx, nppmx0, ntmax, npbmx, nppmx1
! part = linear particle array
      if (.not.allocated(part)) allocate(part(idimp,max(np,npi)))
!
! read in current and initial time
      rewind iur
      read (iur,iostat=ios) ntime, ntime0
      if (ios /= 0) then
         write (*,*) 'ntime restart error, ios = ', ios
         stop
      endif
      write (*,*) 'restarting from ntime, idrun0 = ', ntime, idrun0
! read in size of electron array
      read (iur,iostat=ios) npp, ndimp
      if (ios /= 0) then
         write (*,*) 'np restart error, ios = ', ios
         stop
      endif
      if (ndimp /= size(part,1)) then
         write (*,*) 'restart error, idimp=', ndimp, size(part,1)
         stop
      endif
      if (npp /= np) then
         write (*,*) 'restart warning: new np/old np=', npp, np
         np = npp
      endif
! read in electrons, if non-zero
      if (npp > 0) then
         read (iur,iostat=ios) ((part(i,j),i=1,idimp),j=1,npp)
         if (ios /= 0) then
            write (*,*) 'electron array read error, ios = ', ios
            stop
         endif
      endif
! kpic = number of electrons in each tile
      if (.not.allocated(kpic)) allocate(kpic(mx1))
! find number of electrons in each of mx, tiles: updates kpic, nppmx
      call mdblkp2(part,kpic,nppmx,np,mx,irc)
! allocate vector electron data
      nppmx0 = (1.0 + xtras)*nppmx
      ntmax = xtras*nppmx
      npbmx = xtras*nppmx
      if (.not.allocated(ppart)) allocate(ppart(idimp,nppmx0,mx1))
      if (.not.allocated(ppbuff)) allocate(ppbuff(idimp,npbmx,mx1))
      if (.not.allocated(ncl)) allocate(ncl(2,mx1))
      if (.not.allocated(ihole)) allocate(ihole(2,ntmax+1,mx1))
! copy ordered electron data for OpenMP: updates ppart and kpic
      call mpmovin1(part,ppart,kpic,mx,irc)
! sanity check for electrons
      call mcheck1(ppart,kpic,nx,mx,irc)
      if (irc /= 0) stop
! read in to determine if ions are moving
      read (iur,iostat=ios) it
      if (ios /= 0) then
         write (*,*) 'movion restart error, ios = ', ios
         stop
      endif
      if (it /= movion) then
         write (*,*) 'movion restart error, movion = ', it, movion
         stop
      endif
! ions are moving
      if (movion==1) then
! read in size of ion array
         read (iur,iostat=ios) npp, ndimp
         if (ios /= 0) then
            write (*,*) 'npi restart error, ios = ', ios
            stop
         endif
         if (ndimp /= size(part,1)) then
            write (*,*) 'ion restart error, idimp=',ndimp,size(part,1)
            stop
         endif
         if (npp /= npi) then
            write (*,*) 'restart warning: new npi/old npi=', npp, npi
            npi = npp
         endif
! read in ions, if non-zero
         if (npi > 0) then
            read (iur,iostat=ios) ((part(i,j),i=1,idimp),j=1,npp)
            if (ios /= 0) then
               write (*,*) 'ion array read error, ios = ', ios
               stop
            endif
         endif
! kipic = number of ions in each tile
         if (.not.allocated(kipic)) allocate(kipic(mx1))
! find number of ions in each of mx, tiles: updates kipic, nppmx
         call mdblkp2(part,kipic,nppmx,npi,mx,irc)
! allocate vector ion data
         nppmx1 = (1.0 + xtras)*nppmx
         if (.not.allocated(pparti)) allocate(pparti(idimp,nppmx1,mx1))
! copy ordered ion data for OpenMP: updates pparti and kipic
         call mpmovin1(part,pparti,kipic,mx,irc)
! sanity check for ions
         call mcheck1(pparti,kipic,nx,mx,irc)
         if (irc /= 0) stop
! ions are not moving, read in ion density
      else
         read (iur,iostat=ios) it
         if (ios /= 0) then
            write (*,*) 'qi size restart error, ios = ', ios
            stop
         endif
         if (it > size(qi)) then
            write (*,*) 'qi restart error, size(qi)=',it,size(qi)
            stop
         endif
         if (it > 0) then
            read (iur,iostat=ios) (qi(j),j=1,it)
            if (ios /= 0) then
               write (*,*) 'qi read error, ios = ', ios
               stop
            endif
         endif
      endif
! read in electric field parameter
      read (iur,iostat=ios) it
      if (ios /= 0) then
         write (*,*) 'emf restart error, ios = ', ios
         stop
      endif
      if (it /= emf) then
         write (*,*) 'warning: emf values differ, emf=',it,emf
      endif
      ntime0 = ntime0 + ntime
      end subroutine
!
!-----------------------------------------------------------------------
      subroutine dwrite_restart1(iur)
! write out restart diagnostic file for electrostatic code
      implicit none
! iur = restart file descriptor
      integer, intent(in) :: iur
! local data
      integer :: i, j, k, it, is, ios
      character(len=32) :: fname
! write out energy diagnostic parameter
      write (iur) ntw
      if (ntw > 0) then
         write (iur) itw
! write out time history array sizes and data
         if (itw > 0) then
            it = size(wt,2)
            write (iur) size(wt,1), it
            write (iur) ((wt(i,j),i=1,itw),j=1,it)
         endif
      endif
!
! write out electron density diagnostic parameter
      write (iur) ntde
! write out record location
      if (ntde > 0) then
         write (iur) nderec
! write out record length (zero if error) and file name (if no error)
         if (nderec > 0) then
            inquire(file=fdename,recl=it,iostat=ios)
            if (ios /= 0) it = 0
            write (iur) it
            if (it > 0) then
               fname = fdename
               write (iur) fname
            endif
         endif
      endif
!
! write out potential diagnostic parameter
      write (iur) ntp
! write out record location
      if (ntp > 0) then
         write (iur) nprec
! write out record length (zero if error) and file name (if no error)
         if (nprec > 0) then
            inquire(file=fpname,recl=it,iostat=ios)
            if (ios /= 0) it = 0
            write (iur) it
            if (it > 0) then
               fname = fpname
               write (iur) fname
            endif
         endif
! write out spectrum flag
         if ((ndp==2).or.(ndp==3)) then
            write (iur) itp
! write out spectrum sizes and data
            if (itp > 0) then
               it = size(pks,2); is = size(pks,3)
               write (iur) size(pks,1), it, is
               write (iur) (((pks(i,j,k),i=1,4),j=1,it),k=1,is)
            endif
         else
            it = 0
            write (iur) it
         endif
      endif
!
! write out longitudinal efield diagnostic parameter
      write (iur) ntel
! write out record location
      if (ntel > 0) then
         write (iur) nelrec
! write out record length (zero if error) and file name (if no error)
         if (nelrec > 0) then
            inquire(file=felname,recl=it,iostat=ios)
            if (ios /= 0) it = 0
            write (iur) it
            if (it > 0) then
               fname = felname
               write (iur) fname
            endif
         endif
      endif
!
! write out ion density diagnostic parameter
      if (movion==1) then
         write (iur) ntdi
! write out record location
         if (ntdi > 0) then
            write (iur) ndirec
! write out record length (zero if error) and file name (if no error)
            if (ndirec > 0) then
               inquire(file=fdiname,recl=it,iostat=ios)
               if (ios /= 0) it = 0
               write (iur) it
               if (it > 0) then
                  fname = fdiname
                  write (iur) fname
               endif
            endif
! write out spectrum flag
            if ((nddi==2).or.(nddi==3)) then
               write (iur) itdi
! write out spectrum sizes and data
               if (itdi > 0) then
                  it = size(pksdi,2); is = size(pksdi,3)
                  write (iur) size(pksdi,1), it, is
                  write (iur) (((pksdi(i,j,k),i=1,4),j=1,it),k=1,is)
               endif
            else
               it = 0
               write (iur) it
            endif
         endif
      endif
!
! write out velocity diagnostic parameter
      write (iur) ntv
      if (ntv > 0) then
         write (iur) itv
! write out time history array sizes and data
         if (itv > 0) then
            it = size(fvtm,2)
            write (iur) size(fvtm,1), it, size(fvtm,3)
            write (iur) (((fvtm(i,j,k),i=1,itv),j=1,it),k=1,3)
            if (movion==1) then
               write (iur) size(fvtmi,1), it, size(fvtmi,3)
               write (iur) (((fvtmi(i,j,k),i=1,itv),j=1,it),k=1,3)
            endif
         endif
      endif
!
! write out trajectory diagnostic parameter
      write (iur) ntt
      if (ntt > 0) then
         if ((nst==1).or.(nst==2)) then
            write (iur) itt
! write out time history sizes and data
            if (itt > 0) then
               it = size(partd,2); is = size(partd,3)
               write (iur) size(partd,1), it, is
               write (iur) (((partd(i,j,k),i=1,itt),j=1,it),k=1,is)
            endif
         else
            it = 0
            write (iur) it
         endif
      endif
      end subroutine
!
!-----------------------------------------------------------------------
      subroutine dread_restart1(iur)
! read in restart diagnostic file for electrostatic code
      implicit none
! iur = restart file descriptor
      integer, intent(in) :: iur
! local data
      integer :: i, j, k, it, is, ir, ios
      character(len=32) :: fname
! read in energy diagnostic parameter
      read (iur,iostat=ios) it
      if (ios /= 0) then
         write (*,*) 'ntw restart error, ios = ', ios
         stop
      endif
      if (it /= ntw) then
         write (*,*) 'restart error: read/expected ntw=', it, ntw
         stop
      endif
      if (ntw > 0) then
         read (iur,iostat=ios) itw
         if (ios /= 0) then
            write (*,*) 'itw restart error, ios = ', ios
            stop
         endif
! read in time history array sizes and data
         if (itw > 0) then
            read (iur,iostat=ios) is, it
            if (ios /= 0) then
               write (*,*) 'ntw restart array size error, ios = ', ios
               stop
            endif
            if (is /= mtw) then
               write (*,*) 'restart error: read/expected mtw=', is, mtw
               stop
            endif
            if (it > size(wt,2)) then
               write (*,*) 'wt size error read/expected=', it,size(wt,2)
               stop
            endif
            read (iur,iostat=ios) ((wt(i,j),i=1,itw),j=1,it)
            if (ios /= 0) then
               write (*,*) 'wt array read error, ios = ', ios
               stop
            endif
! restore energy accumulations
            if (allocated(s)) then
               s = 0.0
               s(1) = wt(1,1)
               s(2) = wt(1,2)
               s(3) = dble(wt(1,4))
               s(4) = s(3)
               do it = 2, itw
                  s(1) = s(1) + wt(it,1)
                  s(2) = s(2) + wt(it,2)
                  s(3) = min(s(3),dble(wt(it,4)))
                  s(4) = max(s(4),dble(wt(it,4)))
               enddo
            endif
         endif
      endif
!
! read in electron density diagnostic parameter
      read (iur,iostat=ios) it
      if (ios /= 0) then
         write (*,*) 'ntde restart error, ios = ', ios
         stop
      endif
      if (it /= ntde) then
         write (*,*) 'restart error: read/expected ntde=', it, ntde
         stop
      endif
! read in record location
      if (ntde > 0) then
         read (iur,iostat=ios) nderec
         if (ios /= 0) then
            write (*,*) 'nderec restart error, ios = ', ios
            stop
         endif
! read in record length (zero if error) and file name (if no error)
         if (nderec > 0) then
            read (iur,iostat=ios) it
            if (ios /= 0) then
               write (*,*) 'ntde record length error, ios = ', ios
               stop
            endif
            if (it==0) then
               write (*,*) 'ntde zero length record error'
               stop
            endif
            read (iur,iostat=ios) fname
            if (ios /= 0) then
               write (*,*) 'ntde file name error, ios = ', ios
               stop
            endif
            fdename = fname
         endif
      endif
!
! read in potential diagnostic parameter
      read (iur,iostat=ios) it
      if (ios /= 0) then
         write (*,*) 'ntp restart error, ios = ', ios
         stop
      endif
      if (it /= ntp) then
         write (*,*) 'restart error: read/expected ntp=', it, ntp
         stop
      endif
! read in record location
      if (ntp > 0) then
         read (iur,iostat=ios) nprec
         if (ios /= 0) then
            write (*,*) 'nprec restart error, ios = ', ios
            stop
         endif
! read in record length (zero if error) and file name (if no error)
         if (nprec > 0) then
            read (iur,iostat=ios) it
            if (ios /= 0) then
               write (*,*) 'ntp record length error, ios = ', ios
               stop
            endif
            if (it==0) then
               write (*,*) 'ntp zero length record error'
               stop
            endif
            read (iur,iostat=ios) fname
            if (ios /= 0) then
               write (*,*) 'ntp file name error, ios = ', ios
               stop
            endif
            fpname = fname
         endif
! read in spectrum flag
         read (iur,iostat=ios) itp
         if (ios /= 0) then
            write (*,*) 'itp restart error, ios = ', ios
            stop
         endif
! read in spectrum sizes and data
         if (itp > 0) then
            read (iur,iostat=ios) ir, it, is
            if (ios /= 0) then
               write (*,*) 'ntp restart array size error, ios = ', ios
               stop
            endif
            if (ir /= 4) then
               write (*,*) 'pks size error: read/expected 4 =', ir
               stop
            endif
            if (it /= modesxp) then
               write (*,*) 'pks size error: read/expected modesxp=', it,&
     &modesxp
               stop
            endif
            if (is /= iw) then
               write (*,*) 'pks size error: read/expected iw=', is, iw
               stop
            endif
            read (iur,iostat=ios) (((pks(i,j,k),i=1,4),j=1,it),k=1,is)
            if (ios /= 0) then
                  write (*,*) 'pks array read error, ios = ', ios
                  stop
            endif
         endif
      endif
!
! read in longitudinal efield diagnostic parameter
      read (iur,iostat=ios) it
      if (ios /= 0) then
         write (*,*) 'ntel restart error, ios = ', ios
         stop
      endif
      if (it /= ntel) then
         write (*,*) 'restart error: read/expected ntel=', it, ntel
         stop
      endif
! read in record location
      if (ntel > 0) then
         read (iur,iostat=ios) nelrec
         if (ios /= 0) then
            write (*,*) 'nelrec restart error, ios = ', ios
            stop
         endif
! read in record length (zero if error) and file name (if no error)
         if (nelrec > 0) then
            read (iur,iostat=ios) it
            if (ios /= 0) then
               write (*,*) 'ntel record length error, ios = ', ios
               stop
            endif
            if (it==0) then
               write (*,*) 'ntel zero length record error'
               stop
            endif
            read (iur,iostat=ios) fname
            if (ios /= 0) then
               write (*,*) 'ntel file name error, ios = ', ios
               stop
            endif
            felname = fname
         endif
      endif
!
! read in ion density diagnostic parameter
      if (movion==1) then
         read (iur,iostat=ios) it
         if (ios /= 0) then
            write (*,*) 'ntdi restart error, ios = ', ios
            stop
         endif
         if (it /= ntdi) then
            write (*,*) 'restart error: read/expected ntdi=', it, ntdi
            stop
         endif
! read in record location
         if (ntdi > 0) then
            read (iur,iostat=ios) ndirec
            if (ios /= 0) then
               write (*,*) 'ndirec restart error, ios = ', ios
               stop
            endif
! read in record length (zero if error) and file name (if no error)
            if (ndirec > 0) then
               read (iur,iostat=ios) it
               if (ios /= 0) then
                  write (*,*) 'ntdi record length error, ios = ', ios
                  stop
               endif
               if (it==0) then
                  write (*,*) 'ntdi zero length record error'
                  stop
               endif
               read (iur,iostat=ios) fname
               if (ios /= 0) then
                  write (*,*) 'ntdi file name error, ios = ', ios
               stop
               endif
               fdiname = fname
            endif
! read in spectrum flag
            read (iur,iostat=ios) itdi
            if (ios /= 0) then
               write (*,*) 'itdi restart error, ios = ', ios
               stop
            endif
! read in spectrum sizes and data
            if (itdi > 0) then
               read (iur,iostat=ios) ir, it, is
               if (ios /= 0) then
                  write (*,*) 'ntdi restart array size error, ios=', ios
                  stop
               endif
               if (ir /= 4) then
                  write (*,*) 'pksdi size error: read/expected 4 =', ir
                  stop
               endif
               if (it /= modesxdi) then
                  write (*,*) 'pksdi size error: read/expected modesxdi=&
     &', it, modesxdi
                  stop
               endif
               if (is /= iwi) then
                  write (*,*) 'pksdi size error: read/expected iwi=',is,&
     &iwi
                  stop
               endif
               read (iur,iostat=ios) (((pksdi(i,j,k),i=1,4),j=1,it),    &
     &k=1,is)
               if (ios /= 0) then
                  write (*,*) 'pksdi array read error, ios = ', ios
                  stop
               endif
            endif
         endif
      endif
!
! read in velocity diagnostic parameter
      read (iur,iostat=ios) it
      if (ios /= 0) then
         write (*,*) 'ntv restart error, ios = ', ios
         stop
      endif
      if (it /= ntv) then
         write (*,*) 'restart error: read/expected ntv=', it, ntv
         stop
      endif
      if (ntv > 0) then
         read (iur,iostat=ios) itv
         if (ios /= 0) then
            write (*,*) 'itv restart error, ios = ', ios
            stop
         endif
! read in time history array sizes and data
         if (itv > 0) then
            read (iur,iostat=ios) is, it, ir
            if (ios /= 0) then
               write (*,*) 'ntv restart array size error, ios = ', ios
               stop
            endif
            if (is /= mtv) then
               write (*,*) 'restart error: read/expected mtv=', is, mtv
               stop
            endif
            if (it /= ndim) then
               write (*,*) 'fvtm size error read/expected ndim=', it,   &
     &ndim
               stop
            endif
            if (ir /= 3) then
               write (*,*) 'fvtm size error read/expected 3=', ir
               stop
            endif
            read (iur,iostat=ios) (((fvtm(i,j,k),i=1,itv),j=1,it),k=1,3)
            if (ios /= 0) then
               write (*,*) 'fvtm array read error, ios = ', ios
               stop
            endif
            if (movion==1) then
               read (iur,iostat=ios) is, it, ir
               if (ios /= 0) then
                  write (*,*) 'ntv ion restart array size error, ios = ', ios
                  stop
               endif
               if (is /= mtv) then
                  write (*,*) 'ion restart error: read/expected mtv=',  &
     &is, mtv
                  stop
               endif
               if (it /= ndim) then
                  write (*,*) 'fvtmi size error read/expected ndim=', it&
     &, ndim
                  stop
               endif
               if (ir /= 3) then
                  write (*,*) 'fvtmi size error read/expected 3=', ir
                  stop
               endif
               read (iur,iostat=ios) (((fvtmi(i,j,k),i=1,itv),j=1,it),  &
     &k=1,3)
               if (ios /= 0) then
                  write (*,*) 'fvtmi array read error, ios = ', ios
                  stop
               endif
            endif
         endif
      endif
!
! read in trajectory diagnostic parameter
      read (iur,iostat=ios) it
      if (ios /= 0) then
         write (*,*) 'ntt restart error, ios = ', ios
         stop
      endif
      if (it /= ntt) then
         write (*,*) 'restart error: read/expected ntt=', it, ntt
         stop
      endif
      if (ntt > 0) then
         read (iur,iostat=ios) itt
         if (ios /= 0) then
            write (*,*) 'itt restart error, ios = ', ios
            stop
         endif
! read in time history sizes and data
         if (itt > 0) then
            if ((nst==1).or.(nst==2)) then
               read (iur,iostat=ios) ir, it, is
               if (ios /= 0) then
                  write (*,*) 'ntt restart array size error, ios = ',   &
     &ios
                  stop
               endif
               if (ir /= mtt) then
                  write (*,*) 'restart error: read/expected mtt=', ir,  &
     &mtt
                  stop
               endif
               if (it /= idimp) then
                  write (*,*) 'partd size error read/expected idimp=',  &
     &it, size(partd,2)
                  stop
               endif
               if (is /= nprobt) then
                  write (*,*) 'partd size error read/expected nprobt=', &
     &is, nprobt
                  stop
               endif
               read (iur,iostat=ios) (((partd(i,j,k),i=1,itt),j=1,it),  &
     &k=1,is)
               if (ios /= 0) then
                  write (*,*) 'partd array read error, ios = ', ios
                  stop
               endif
            endif
         endif
      endif
      end subroutine
!
!-----------------------------------------------------------------------
      subroutine close_restart1()
! close reset and restart files
      implicit none
! iur, iurr = restart, reset, old restart file descriptors
      close(unit=iurr)
      if (nustrt==1) then
         if (ntr > 0) close(unit=iur)
      else if (nustrt==2) then
         close(unit=iur)
      else if (nustrt==0) then
         if (ntr > 0) close(unit=iur)
         if (idrun /= idrun0) close(unit=iur0)
      endif
      end subroutine
!
!-----------------------------------------------------------------------
      subroutine reset_diags1()
! reset electrostatic diagnostics
      implicit none
      if (ntw > 0) then
         itw = 0; wt = 0.0
         if (allocated(s)) s = 0.0d0
      endif
      if (ntde > 0) then
         if (nderec > 1) nderec = 1
      endif
      if (ntp > 0) then
         if (nprec > 1) nprec = 1
         if ((ndp==2).or.(ndp==3)) then
            itp = 0; pks = 0.0d0
         endif
      endif
      if (movion==1) then
         if (ntdi > 0) then
            if (ndirec > 1) ndirec = 1
            if ((nddi==2).or.(nddi==3)) then
               itdi = 0; pksdi = 0.0d0
            endif
         endif
      endif
      if (ntel > 0) then
         if (nelrec > 1) nelrec = 1
      endif
      if (ntv > 0) then
         itv = 0; fvtm = 0.0
         if (movion==1) then
            fvtmi = 0.0
         endif
      endif
      if (ntt > 0) itt = 0
      end subroutine
!
      end module
