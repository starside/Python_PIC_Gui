!-----------------------------------------------------------------------
! High Level library for 1-2/2D Electromagnetic OpenMP PIC code
! init_fields13: allocate electromagnetic field data for standard code
! del_fields13: delete electromagnetic field data for standard code
! init_detfield13: calculate initial darwin electric field with
!                  approximate value
! init_electrons13: initialize electrons for 1-2/2d code
! deposit_ecurrent13: deposit electron current with OpenMP
! push_electrons13: push electrons with OpenMP
! init_ions13: initialize ions for 1-2/2d code
! deposit_icurrent13: deposit ion current with OpenMP
! push_ions13: push ions with OpenMP
! em_time_reverse1: start running simulation backwards
! init_energy_diag13: initialize energy diagnostic
! energy_diag13: energy diagnostic
! print_energy13: print energy summaries
! del_energy_diag13: delete energy diagnostic
! init_spectrum13: allocate scratch arrays for vector fields
! del_spectrum13: delete scratch arrays for vector fields
! init_icurrent_diag13: initialize ion current density diagnostic
! icurrent_diag13: ion current density diagnostic
! del_icurrent_diag13: delete ion current density diagnostic
! init_vrpotential_diag13: initialize radiative vector potential
!                          diagnostic
! vrpotential_diag13: radiative vector potential diagnostic
! del_vrpotential_diag13: delete radiative vector potential diagnostic
! init_vpotential_diag13: initialize vector potential diagnostic
! vpotential_diag13: vector potential diagnostic
! del_vpotential_diag13: delete vector potential diagnostic
! init_etfield_diag13: initialize transverse efield diagnostic
! etfield_diag13: transverse efield diagnostic
! del_etfield_diag13: delete transverse efield diagnostic
! init_bfield_diag13: initialize magnetic field diagnostic
! bfield_diag13: magnetic field diagnostic
! del_bfield_diag13: delete magnetic field diagnostic
! init_evelocity_diag13: initialize electron velocity diagnostic
! evelocity_diag13: electron velocity diagnostic
! del_evelocity_diag13: delete electron velocity diagnostic
! init_ivelocity_diag13: initialize ion velocity diagnostic
! ivelocity_diag13: ion velocity diagnostic
! del_ivelocity_diag13: delete ion velocity diagnostic
! init_traj_diag13: initialize trajectory diagnostic
! traj_diag13: trajectory diagnostic
! print_timings13: print timing summaries
! close_diags13: close diagnostics
! initialize_diagnostics13: initialize all diagnostics from namelist
!                           input parameters
! bwrite_restart13: write out basic restart file for electromagnetic
!                   code
! bread_restart13: read in basic restart file for electromagnetic code
! dwrite_restart13: write out restart diagnostic file for
!                   electromagnetic code
! dread_restart13: read in restart diagnostic file for electromagnetic
!                  code
! reset_diags13: reset electrostatic/electromagnetic diagnostics
! written by Viktor K. Decyk, UCLA
! copyright 1999-2016, regents of the university of california
! update: december 19, 2016
      module fb1
      use f1
      use mbpush1
      use mcurd1
      implicit none
! wf/wb = magnetic field/transverse electric field energies
      real :: wf = 0.0, wb = 0.0
      real :: zero = 0.0
!
! declare scalars for standard code
      integer :: i
      real :: dth, omt
      real :: vtyi, vtzi, vtdyi, vtdzi
!
! declare scalars for diagnostics
      integer :: iwr, mta, mtet, mtar, mtji
      integer :: ita = 0, itet = 0, itar = 0, itji = 0
! default Fortran unit numbers
      integer :: iua = 13, iuet = 14, iub = 15, iuar = 16
      integer :: iuji = 21
      real :: wef
!
! declare and initialize timing data
      real :: tdjpost = 0.0
!
! declare arrays for standard code:
! cue/cui = electron/ion current density with guard cells
      real, dimension(:,:), allocatable :: cue, cui
! fxyze/byze = smoothed electric/magnetic field with guard cells
      real, dimension(:,:), allocatable :: fxyze, byze
! eyz/byz = transverse electric/magnetic field in fourier space
      complex, dimension(:,:), allocatable :: eyz, byz
! arrays required for darwin initial fields
      real, dimension(:,:), allocatable :: amu, dcu
!
! public diagnostic arrays
! s = scratch array for energies
      double precision, dimension(:), allocatable :: s
! vfield = scratch array for vector field
      real, dimension(:,:), allocatable :: vfield
! vpkwji = power spectrum for ion current density
      real, dimension(:,:,:,:), allocatable :: vpkwji
! vpkwr = power spectrum for radiative vector potential
      real, dimension(:,:,:,:), allocatable :: vpkwr
! vpkw = power spectrum for vector potential
      real, dimension(:,:,:,:), allocatable :: vpkw
! vpkwet = power spectrum for transverse efield
      real, dimension(:,:,:,:), allocatable :: vpkwet
! vwkji = maximum frequency as a function of k for ion current
      real, dimension(:,:,:), allocatable :: vwkji
! vwkr = maximum frequency as a function of k for radiative vector
!        potential
      real, dimension(:,:,:), allocatable :: vwkr
! oldcu = previous current density with guard cells
      real, dimension(:,:), allocatable :: oldcu
! vwk = maximum frequency as a function of k for vector potential
      real, dimension(:,:,:), allocatable :: vwk
! vwket = maximum frequency as a function of k for transverse efield
      real, dimension(:,:,:), allocatable :: vwket
!
! private diagnostic arrays
! scratch array for vector field
      complex, dimension(:,:), allocatable :: vfieldc
! scratch arrays for spectral analysis
      real, dimension(:), allocatable :: wmr
! curit = store selected fourier modes for ion current density
      complex, dimension(:,:), allocatable :: curit
! vpksji = accumulated complex spectrum for ion current density
      double precision, dimension(:,:,:,:), allocatable :: vpksji
! vpotr = store selected fourier modes for radiative vector potential
      complex, dimension(:,:), allocatable :: vpotr
! vpksr = accumulated complex spectrum for radiative vector potential
      double precision, dimension(:,:,:,:), allocatable :: vpksr
! vpott = store selected fourier modes for vector potential
      complex, dimension(:,:), allocatable :: vpott
! vpks = accumulated complex spectrum for vector potential
      double precision, dimension(:,:,:,:), allocatable :: vpks
! ett = store selected fourier modes for transverse efield
      complex, dimension(:,:), allocatable :: ett
! vpkset = accumulated complex spectrum for transverse efield
      double precision, dimension(:,:,:,:), allocatable :: vpkset
! bt = store selected fourier modes for magnetic field
      complex, dimension(:,:), allocatable :: bt
! sfv/sfvi = electron/ion velocity distribution functions in tile
      real, dimension(:,:,:), allocatable :: sfv, sfvi
! iprobt = scratch array 
      integer, dimension(:), allocatable :: iprobt
! partt = particle trajectories tracked
      real, dimension(:,:), allocatable :: partt
!
      save
!
      public :: cue, cui, fxyze, byze, eyz, byz
      public :: s, vfield
      public :: vpkwji, vpkwr, vpkw, vpkwet, vwkji, vwkr, vwk, vwket
      public :: wmr
      public :: curit, vpksji
      private :: vfieldc
      private :: amu, dcu
      private :: vpotr, vpott, ett, bt
      private :: vpksr, vpks, vpkset
      private :: sfv, sfvi, iprobt, partt
!
      contains
!
!-----------------------------------------------------------------------
      subroutine init_fields13()
! allocate electromagnetic field data for standard code
      implicit none
! allocate electrostatic field data: qe, qi, fxe, ffc, mixup, sct
      call init_fields1()
      allocate(fxyze(3,nxe),cue(2,nxe),byze(2,nxe))
      allocate(eyz(2,nxeh),byz(2,nxeh))
      end subroutine
!
!-----------------------------------------------------------------------
      subroutine del_fields13()
! delete electromagnetic field data for standard code
      implicit none
      deallocate(fxyze,cue,byze,eyz,byz)
      call del_fields1()
      end subroutine
!
!-----------------------------------------------------------------------
      subroutine init_detfield13()
! calculate initial darwin electric field with approximate value
! approximation assumes initial accelerations are zero
      implicit none
      allocate(amu(2,nxe),dcu(2,nxe))
      amu = 0.0
      call wmgmjpost1(ppart,amu,kpic,qme,ci,tdjpost,mx,relativity)
      call macguard1(amu,tguard,nx)
      isign = -1
      call mfft1rn(amu,isign,mixup,sct,tfft,indx)
      call mdcuperp1(dcu,amu,tfield,nx)
      deallocate(amu)
      call metfield1(dcu,eyz,ffc,ci,wf,tfield,nx)
      deallocate(dcu)
      end subroutine
!
!-----------------------------------------------------------------------
      subroutine init_electrons13()
! initialize electrons for 1-2/2d code
      implicit none
! part = particle array
      allocate(part(idimp,max(np,npi)))
! background electrons
      if (npx > 0) then
! calculates initial particle co-ordinates with various density profiles
         call mfdistr1(part,ampdx,scaledx,shiftdx,1,npx,nx,ipbc,ndprof)
! initialize particle velocities
         call wmvdistr1h(part,1,vtx,vty,vtz,vx0,vy0,vz0,npx,nvdist)
      endif
! beam electrons
      if (npxb > 0) then
         it = npx + 1
! calculates initial particle co-ordinates with various density profiles
         call mfdistr1(part,ampdx,scaledx,shiftdx,it,npxb,nx,ipbc,ndprof&
     &)
! initialize particle velocities
         call wmvdistr1h(part,it,vtdx,vtdy,vtdz,vdx,vdy,vdz,npxb,nvdist)
      endif
!
! mark electron beam particles
      if ((nts > 0).and.(ntsc > 0)) then
         call setmbeam1(part,npx)
      endif
!
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
!
! copy ordered electron data for OpenMP: updates ppart and kpic
      call mpmovin1(part,ppart,kpic,mx,irc)
!
! sanity check for electrons
      call mcheck1(ppart,kpic,nx,mx,irc)
      if (irc /= 0) stop
!
      end subroutine
!
!-----------------------------------------------------------------------
      subroutine deposit_ecurrent13(ppart,kpic)
! deposit electron current with OpenMP
      implicit none
! ppart = tiled electron particle array
      real, dimension(:,:,:), intent(inout) :: ppart
! kpic = number of electrons in each tile
      integer, dimension(:), intent(inout) :: kpic
! updates ppart and cue, and possibly ncl, ihole, irc
      call wmdjpost1(ppart,cue,kpic,ncl,ihole,qme,dth,ci,tdjpost,nx,mx, &
     &ipbc,relativity,plist,irc)
! add guard cells: updates cue
      call macguard1(cue,tguard,nx)
!
! reorder electrons by tile with OpenMP:
! updates ppart, ppbuff, kpic, ncl, irc, and possibly ihole
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
      subroutine push_electrons13(ppart,kpic)
! push electrons with OpenMP
      implicit none
! ppart = tiled electron particle array
      real, dimension(:,:,:), intent(inout) :: ppart
! kpic = number of electrons in each tile
      integer, dimension(:), intent(inout) :: kpic
      wke = 0.0
! updates ppart and wke, and possibly ncl, ihole, irc
      if (mzf==0) then
         call wmbpush1(ppart,fxyze,byze,kpic,ncl,ihole,omx,qbme,dt,dth, &
     &ci,wke,tpush,nx,mx,ipbc,relativity,plist,irc)
! zero force: updates ppart, wke and possibly ncl, ihole, and irc
      else
         call wmpush1zf(ppart,kpic,ncl,ihole,dth,ci,wke,tpush,nx,mx,ipbc&
     &,relativity,plist,irc)
      endif
!
! reorder electrons by tile with OpenMP:
! updates ppart, ppbuff, kpic, ncl, irc, and possibly ihole
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
      subroutine init_ions13()
! initialize ions for 1-2/2d code
      implicit none
! part = particle array
      allocate(kipic(mx1),cui(2,nxe))
      cui = 0.0
! background ions
      if (npxi > 0) then
! calculates initial particle co-ordinates with various density profiles
            call mfdistr1(part,ampdxi,scaledxi,shiftdxi,1,npxi,nx,ipbc, &
     &ndprofi)
! initialize particle velocities
            call wmvdistr1h(part,1,vtxi,vtyi,vtzi,vxi0,vyi0,vzi0,npxi,  &
     &nvdist)
      endif
! beam ions
      if (npxbi > 0) then
            it = npxi + 1
! calculates initial particle co-ordinates with various density profiles
            call mfdistr1(part,ampdxi,scaledxi,shiftdxi,it,npxbi,nx,ipbc&
     &,ndprofi)
! initialize particle velocities
            call wmvdistr1h(part,it,vtdxi,vtdyi,vtdzi,vdxi,vdyi,vdzi,   &
     &npxbi,nvdist)
      endif
!
! mark ion beam particles
      if ((nts > 0).and.(ntsc > 0)) then
            call setmbeam1(part,npxi)
      endif
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
      subroutine deposit_icurrent13(pparti,kipic)
! deposit ion current with OpenMP
      implicit none
! pparti = tiled electron/ion particle arrays
      real, dimension(:,:,:), intent(inout) :: pparti
! kipic = number of electrons/ions in each tile
      integer, dimension(:), intent(inout) :: kipic
! updates pparti and cui, and possibly ncl, ihole, irc
      call wmdjpost1(pparti,cui,kipic,ncl,ihole,qmi,dth,ci,tdjpost,nx,mx&
     &,ipbc,relativity,plist,irc)
! add guard cells: updates cui
      call macguard1(cui,tguard,nx)
!
! reorder ions by tile with OpenMP:
! updates pparti, ppbuff, kipic, ncl, irc, and possibly ihole
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
      subroutine push_ions13(pparti,kipic)
! push ions with OpenMP
      implicit none
! pparti = tiled electron/ion particle arrays
      real, dimension(:,:,:), intent(inout) :: pparti
! kipic = number of electrons/ions in each tile
      integer, dimension(:), intent(inout) :: kipic
      wki = 0.0
! updates pparti and wki, and possibly ncl, ihole, irc
      if (mzf==0) then
         call wmbpush1(pparti,fxyze,byze,kipic,ncl,ihole,omx,qbmi,dt,dth&
     &,ci,wki,tpush,nx,mx,ipbc,relativity,plist,irc)
      else
         call wmpush1zf(pparti,kipic,ncl,ihole,dth,ci,wki,tpush,nx,mx,  &
     &ipbc,relativity,plist,irc)
      endif
      wki = wki*rmass
!
! reorder ions by tile with OpenMP:
! updates pparti, ppbuff, kipic, ncl, irc, and possibly ihole
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
      subroutine em_time_reverse1()
! start running simulation backwards
! need to advance maxwell field solver one step ahead
      implicit none
! deposit electron current: updates cue
      cue = 0.0
      call wmdjpost1(ppart,cue,kpic,ncl,ihole,qme,zero,ci,tdjpost,nx,mx,&
     &ipbc,relativity,plist,irc)
      call macguard1(cue,tguard,nx)
! deposit ion current: updates cui
      if (movion==1) then
         cui = 0.0
         call wmdjpost1(pparti,cui,kipic,ncl,ihole,qmi,zero,ci,tdjpost, &
     &nx,mx,ipbc,relativity,plist,irc)
         call macguard1(cui,tguard,nx)
         call maddcuei1(cue,cui,tfield,nx)
      endif
      isign = -1
      call mfft1rn(cue,isign,mixup,sct,tfft,indx)
! updates eyz, byz, wef, ws
      call mmaxwel1(eyz,byz,cue,ffc,ci,dt,wef,ws,tfield,nx)
! reverse time
      dt = -dt; dth = -dth
      end subroutine
!
!-----------------------------------------------------------------------
      subroutine init_energy_diag13()
! initialize energy diagnostic
      implicit none
! wt = energy time history array
      mtw = (nloop - 1)/ntw + 1; itw = 0
      allocate(wt(mtw,7),s(7))
      wt = 0.0; s = 0.0d0
      end subroutine
!
!-----------------------------------------------------------------------
      subroutine energy_diag13(wt,ntime,iuot)
! energy diagnostic
      implicit none
! wt = energy time history array
! ntime = current time step
! iuot = output file descriptor
      real, dimension(:,:), intent(inout) :: wt
      integer, intent(in) :: ntime, iuot
      wef = we + wf + wb
      ws = wef + wke + wki
      if (ntime==0) s(6) = ws
      write (iuot,*) 'Total Field, Kinetic and Total Energies:'
      if (movion==0) then
         write (iuot,'(3e14.7)') wef, wke, ws
      else
         write (iuot,'(4e14.7)') wef, wke, wki, ws
      endif
      write (iuot,*) 'Electric(l,t) and Magnetic Energies = '
      write (iuot,'(3e14.7)') we, wf, wb
      itw = itw + 1
! store energies in time history array
      wt(itw,:) = (/wef,wke,wki,ws,we,wf,wb/)
      s(1) = s(1) + we
      s(2) = s(2) + wke
      s(3) = s(3) + wf
      s(4) = s(4) + wb
      s(5) = s(5) + wki
      s(6) = min(s(6),dble(ws))
      s(7) = max(s(7),dble(ws))
      end subroutine
!
!-----------------------------------------------------------------------
      subroutine print_energy13(wt,iuot)
! print energy summaries
      implicit none
! wt = energy time history array
! iuot = output file descriptor
      real, dimension(:,:), intent(in) :: wt
! iuot = output file descriptor
      integer, intent(in) :: iuot
      s(6) = (s(7) - s(6))/wt(1,4)
      write (iuot,*) 'Energy Conservation = ', real(s(6))
      s(1) = s(1)/real(itw)
      write (iuot,*) 'Average Field Energy <WE> = ', real(s(1))
      s(2) = s(2)/real(itw)
      write (iuot,*) 'Average Electron Kinetic Energy <WKE> = ',        &
     &real(s(2))
      write (iuot,*) 'Ratio <WE>/<WKE>= ', real(s(1)/s(2))
      s(3) = s(3)/real(itw)
      write (iuot,*) 'Average Transverse EField Energy <WF> = ',        &
     &real(s(3))
      write (iuot,*) 'Ratio <WF>/<WKE>= ', real(s(3)/s(2))
      s(4) = s(4)/real(itw)
      write (iuot,*) 'Average Magnetic Field Energy <WB> = ', real(s(4))
      write (iuot,*) 'Ratio <WB>/<WKE>= ', real(s(4)/s(2))
      write (iuot,*)
      end subroutine
!
!-----------------------------------------------------------------------
      subroutine del_energy_diag13()
! delete energy diagnostic
      implicit none
! wt = energy time history array
      deallocate(wt,s)
      end subroutine
!
!-----------------------------------------------------------------------
      subroutine init_spectrum13()
! allocate scratch arrays for vector fields
      implicit none
      allocate(vfield(2,nxe),vfieldc(2,nxh))
! allocate and initialize high frequency array for spectral analysis
      if ((nta > 0).or.(ntet > 0).or.(ntar > 0)) then
         iwr = (wrmax - wrmin)/dwr + 1.5
         allocate(wmr(iwr))
         do it = 1, iwr
            wmr(it) = wrmin + dwr*real(it-1)
         enddo
      endif
      end subroutine
!
!-----------------------------------------------------------------------
      subroutine del_spectrum13()
! delete scratch arrays for vector fields
      implicit none
      deallocate(vfield,vfieldc)
      if ((nta > 0).or.(ntet > 0).or.(ntar > 0)) then
         deallocate(wmr)
      endif
      end subroutine
!
!-----------------------------------------------------------------------
      subroutine init_icurrent_diag13()
! initialize ion current density diagnostic
      implicit none
      fjiname = 'curik1.'//cdrun
      modesxji = min(modesxji,nxh+1)
      allocate(curit(2,modesxji))
! open file: updates njirec and possibly iuji
      if (njirec==0) then
         call dafopenvc1(curit,iuji,njirec,trim(fjiname))
      endif
! ion spectral analysis
      if ((ndji==2).or.(ndji==3)) then
         mtji = (nloop - 1)/ntji + 1; itji = 0
         allocate(vpkwji(2,modesxji,iwi,2))
         allocate(vpksji(2,4,modesxji,iwi),vwkji(2,modesxji,2))
         vpksji = 0.0d0
      endif
      end subroutine
!
!-----------------------------------------------------------------------
      subroutine icurrent_diag13(vfield,vpkwji,vwkji,ntime)
! ion current density diagnostic
      implicit none
! vfield = scratch array for vector field
      real, dimension(:,:), intent(inout) :: vfield
! vpkwji = power spectrum for ion current density
      real, dimension(:,:,:,:), intent(inout) :: vpkwji
! vwkji = maximum frequency as a function of k for ion current
      real, dimension(:,:,:), intent(inout) :: vwkji
! ntime = current time step
      integer, intent(in) :: ntime
      vfield = cui
! transform ion current density to fourier space: updates vfield
      isign = -1
      call mfft1rn(vfield,isign,mixup,sct,tfft,indx)
! calculate smoothed ion current in fourier space: updates vfieldc
      call msmooth13(vfield,vfieldc,ffc,tfield,nx)
! store selected fourier modes: updates curit
      call mrdvmodes1(vfieldc,curit,tfield,nx,modesxji)
! write diagnostic output: updates ndirec
      call dafwritevc1(curit,tdiag,iuji,njirec,modesxji)
! transform smoothed ion current to real space: updates vfield
      if ((ndji==1).or.(ndji==3)) then
         call mfft1crn(vfieldc,vfield,mixup,sct,tfft,indx)
         call mcguard1(vfield,tguard,nx)
      endif
! ion spectral analysis
      if ((ndji==2).or.(ndji==3)) then
         itji = itji + 1
         ts = dt*real(ntime)
         call mivcspect1(curit,wmi,vpkwji,vpksji,ts,t0,tdiag,mtji,iwi,  &
     &modesxji,nx,-1)
! performs frequency analysis of accumulated complex time series
         vwkji(1,:,1) = wmi(maxloc(vpkwji(1,:,:,1),dim=2))
         vwkji(2,:,1) = wmi(maxloc(vpkwji(2,:,:,1),dim=2))
         vwkji(1,:,2) = wmi(maxloc(vpkwji(1,:,:,2),dim=2))
         vwkji(2,:,2) = wmi(maxloc(vpkwji(2,:,:,2),dim=2))
      endif
      end subroutine
!
!-----------------------------------------------------------------------
      subroutine del_icurrent_diag13()
! delete ion current density diagnostic
      implicit none
      if (njirec > 0) then
         close(unit=iuji)
         njirec = njirec - 1
      endif
      deallocate(curit)
! spectral analysis
      if ((ndji==2).or.(ndji==3)) then
         deallocate(vpkwji,vwkji,vpksji)
      endif
      end subroutine
!
!-----------------------------------------------------------------------
      subroutine init_vrpotential_diag13()
! initialize radiative vector potential diagnostic
      implicit none
      farname = 'vpotrk1.'//cdrun
      modesxar = min(modesxar,nxh+1)
      allocate(vpotr(2,modesxar))
! open file: updates narrec and possibly iuar
      if (narrec==0) call dafopenvc1(vpotr,iuar,narrec,trim(farname))
      allocate(oldcu(2,nxe))
      oldcu = 0.0
! spectral analysis
      if ((ndar==2).or.(ndar==3)) then
         mtar = (nloop - 1)/ntar + 1; itar = 0
         allocate(vpkwr(2,modesxar,iwr,2),vpksr(2,4,modesxar,iwr))
         allocate(vwkr(2,modesxar,2))
         vpksr = 0.0d0
      endif
      end subroutine
!
!-----------------------------------------------------------------------
      subroutine vrpotential_diag13(vfield,vpkwr,vwkr,ntime)
! radiative vector potential diagnostic
      implicit none
! vfield = scratch array for vector field
      real, dimension(:,:), intent(inout) :: vfield
! vpkwr = power spectrum for radiative vector potential
      real, dimension(:,:,:,:), intent(inout) :: vpkwr
! vwkr = maximum frequency as a function of k for radiative vector
!        potential
      real, dimension(:,:,:), intent(inout) :: vwkr
! ntime = current time step
      integer, intent(in) :: ntime
! average current: updates vfieldc = 0.5*(cue + oldcu)
      call mcuave1(vfieldc,cue,oldcu,tfield,nx)
! calculate radiative vector potential in fourier space: updates vfieldc
! vfieldc should contain averaged current on entry
       call mavrpot1(vfieldc,byz,ffc,ci,tfield,nx)
! store selected fourier modes: updates vpotr
      call mrdvmodes1(vfieldc,vpotr,tfield,nx,modesxar)
! write diagnostic output: updates narrec
      call dafwritevc1(vpotr,tdiag,iuar,narrec,modesxar)
! transform radiative vector potential to real space: updates vfield
      if ((ndp==1).or.(ndp==3)) then
         call mfft1crn(vfieldc,vfield,mixup,sct,tfft,indx)
         call mcguard1(vfield,tguard,nx)
      endif
! spectral analysis
      if ((ndar==2).or.(ndar==3)) then
         itar = itar + 1
         ts = dt*real(ntime)
         call mivcspect1(vpotr,wmr,vpkwr,vpksr,ts,t0,tdiag,mtar,iwr,    &
     &modesxar,nx,1)
! performs frequency analysis of accumulated complex vector time series
         vwkr(1,:,1) = wmr(maxloc(vpkwr(1,:,:,1),dim=2))
         vwkr(2,:,1) = wmr(maxloc(vpkwr(2,:,:,1),dim=2))
         vwkr(1,:,2) = wmr(maxloc(vpkwr(1,:,:,2),dim=2))
         vwkr(2,:,2) = wmr(maxloc(vpkwr(2,:,:,2),dim=2))
      endif
      end subroutine
!
!-----------------------------------------------------------------------
      subroutine del_vrpotential_diag13()
! delete radiative vector potential diagnostic
      implicit none
      if (narrec > 0) then
         close(unit=iuar)
         narrec = narrec - 1
      endif
      deallocate(vpotr)
! spectral analysis
      if ((ndar==2).or.(ndar==3)) then
         deallocate(vpkwr,vwkr,vpksr)
      endif
      ceng = affp
      end subroutine
!
!-----------------------------------------------------------------------
      subroutine init_vpotential_diag13()
! initialize vector potential diagnostic
      implicit none
      faname = 'vpotk1.'//cdrun
      modesxa = min(modesxa,nxh+1)
      allocate(vpott(2,modesxa))
! open file: updates narec and possibly iua
      if (narec==0) call dafopenvc1(vpott,iua,narec,trim(faname))
! spectral analysis
      if ((nda==2).or.(nda==3)) then
         mta = (nloop - 1)/nta + 1; ita = 0
         allocate(vpkw(2,modesxa,iwr,2),vpks(2,4,modesxa,iwr))
         allocate(vwk(2,modesxa,2))
         vpks = 0.0d0
      endif
      end subroutine
!
!-----------------------------------------------------------------------
      subroutine vpotential_diag13(vfield,vpkw,vwk,ntime)
! vector potential diagnostic
      implicit none
! vfield = scratch array for vector field
      real, dimension(:,:), intent(inout) :: vfield
! vpkw = power spectrum for vector potential
      real, dimension(:,:,:,:), intent(inout) :: vpkw
! vwk = maximum frequency as a function of k for vector potential
      real, dimension(:,:,:), intent(inout) :: vwk
! ntime = current time step
      integer, intent(in) :: ntime
! calculate vector potential in fourier space: updates vfieldc
      call mavpot1(byz,vfieldc,tfield,nx)
! store selected fourier modes: updates vpott
      call mrdvmodes1(vfieldc,vpott,tfield,nx,modesxa)
! write diagnostic output: updates narec
      call dafwritevc1(vpott,tdiag,iua,narec,modesxa)
! transform vector potential to real space: updates vfield
      if ((nda==1).or.(nda==3)) then
         call mfft1crn(vfieldc,vfield,mixup,sct,tfft,indx)
         call mcguard1(vfield,tguard,nx)
      endif
! spectral analysis
      if ((nda==2).or.(nda==3)) then
         ita = ita + 1
         ts = dt*real(ntime)
         call mivcspect1(vpott,wmr,vpkw,vpks,ts,t0,tdiag,mta,iwr,modesxa&
     &,nx,1)
! performs frequency analysis of accumulated complex vector time series
         vwk(1,:,1) = wmr(maxloc(vpkw(1,:,:,1),dim=2))
         vwk(2,:,1) = wmr(maxloc(vpkw(2,:,:,1),dim=2))
         vwk(1,:,2) = wmr(maxloc(vpkw(1,:,:,2),dim=2))
         vwk(2,:,2) = wmr(maxloc(vpkw(2,:,:,2),dim=2))
      endif
      end subroutine
!
!-----------------------------------------------------------------------
      subroutine del_vpotential_diag13()
! delete vector potential diagnostic
      implicit none
      if (narec > 0) then
         close(unit=iua)
         narec = narec - 1
      endif
      deallocate(vpott)
! spectral analysis
      if ((nda==2).or.(nda==3)) then
         deallocate(vpkw,vwk,vpks)
      endif
      ceng = affp
      end subroutine
!
!-----------------------------------------------------------------------
      subroutine init_etfield_diag13()
! initialize transverse efield diagnostic
      implicit none
      fetname = 'etk1.'//cdrun
      modesxet = min(modesxet,nxh+1)
      allocate(ett(2,modesxet))
! open file: updates netrec and possibly iuet
      if (netrec==0) call dafopenvc1(ett,iuet,netrec,trim(fetname))
! spectral analysis
      if ((ndet==2).or.(ndet==3)) then
         mtet = (nloop - 1)/ntet + 1; itet = 0
         allocate(vpkwet(2,modesxet,iwr,2),vpkset(2,4,modesxet,iwr))
         allocate(vwket(2,modesxet,2))
         vpkset = 0.0d0
      endif
      end subroutine
!
!-----------------------------------------------------------------------
      subroutine etfield_diag13(vfield,vpkwet,vwket,ntime)
! transverse efield diagnostic
      implicit none
! vfield = scratch array for vector field
      real, dimension(:,:), intent(inout) :: vfield
! vpkwet = power spectrum for transverse efield
      real, dimension(:,:,:,:), intent(inout) :: vpkwet
! vwket = maximum frequency as a function of k for transverse efield
      real, dimension(:,:,:), intent(inout) :: vwket
! ntime = current time step
      integer, intent(in) :: ntime
! store selected fourier modes: updates ett
      call mrdvmodes1(eyz,ett,tfield,nx,modesxet)
! write diagnostic output: updates netrec
      call dafwritevc1(ett,tdiag,iuet,netrec,modesxet)
! transform transverse efield to real space: updates vfield
      if ((ndet==1).or.(ndet==3)) then
         call mfft1crn(eyz,vfield,mixup,sct,tfft,indx)
         call mcguard1(vfield,tguard,nx)
      endif
! spectral analysis
      if ((ndet==2).or.(ndet==3)) then
         itet = itet + 1
         ts = dt*real(ntime)
         call mivcspect1(ett,wmr,vpkwet,vpkset,ts,t0,tdiag,mtet,iwr,    &
     &modesxet,nx,0)
! performs frequency analysis of accumulated complex vector time series
         vwket(1,:,1) = wmr(maxloc(vpkwet(1,:,:,1),dim=2))
         vwket(2,:,1) = wmr(maxloc(vpkwet(2,:,:,1),dim=2))
         vwket(1,:,2) = wmr(maxloc(vpkwet(1,:,:,2),dim=2))
         vwket(2,:,2) = wmr(maxloc(vpkwet(2,:,:,2),dim=2))
      endif
      end subroutine
!
!-----------------------------------------------------------------------
      subroutine del_etfield_diag13()
! delete transverse efield diagnostic
      implicit none
      if (netrec > 0) then
         close(unit=iuet)
         netrec = netrec - 1
      endif
      deallocate(ett)
! spectral analysis
      if ((ndet==2).or.(ndet==3)) then
         deallocate(vpkwet,vwket,vpkset)
      endif
      ceng = affp
      end subroutine
!
!-----------------------------------------------------------------------
      subroutine init_bfield_diag13()
! initialize magnetic field diagnostic
      implicit none
      fbname = 'bk1.'//cdrun
      modesxb = min(modesxb,nxh+1)
      allocate(bt(2,modesxb))
! open file: updates nbrec and possibly iub
      if (netrec==0) call dafopenvc1(bt,iub,nbrec,trim(fbname))
      end subroutine
!
!-----------------------------------------------------------------------
      subroutine bfield_diag13(vfield)
! magnetic field diagnostic
      implicit none
! vfield = scratch array for vector field
      real, dimension(:,:), intent(inout) :: vfield
! store selected fourier modes: updates bt
      call mrdvmodes1(byz,bt,tfield,nx,modesxb)
! write diagnostic output: updates nbrec
      call dafwritevc1(bt,tdiag,iub,nbrec,modesxb)
! transform magnetic field to real space: updates vfield
      call mfft1crn(byz,vfield,mixup,sct,tfft,indx)
      call mcguard1(vfield,tguard,nx)
      end subroutine
!
!-----------------------------------------------------------------------
      subroutine del_bfield_diag13()
! delete magnetic field diagnostic
      implicit none
      if (nbrec > 0) then
         close(unit=iub)
         nbrec = nbrec - 1
      endif
      deallocate(bt)
      ceng = affp
      end subroutine
!
!-----------------------------------------------------------------------
      subroutine init_evelocity_diag13()
! initialize electron velocity diagnostic
      implicit none
      allocate(fv(2*nmv+2,ndim),sfv(2*nmv+2,ndim,mx1+1),fvm(ndim,3))
      mtv = (nloop - 1)/ntv + 1; itv = 0
      allocate(fvtm(mtv,ndim,3))
      ws = 2.0*max(4.0*vtx+abs(vx0),4.0*vtdx+abs(vdx))
      ws = max(ws,2.0*max(4.0*vty+abs(vy0),4.0*vtdy+abs(vdy)))
      ws = max(ws,2.0*max(4.0*vtz+abs(vz0),4.0*vtdx+abs(vdz)))
      sfv(1,1,:) = ws
      sfv(1,2,:) = ws
      sfv(1,3,:) = ws
      fvtm = 0.0
      end subroutine
!
!-----------------------------------------------------------------------
      subroutine evelocity_diag13(ppart,kpic,fv,fvm,fvtm)
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
      subroutine del_evelocity_diag13()
! delete electron velocity diagnostic
      implicit none
      deallocate(sfv)
      call del_evelocity_diag1()
      end subroutine
!
!-----------------------------------------------------------------------
      subroutine init_ivelocity_diag13()
! initialize ion velocity diagnostic
      implicit none
      allocate(fvi(2*nmv+2,ndim),sfvi(2*nmv+2,ndim,mx1+1),fvmi(ndim,3))
      allocate(fvtmi(mtv,ndim,3))
      ws = 2.0*max(4.0*vtxi+abs(vxi0),4.0*vtdxi+abs(vdxi))
      ws = max(ws,2.0*max(4.0*vtyi+abs(vyi0),4.0*vtdyi+abs(vdyi)))
      ws = max(ws,2.0*max(4.0*vtzi+abs(vzi0),4.0*vtdzi+abs(vdzi)))
      sfvi(1,1,:) = ws
      sfvi(1,2,:) = ws
      sfvi(1,3,:) = ws
      fvtmi = 0.0
      end subroutine
!
!-----------------------------------------------------------------------
      subroutine ivelocity_diag13(pparti,kipic,fvi,fvmi,fvtmi)
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
      call mvpdist1(pparti,kipic,sfvi,fvmi,tdiag,npi,nmv)
      fvi = sfvi(:,:,mx1+1)
! update time step if electrons have not been calculated
      if (ndv==2) itv = itv + 1
! store time history of ion vdrift, vth, and entropy
      fvtmi(itv,:,:) = fvmi
      end subroutine
!
!-----------------------------------------------------------------------
      subroutine del_ivelocity_diag13()
! delete ion velocity diagnostic
      implicit none
      call del_ivelocity_diag1()
      deallocate(sfvi)
      end subroutine
!
!-----------------------------------------------------------------------
      subroutine init_traj_diag13(ntime)
! initialize trajectory diagnostic
      implicit none
! ntime = current time step
      integer, intent(in) :: ntime
! set initial test trajectories
      if ((ntime+ntime0)==0) then
         allocate(iprobt(nprobt))
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
      endif
      if (nst==3) then
         allocate(fvtp(2*nmv+2,ndim),fvmtp(ndim,3))
         fvtp(1,:) = 2.0*max(4.0*vtx+abs(vx0),4.0*vtdx+abs(vdx))
      endif
      end subroutine
!
!-----------------------------------------------------------------------
      subroutine traj_diag13(ppart,kpic,partd,fvtp,fvmtp)
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
      subroutine print_timings13(tinit,tloop,iuot)
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
      write (iuot,*) 'current deposit time = ', tdjpost
      tdpost = tdpost + tdjpost
      write (iuot,*) 'total deposit time = ', tdpost
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
      subroutine close_diags13(iudm)
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
! radiative vector potential diagnostic
      if (ntar > 0) call del_vrpotential_diag13()
! vector potential diagnostic
      if (nta > 0) then
         call del_vpotential_diag13()
      endif
! transverse efield diagnostic
      if (ntet > 0) call del_etfield_diag13()
! magnetic field diagnostic
      if (ntb > 0) call del_bfield_diag13()
! ion diagnostics
      if (movion==1) then
! ion density diagnostic
         if (ntdi > 0) call del_idensity_diag1()
! ion current diagnostic
         if (ntji > 0) call del_icurrent_diag13()
      endif
! write final diagnostic metafile
      call writnml1(iudm)
      close(unit=iudm)
! deallocate arrays
      call del_fields13()
      call del_electrons1()
      if (movion==1) call del_ions1()
      deallocate(part,ppbuff,ncl,ihole)
      if ((ntde > 0).or.(ntp > 0).or.(ntel > 0).or.(ntdi > 0)) then
         call del_spectrum1()
      endif
      if ((nta>0).or.(ntet>0).or.(ntb>0).or.(ntar>0).or.(ntji>0)) then
         call del_spectrum13()
      endif
      if (ntw > 0) call del_energy_diag13()
      if (ntv > 0) then
         call del_evelocity_diag1()
         deallocate(sfv)
         if (movion==1) then
            call del_ivelocity_diag1()
            deallocate(sfvi)
         endif
      endif
      if (ntt > 0) call del_traj_diag1()
      end subroutine
!
!-----------------------------------------------------------------------
      subroutine initialize_diagnostics13(ntime)
! initialize all diagnostics from namelist input parameters
      implicit none
! ntime = current time step
      integer, intent(in) :: ntime
! initialize energy diagnostic: updates wt
      if (ntw > 0) then
         call init_energy_diag13()
      endif
!
! allocate and initialize scratch arrays for scalar fields:
! allocates sfield
      if ((ntde > 0).or.(ntp > 0).or.(ntel > 0).or.(ntdi > 0)) then
         call init_spectrum1()
      endif
!
! allocate and initialize scratch arrays for vector fields:
! allocates vfield
      if ((nta>0).or.(ntet>0).or.(ntb>0).or.(ntar>0).or.(ntji>0)) then
         call init_spectrum13()
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
! initialize ion current density diagnostic: allocates vpkwji, vwkji
      if (movion==1) then
         if (ntji > 0) then
            call init_icurrent_diag13()
         endif
      endif
!
! initialize radiative vector potential diagnostic:
! allocates vpkwr, vwkr, oldcu
      if (ntar > 0) then
         call init_vrpotential_diag13()
      endif
!
! initialize vector potential diagnostic: allocates vpkw, vwk
      if (nta > 0) then
         call init_vpotential_diag13()
      endif
!
! initialize transverse efield diagnostic: allocates vpkwet, vwket
      if (ntet > 0) then
         call init_etfield_diag13()
      endif
!
! initialize magnetic field diagnostic
      if (ntb > 0) then
         call init_bfield_diag13()
      endif
!
! initialize velocity diagnostic
      if (ntv > 0) then
! electrons: allocates fv, fvm, fvtm
         call init_evelocity_diag13()
! ions: allocates fvi, fvmi, fvtmi
         if (movion==1) then
            call init_ivelocity_diag13()
         endif
      endif
!
! initialize trajectory diagnostic: allocates partd, fvtp, fvmtp
      if (ntt > 0) then
         call init_traj_diag13(ntime)
      endif
      end subroutine
!
!-----------------------------------------------------------------------
      subroutine bwrite_restart13(iur,ntime)
! write out basic restart file for electromagnetic code
      implicit none
! iur = restart file descriptor
! ntime = current time step
      integer, intent(in) :: iur, ntime
! local data
      integer :: i, j
      integer :: nxvh
! write out particles and electrostatic fields
      call bwrite_restart1(iur,ntime)
! write out electromagnetic fields
      nxvh = size(eyz,2)
      write (iur) nxvh
      if (nxvh > 0) then
         write (iur) ((eyz(i,j),i=1,2),j=1,nxvh)
         write (iur) ((byz(i,j),i=1,2),j=1,nxvh)
      endif
      end subroutine
!
!-----------------------------------------------------------------------
      subroutine bread_restart13(iur)
! read in basic restart file for electromagnetic code
      implicit none
! iur = restart file descriptor
      integer, intent(in) :: iur
! local data
      integer :: i, j
      integer :: ios, it
! read in particles and electrostatic fields
      call bread_restart1(iur)
! allocate ion current
      if (.not.allocated(cui)) allocate(cui(2,nxe))
      cui = 0.0
! read in electromagnetic fields
      read (iur,iostat=ios) it
      if (ios /= 0) then
         write (*,*) 'eyz/byz size restart error, ios = ', ios
         stop
      endif
      if (it > size(eyz,2)) then
         write (*,*) 'eyz/byz restart error, size(eyz)=',it,size(eyz,2)
         stop
      endif
      if (it > 0) then
         read (iur,iostat=ios) ((eyz(i,j),i=1,2),j=1,it)
         if (ios /= 0) then
            write (*,*) 'eyz read error, ios = ', ios
            stop
         endif
         read (iur,iostat=ios) ((byz(i,j),i=1,2),j=1,it)
         if (ios /= 0) then
            write (*,*) 'byz read error, ios = ', ios
            stop
         endif
      endif
      end subroutine
!
!-----------------------------------------------------------------------
      subroutine dwrite_restart13(iur)
! write out restart diagnostic file for electromagnetic code
      implicit none
! iur = restart file descriptor
      integer, intent(in) :: iur
! local data
      integer :: i, j, k, l, it, is, ios
      character(len=32) :: fname
! write out restart diagnostic file for electrostatic code
      call dwrite_restart1(iur)
!
! write out radiative vector potential diagnostic parameter
      write (iur) ntar
! write out record location
      if (ntar > 0) then
         write (iur) narrec
! write out record length (zero if error) and file name (if no error)
         if (narrec > 0) then
            inquire(file=farname,recl=it,iostat=ios)
            if (ios /= 0) it = 0
            write (iur) it
            if (it > 0) then
               fname = farname
               write (iur) fname
            endif
         endif
! write out current density
         it = size(cue,2)
         write (iur) it
         write (iur) ((cue(i,j),i=1,2),j=1,it)
! write out spectrum flag
         if ((ndar==2).or.(ndar==3)) then
            write (iur) itar
! write out spectrum sizes and data
            if (itar > 0) then
               it = size(vpksr,3); is = size(vpksr,4)
               write (iur) size(vpksr,1), size(vpksr,2), it, is
               write (iur) ((((vpksr(i,j,k,l),i=1,2),j=1,4),k=1,it),    &
     &l=1,is)
            endif
         else
            it = 0
            write (iur) it
         endif
      endif
!
! write out vector potential diagnostic parameter
      write (iur) nta
! write out record location
      if (nta > 0) then
         write (iur) narec
! write out record length (zero if error) and file name (if no error)
         if (narec > 0) then
            inquire(file=faname,recl=it,iostat=ios)
            if (ios /= 0) it = 0
            write (iur) it
            if (it > 0) then
               fname = faname
               write (iur) fname
            endif
         endif
! write out spectrum flag
         if ((nda==2).or.(nda==3)) then
            write (iur) ita
! write out spectrum sizes and data
            if (ita > 0) then
               it = size(vpks,3); is = size(vpks,4)
               write (iur) size(vpks,1), size(vpks,2), it, is
               write (iur) ((((vpks(i,j,k,l),i=1,2),j=1,4),k=1,it),     &
     &l=1,is)
            endif
         else
            it = 0
            write (iur) it
         endif
      endif
!
! write out transverse efield diagnostic parameter
      write (iur) ntet
! write out record location
      if (ntet > 0) then
         write (iur) netrec
! write out record length (zero if error) and file name (if no error)
         if (netrec > 0) then
            inquire(file=fetname,recl=it,iostat=ios)
            if (ios /= 0) it = 0
            write (iur) it
            if (it > 0) then
               fname = fetname
               write (iur) fname
            endif
         endif
! write out spectrum flag
         if ((ndet==2).or.(ndet==3)) then
            write (iur) itet
! write out spectrum sizes and data
            if (itet > 0) then
               it = size(vpkset,3); is = size(vpkset,4)
               write (iur) size(vpkset,1), size(vpkset,2), it, is
               write (iur) ((((vpkset(i,j,k,l),i=1,2),j=1,4),k=1,it),   &
     &l=1,is)
            endif
         else
            it = 0
            write (iur) it
         endif
      endif
!
! write out magnetic field diagnostic diagnostic parameter
      write (iur) ntb
! write out record location
      if (ntb > 0) then
         write (iur) nbrec
! write out record length (zero if error) and file name (if no error)
         if (nbrec > 0) then
            inquire(file=fbname,recl=it,iostat=ios)
            if (ios /= 0) it = 0
            write (iur) it
            if (it > 0) then
               fname = fbname
               write (iur) fname
            endif
         endif
      endif
!
! write out ion current density diagnostic parameter
      if (movion==1) then
         write (iur) ntji
! write out record location
         if (ntji > 0) then
            write (iur) njirec
! write out record length (zero if error) and file name (if no error)
            if (njirec > 0) then
               inquire(file=fjiname,recl=it,iostat=ios)
               if (ios /= 0) it = 0
               write (iur) it
               if (it > 0) then
                  fname = fjiname
                  write (iur) fname
               endif
            endif
! write out spectrum flag
            if ((ndji==2).or.(ndji==3)) then
               write (iur) itji
! write out spectrum sizes and data
               if (itji > 0) then
                  it = size(vpksji,3); is = size(vpksji,4)
                  write (iur) size(vpksji,1), size(vpksji,2), it, is
                  write (iur) ((((vpksji(i,j,k,l),i=1,2),j=1,4),k=1,it),&
     &l=1,is)
               endif
            else
               it = 0
               write (iur) it
            endif
         endif
      endif
      end subroutine
!
!-----------------------------------------------------------------------
      subroutine dread_restart13(iur)
! read in restart diagnostic file for electromagnetic code
      implicit none
! iur = restart file descriptor
      integer, intent(in) :: iur
! local data
      integer :: i, j, k, l, it, is, ir, iq, ios
      character(len=32) :: fname
! read in restart diagnostic file for electrostatic code
      call dread_restart1(iur)
! restore energy accumulations
      if (ntw > 0) then
         if (itw > 0) then
            s(1) = wt(1,5)
            s(2) = wt(1,2)
            s(3) = wt(1,6)
            s(4) = wt(1,7)
            s(5) = wt(1,3)
            s(6) = dble(wt(1,4))
            s(7) = s(6)
            do it = 2, itw
               s(1) = s(1) + wt(it,5)
               s(2) = s(2) + wt(it,2)
               s(3) = s(3) + wt(it,6)
               s(4) = s(4) + wt(it,7)
               s(5) = s(5) + wt(it,3)
               s(6) = min(s(6),dble(wt(it,4)))
               s(7) = max(s(7),dble(wt(it,4)))
            enddo
         endif
      endif
!
! read in radiative vector potential diagnostic parameter
      read (iur,iostat=ios) it
      if (ios /= 0) then
         write (*,*) 'ntar restart error, ios = ', ios
         stop
      endif
      if (it /= ntar) then
         write (*,*) 'restart error: read/expected ntar=', it, ntar
         stop
      endif
! read in record location
      if (ntar > 0) then
         read (iur,iostat=ios) narrec
         if (ios /= 0) then
            write (*,*) 'narrec restart error, ios = ', ios
            stop
         endif
! read in record length (zero if error) and file name (if no error)
         if (narrec > 0) then
            read (iur,iostat=ios) it
            if (ios /= 0) then
               write (*,*) 'ntar record length error, ios = ', ios
               stop
            endif
            if (it==0) then
               write (*,*) 'ntar zero length record error'
               stop
            endif
            read (iur,iostat=ios) fname
            if (ios /= 0) then
               write (*,*) 'ntar file name error, ios = ', ios
               stop
            endif
            farname = fname
         endif
! read in current density
         read (iur,iostat=ios) it
         if (ios /= 0) then
            write (*,*) 'cue array size error, ios = ', ios
            stop
         endif
         if (it > size(cue,2)) then
            write (*,*) 'cue size error: read/expected=', it,size(cue,2)
            stop
         endif
         read (iur,iostat=ios) ((cue(i,j),i=1,2),j=1,it)
         if (ios /= 0) then
            write (*,*) 'cue array read error, ios = ', ios
            stop
         endif
! read in spectrum flag
         read (iur,iostat=ios) itar
         if (ios /= 0) then
            write (*,*) 'itar restart error, ios = ', ios
            stop
         endif
! read in spectrum sizes and data
         if (itar > 0) then
            read (iur,iostat=ios) iq, ir, it, is
            if (ios /= 0) then
               write (*,*) 'ntar restart array size error, ios = ', ios
               stop
            endif
            if (iq /= 2) then
               write (*,*) 'vpksr size error: read/expected 2 =', iq
               stop
            endif
            if (ir /= 4) then
               write (*,*) 'vpksr size error: read/expected 4 =', ir
               stop
            endif
            if (it /= modesxar) then
               write (*,*) 'vpksr size error: read/expected modesxar=', &
     &it, modesxar
               stop
            endif
            if (is /= iwr) then
               write (*,*) 'vpksr size error: read/expected iwr=',is,iwr
               stop
            endif
            read (iur,iostat=ios) ((((vpksr(i,j,k,l),i=1,2),j=1,4),     &
     &k=1,it),l=1,is)
            if (ios /= 0) then
               write (*,*) 'vpksr array read error, ios = ', ios
               stop
            endif
         endif
      endif
!
! read in vector potential diagnostic parameter
      read (iur,iostat=ios) it
      if (ios /= 0) then
         write (*,*) 'nta restart error, ios = ', ios
         stop
      endif
      if (it /= nta) then
         write (*,*) 'restart error: read/expected nta=', it, nta
         stop
      endif
! read in record location
      if (nta > 0) then
         read (iur,iostat=ios) narec
         if (ios /= 0) then
            write (*,*) 'narec restart error, ios = ', ios
            stop
         endif
! read in record length (zero if error) and file name (if no error)
         if (narec > 0) then
            read (iur,iostat=ios) it
            if (ios /= 0) then
               write (*,*) 'nta record length error, ios = ', ios
               stop
            endif
            if (it==0) then
               write (*,*) 'nta zero length record error'
               stop
            endif
            read (iur,iostat=ios) fname
            if (ios /= 0) then
               write (*,*) 'nta file name error, ios = ', ios
               stop
            endif
            faname = fname
         endif
! read in spectrum flag
         read (iur,iostat=ios) ita
         if (ios /= 0) then
            write (*,*) 'ita restart error, ios = ', ios
            stop
         endif
! read in spectrum sizes and data
         if (ita > 0) then
            read (iur,iostat=ios) iq, ir, it, is
            if (ios /= 0) then
               write (*,*) 'nta restart array size error, ios=', ios
               stop
            endif
            if (iq /= 2) then
               write (*,*) 'vpks size error: read/expected 2 =', iq
               stop
            endif
            if (ir /= 4) then
               write (*,*) 'vpks size error: read/expected 4 =', ir
               stop
            endif
            if (it /= modesxa) then
               write (*,*) 'vpks size error: read/expected modesxa=',it,&
     &modesxa
                  stop
            endif
            if (is /= iwr) then
               write (*,*) 'vpks size error: read/expected iwr=',is,iwr
               stop
            endif
            read (iur,iostat=ios) ((((vpks(i,j,k,l),i=1,2),j=1,4),      &
     &k=1,it),l=1,is)
            if (ios /= 0) then
               write (*,*) 'vpks array read error, ios = ', ios
               stop
            endif
         endif
      endif
!
! read in transverse efield diagnostic parameter
      read (iur,iostat=ios) it
      if (ios /= 0) then
         write (*,*) 'ntet restart error, ios = ', ios
         stop
      endif
      if (it /= ntet) then
         write (*,*) 'restart error: read/expected ntet=', it, ntet
         stop
      endif
! read in record location
      if (ntet > 0) then
         read (iur,iostat=ios) netrec
         if (ios /= 0) then
            write (*,*) 'netrec restart error, ios = ', ios
            stop
         endif
! read in record length (zero if error) and file name (if no error)
         if (netrec > 0) then
            read (iur,iostat=ios) it
            if (ios /= 0) then
               write (*,*) 'ntet record length error, ios = ', ios
               stop
            endif
            if (it==0) then
               write (*,*) 'ntet zero length record error'
               stop
            endif
            read (iur,iostat=ios) fname
            if (ios /= 0) then
               write (*,*) 'ntet file name error, ios = ', ios
               stop
            endif
            fetname = fname
         endif
! read in spectrum flag
         read (iur,iostat=ios) itet
         if (ios /= 0) then
            write (*,*) 'itet restart error, ios = ', ios
            stop
         endif
! read in spectrum sizes and data
         if (itet > 0) then
            read (iur,iostat=ios) iq, ir, it, is
            if (ios /= 0) then
               write (*,*) 'ntet restart array size error, ios = ', ios
               stop
            endif
            if (iq /= 2) then
               write (*,*) 'vpkset size error: read/expected 2 =', iq
               stop
            endif
            if (ir /= 4) then
               write (*,*) 'vpkset size error: read/expected 4 =', ir
               stop
            endif
            if (it /= modesxet) then
               write (*,*) 'vpkset size error: read/expected modesxet=',&
     &it, modesxet
               stop
            endif
            if (is /= iwr) then
               write (*,*) 'vpkset size error: read/expected iwr=',is,  &
     &iwr
               stop
            endif
            read (iur,iostat=ios) ((((vpkset(i,j,k,l),i=1,2),j=1,4),    &
     &k=1,it),l=1,is)
            if (ios /= 0) then
               write (*,*) 'vpkset array read error, ios = ', ios
               stop
            endif
         endif
      endif
!
! read in magnetic field diagnostic parameter
      read (iur,iostat=ios) it
      if (ios /= 0) then
         write (*,*) 'ntb restart error, ios = ', ios
         stop
      endif
      if (it /= ntb) then
         write (*,*) 'restart error: read/expected nteb=', it, ntb
         stop
      endif
! read in record location
      if (ntb > 0) then
         read (iur,iostat=ios) nbrec
         if (ios /= 0) then
            write (*,*) 'nbrec restart error, ios = ', ios
            stop
         endif
! read in record length (zero if error) and file name (if no error)
         if (nbrec > 0) then
            read (iur,iostat=ios) it
            if (ios /= 0) then
               write (*,*) 'ntb record length error, ios = ', ios
               stop
            endif
            if (it==0) then
               write (*,*) 'ntb zero length record error'
               stop
            endif
            read (iur,iostat=ios) fname
            if (ios /= 0) then
               write (*,*) 'ntb file name error, ios = ', ios
               stop
            endif
            fbname = fname
         endif
      endif
!
! read in ion current density diagnostic parameter
      if (movion==1) then
         read (iur,iostat=ios) it
         if (ios /= 0) then
            write (*,*) 'ntji restart error, ios = ', ios
            stop
         endif
         if (it /= ntji) then
            write (*,*) 'restart error: read/expected ntji=', it, ntji
            stop
         endif
! read in record location
         if (ntji > 0) then
            read (iur,iostat=ios) njirec
            if (ios /= 0) then
               write (*,*) 'njirec restart error, ios = ', ios
               stop
            endif
! read in record length (zero if error) and file name (if no error)
            if (njirec > 0) then
               read (iur,iostat=ios) it
               if (ios /= 0) then
                  write (*,*) 'ntji record length error, ios = ', ios
                  stop
               endif
               if (it==0) then
                  write (*,*) 'ntji zero length record error'
                  stop
               endif
               read (iur,iostat=ios) fname
               if (ios /= 0) then
                  write (*,*) 'ntji file name error, ios = ', ios
                  stop
               endif
               fjiname = fname
            endif
! read in spectrum flag
            read (iur,iostat=ios) itji
            if (ios /= 0) then
               write (*,*) 'itji restart error, ios = ', ios
               stop
            endif
! read in spectrum sizes and data
            if (itji > 0) then
               read (iur,iostat=ios) iq, ir, it, is
               if (ios /= 0) then
                  write (*,*) 'ntji restart array size error, ios=', ios
                  stop
               endif
               if (iq /= 2) then
                  write (*,*) 'vpksji size error: read/expected 2 =', iq
                  stop
               endif
               if (ir /= 4) then
                  write (*,*) 'vpksji size error: read/expected 4 =', ir
                  stop
               endif
               if (it /= modesxji) then
                  write (*,*) 'vpksji size error: read/expected modesxji&
     &=',it,modesxji
                  stop
               endif
               if (is /= iwi) then
                   write (*,*) 'vpksji size error: read/expected iwi=', &
     &is,iwi
                  stop
               endif
               read (iur,iostat=ios) ((((vpksji(i,j,k,l),i=1,2),j=1,4), &
     &k=1,it),l=1,is)
               if (ios /= 0) then
                   write (*,*) 'vpksji array read error, ios = ', ios
                  stop
               endif
            endif
         endif
      endif
      end subroutine
!
!-----------------------------------------------------------------------
      subroutine reset_diags13()
! reset electrostatic/electromagnetic diagnostics
      implicit none
! reset electrostatic diagnostics
      call reset_diags1()
      if (ntw > 0) s = 0.0d0
! reset electromagnetic diagnostics
      if (ntar > 0) then
         if (narrec > 1) narrec = 1
         if ((ndar==2).or.(ndar==3)) then
            itar = 0; vpksr = 0.0d0
         endif
      endif
      if (nta > 0) then
         if (narec > 1) narec = 1
         if ((nda==2).or.(nda==3)) then
            ita = 0; vpks = 0.0d0
         endif
      endif
      if (ntet > 0) then
         if (netrec > 1) netrec = 1
         if ((ndet==2).or.(ndet==3)) then
            itet = 0; vpkset = 0.0d0
         endif
      endif
      if (ntb > 0) then
         if (nbrec > 1) nbrec = 1
      endif
      if (movion==1) then
         if (ntji > 0) then
            if (njirec > 1) njirec = 1
            if ((ndji==2).or.(ndji==3)) then
               itji = 0; vpksji = 0.0d0
            endif
         endif
      endif
      end subroutine
!
      end module
