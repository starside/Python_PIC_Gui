!-----------------------------------------------------------------------
!
      module mdiag1
!
! Fortran90 wrappers to 1d OpenMP PIC library libmdiag1.f
! get_funit returns an unconnected fortran unit number
! fnrecl find record length of direct access file
! dafopenc1 opens new binary file for complex 1d scalar data.
! dafopenvc1 opens new binary file for complex 1d vector data.
! dafwritec1 writes scalar record in direct access binary file
! dafwritevc1 writes vector record in direct access binary file
! mcspect1 performs frequency analysis of complex time series
!          calls CSPECT1
! micspect1 performs incremental frequency analysis of complex scalar
!           time series for one time step
!           calls ICSPECT1
! mivcspect1 performs incremental frequency analysis of complex vector 
!            time series for one time step
!            calls IVCSPECT1
! mvpdist1 calculates 1 component velocity distribution, velocity
!          moments, and entropy with segmented particle array
!          calls VPDIST1 or VPDIST13
! merpdist1 calculates 1d energy distribution for relativistic particles
!           with segmented particle array
!           calls ERPDIST1
! mvdist1 calculates 1 component velocity distribution, velocity
!         moments, and entropy with standard particle array
!         calls VPDIST1 or VDIST13
! settraj1 sets test charge distribution by setting a particle id in
!          particle location 3 or 5
!          calls STPTRAJ1 or STPTRAJ13
! mfnptraj1 finds tagged particles in ppart
!           calls FNPTRAJ1 or FNPTRAJ13
! mptraj1 copies tagged particles in ppart to array partt
!         calls PTRAJ1 or PTRAJ13
! setmbeam1 marks beam particles by setting a particle id in particle
!           location 3 or 5
!           calls STPBEAM1 or STPBEAM13
! written by viktor k. decyk, ucla
! copyright 2016, regents of the university of california
! update: may 30, 2017
!
      use libmdiag1_h
      implicit none
!
      contains
!
!-----------------------------------------------------------------------
      function get_funit(start) result(funit)
! this function returns an unconnected fortran unit number,
! starting with unit = start.  returns -1 if none found
      integer, intent(in) :: start
      integer :: funit
! local data
      integer :: i
      logical :: connected
      funit = -1
! check connection status
      do i = start, 99
         inquire(unit=i,opened=connected)
         if (.not.connected) then
            funit = i
            exit
         endif
      enddo
      end function
!
!-----------------------------------------------------------------------
      function fnrecl(fname) result(it)
! find record length of direct access file
      character(len=*), intent(in) :: fname
      integer :: it, ios
      inquire(file=fname,recl=it,iostat=ios)
      if (ios /= 0) it = 0
      end function
!
!-----------------------------------------------------------------------
      subroutine dafopenc1(fc,iunit,nrec,fname)
! this subroutine opens new direct access binary file
! fc = scalar data array to be written in each record
! iunit = fortran unit number to be used 
! nrec = returns initial record number
! fname = file name
      implicit none
      integer, intent(inout) :: iunit, nrec
      complex, dimension(:), intent(in) :: fc
      character(len=*), intent(in) :: fname
! local data
      integer :: lrec
      inquire(iolength=lrec) fc(1); lrec = lrec*size(fc)
      iunit = get_funit(iunit)
      open(unit=iunit,file=fname,form='unformatted',access='direct',    &
     &recl=lrec,status='replace')
      nrec = 1
      end subroutine
!
!-----------------------------------------------------------------------
      subroutine dafopenvc1(fc,iunit,nrec,fname)
! this subroutine opens new direct access binary file
! fc = vector data array to be written in each record
! iunit = fortran unit number to be used 
! nrec = returns initial record number
! fname = file name
      implicit none
      integer, intent(inout) :: iunit, nrec
      complex, dimension(:,:), intent(in) :: fc
      character(len=*), intent(in) :: fname
! local data
      integer :: lrec
      inquire(iolength=lrec) fc(1,1); lrec = lrec*size(fc)
      iunit = get_funit(iunit)
      open(unit=iunit,file=fname,form='unformatted',access='direct',    &
     &recl=lrec,status='replace')
      nrec = 1
      end subroutine
!
!-----------------------------------------------------------------------
      subroutine dafwritec1(fc,tdiag,iunit,nrec,nx)
! this subroutine writes scalar record in direct access binary file
! fc = scalar data array to be written
! iunit = fortran unit number to be used 
! nrec = record number for write (then updated to next record)
! nx = number of elements to be written in record
      implicit none
      integer, intent(in) :: iunit, nx
      integer, intent(inout) :: nrec
      real, intent(inout) :: tdiag
      complex, dimension(:), intent(in) :: fc
! local data
      integer :: j
      integer, dimension(4) :: itime
      double precision :: dtime
      if (nrec < 1) return
      call dtimer(dtime,itime,-1)
      write (unit=iunit,rec=nrec) (fc(j),j=1,nx)
      nrec = nrec + 1
      call dtimer(dtime,itime,1)
      tdiag = tdiag + real(dtime)
      end subroutine
!
!-----------------------------------------------------------------------
      subroutine dafwritevc1(fc,tdiag,iunit,nrec,nx)
! this subroutine writes vector record in direct access binary file
! fc = vector data array to be written
! iunit = fortran unit number to be used 
! nrec = record number for write (then updated to next record)
! nx = number of elements to be written in record
      implicit none
      integer, intent(in) :: iunit, nx
      integer, intent(inout) :: nrec
      real, intent(inout) :: tdiag
      complex, dimension(:,:), intent(in) :: fc
! local data
      integer :: j, k, ndim
      integer, dimension(4) :: itime
      double precision :: dtime
      ndim = size(fc,1)
      if (nrec < 1) return
      call dtimer(dtime,itime,-1)
      write (unit=iunit,rec=nrec) ((fc(j,k),j=1,ndim),k=1,nx)
      nrec = nrec + 1
      call dtimer(dtime,itime,1)
      tdiag = tdiag + real(dtime)
      end subroutine
!
!-----------------------------------------------------------------------
      subroutine mcspect1(fc,wm,pkw,t0,dt,tdiag,nt,iw,modesx)
! performs incremental frequency analysis of complex time series
      integer, intent(in) :: nt, iw, modesx
      real, intent(in) :: t0, dt
      real, intent(inout) :: tdiag
      complex, dimension(:,:), intent(in) :: fc
      real, dimension(:), intent(in) :: wm
      real, dimension(:,:,:), intent(inout) :: pkw
! local data
      integer :: ntd, iwd, modesxd
      integer, dimension(4) :: itime
      double precision :: dtime
! extract dimensions
      ntd = size(fc,1); modesxd = size(fc,2)
      iwd = size(wm,1)
! initialize timer
      call dtimer(dtime,itime,-1)
! call low level procedure
      call CSPECT1(fc,wm,pkw,t0,dt,nt,iw,modesx,ntd,iwd,modesxd)
! record time
      call dtimer(dtime,itime,1)
      tdiag = tdiag + real(dtime)
      end subroutine
!
!-----------------------------------------------------------------------
      subroutine micspect1(fc,wm,pkw,pks,time,t0,tdiag,nt,iw,modesx,nx, &
     &norm)
! performs incremental frequency analysis of complex scalar time series
! for one time step
! norm = (-1,0,1) = normalize with (inverse gradient,null,gradient) op
      integer, intent(in) :: nt, iw, modesx, nx, norm
      real, intent(in) :: time, t0
      real, intent(inout) :: tdiag
      complex, dimension(:), intent(in) :: fc
      real, dimension(:), intent(in) :: wm
      real, dimension(:,:,:), intent(inout) :: pkw
      double precision, dimension(:,:,:), intent(inout) :: pks
! local data
      integer :: iwd, modesxd
      integer, dimension(4) :: itime
      double precision :: dtime
! extract dimensions
      modesxd = size(fc,1); iwd = size(wm,1)
! initialize timer
      call dtimer(dtime,itime,-1)
! call low level procedure
      call ICSPECT1(fc,wm,pkw,pks,time,t0,nt,iw,modesx,nx,norm,iwd,     &
     &modesxd)
      call dtimer(dtime,itime,1)
      tdiag = tdiag + real(dtime)
      end subroutine
!
!-----------------------------------------------------------------------
      subroutine mivcspect1(fvc,wm,vpkw,vpks,time,t0,tdiag,nt,iw,modesx,&
     &nx,norm)
! performs incremental frequency analysis of complex vector time series
! for one time step
! norm = (-1,0,1) = normalize with (inverse curl,null,curl) op
      integer, intent(in) :: nt, iw, modesx, nx, norm
      real, intent(in) :: time, t0
      real, intent(inout) :: tdiag
      complex, dimension(:,:), intent(in) :: fvc
      real, dimension(:), intent(in) :: wm
      real, dimension(:,:,:,:), intent(inout) :: vpkw
      double precision, dimension(:,:,:,:), intent(inout) :: vpks
! local data
      integer :: iwd, modesxd
      integer, dimension(4) :: itime
      double precision :: dtime
! extract dimensions
      modesxd = size(fvc,2); iwd = size(wm,1)
! initialize timer
      call dtimer(dtime,itime,-1)
! call low level procedure
      call IVCSPECT1(fvc,wm,vpkw,vpks,time,t0,nt,iw,modesx,nx,norm,iwd, &
     &modesxd)
      call dtimer(dtime,itime,1)
      tdiag = tdiag + real(dtime)
      end subroutine
!
!-----------------------------------------------------------------------
      subroutine mvpdist1(ppart,kpic,sfv,fvm,tdiag,np,nmv)
! calculates 1d velocity distribution, velocity moments, and entropy
! with segmented particle array
      integer, intent(in) :: np, nmv
      real, intent(inout) :: tdiag
      real, dimension(:,:,:), intent(in) :: ppart
      integer, dimension(:), intent(in) :: kpic
      real, dimension(:,:,:), intent(inout) :: sfv
      real, dimension(:,:), intent(inout) :: fvm
! local data
      integer :: idimp, nppmx, nmvf, idimv, mx1
      integer, dimension(4) :: itime
      double precision :: dtime
! extract dimensions
      idimp = size(ppart,1); nppmx = size(ppart,2)
      nmvf = size(sfv,1); idimv = size(sfv,2)
      mx1 = size(kpic,1)
! initialize timer
      call dtimer(dtime,itime,-1)
      if (idimv==1) then
         call VPDIST1(ppart,kpic,sfv,fvm,idimp,nppmx,mx1,np,nmv,nmvf)
      else if (idimv==3) then
         call VPDIST13(ppart,kpic,sfv,fvm,idimp,nppmx,mx1,np,nmv,nmvf)
      endif
! record time
      call dtimer(dtime,itime,1)
      tdiag = tdiag + real(dtime)
      end subroutine
!
!-----------------------------------------------------------------------
      subroutine merpdist1(ppart,kpic,sfv,ci,wk,tdiag,nmv)
! calculates 1d energy distribution for relativistic particles
! with segmented particle array
      integer, intent(in) :: nmv
      real, intent(in) :: ci
      real, intent(inout) :: wk, tdiag
      real, dimension(:,:,:), intent(in) :: ppart
      integer, dimension(:), intent(in) :: kpic
      real, dimension(:,:,:), intent(inout) :: sfv
! local data
      integer :: idimp, nppmx, nmvf, idimv, mx1
      integer, dimension(4) :: itime
      double precision :: dtime
! extract dimensions
      idimp = size(ppart,1); nppmx = size(ppart,2)
      nmvf = size(sfv,1); idimv = size(sfv,2)
      mx1 = size(kpic,1)
! initialize timer
      call dtimer(dtime,itime,-1)
      if (idimv==1) then
         call ERPDIST1(ppart,kpic,sfv,ci,wk,idimp,nppmx,mx1,nmv,nmvf)
      else if (idimv==3) then
         call ERPDIST13(ppart,kpic,sfv,ci,wk,idimp,nppmx,mx1,nmv,nmvf)
      endif
! record time
      call dtimer(dtime,itime,1)
      tdiag = tdiag + real(dtime)
      end subroutine
!
!-----------------------------------------------------------------------
      subroutine mvdist1(part,fv,fvm,tdiag,np,nmv)
! calculates 1d velocity distribution, velocity moments, and entropy
! with standard particle array
      integer, intent(in) :: np, nmv
      real, intent(inout) :: tdiag
      real, dimension(:,:), intent(in) :: part
      real, dimension(:,:), intent(inout) :: fv, fvm
! local data
      integer :: idimp, nmvf, idimv
      integer, dimension(4) :: itime
      double precision :: dtime
! extract dimensions
      idimp = size(part,1)
      nmvf = size(fv,1); idimv = size(fv,2)
! initialize timer
      call dtimer(dtime,itime,-1)
      if (idimv==1) then
         call VDIST1(part,fv,fvm,idimp,np,nmv,nmvf)
      else if (idimv==3) then
         call VDIST13(part,fv,fvm,idimp,np,nmv,nmvf)
      endif
! record time
      call dtimer(dtime,itime,1)
      tdiag = tdiag + real(dtime)
      end subroutine
!
!-----------------------------------------------------------------------
      subroutine setptraj1(ppart,kpic,iprobt,nst,vtx,vtsx,dvtx,np,nprobt&
     &,irc)
! sets test charge distribution by setting a particle id in particle
! location 3 or 5
! nst = type of test particle distribution
!   1 = uniformly distribution in real space
!   2 = uniform distribution in velocity space
!   3 = velocity slice at vtsx +- dvtx/2
! nprobt = number of test charges whose trajectories will be stored.
! irc = (0,1) = (no,yes) error condition exists
      real, dimension(:,:,:), intent(inout) :: ppart
      integer, dimension(:), intent(in) :: kpic
      integer, dimension(:), intent(inout) :: iprobt
      integer, intent(in) :: nst, np
      integer, intent(inout) :: nprobt, irc
      real, intent(in) :: vtx, vtsx, dvtx
! local data
      integer :: idimp, mx1, nppmx
      irc = 0
      idimp = size(ppart,1); nppmx = size(ppart,2)
      mx1 = size(kpic,1)
! call low level procedure
      if (idimp > 4) then
         call STPTRAJ13(ppart,kpic,iprobt,nst,vtx,vtsx,dvtx,idimp,nppmx,&
     &mx1,np,nprobt)
      else if (idimp > 2) then
         call STPTRAJ1(ppart,kpic,iprobt,nst,vtx,vtsx,dvtx,idimp,nppmx, &
     &mx1,np,nprobt)
      else
         write (*,*) 'setptraj1 error: idimp=', idimp
         irc = 1
      endif
      end subroutine
!
!-----------------------------------------------------------------------
      subroutine mfnptraj1(ppart,kpic,nprobt,irc)
! this finds tagged particles in ppart
! nprobt = number of test charges whose trajectories will be stored.
! irc = (0,1) = (no,yes) error condition exists
      implicit none
      integer, intent(inout) :: nprobt, irc
      real, dimension(:,:,:), intent(in) :: ppart
      integer, dimension(:), intent(in) :: kpic
! local data
      integer :: idimp, nppmx, mx1
      irc = 0
! extract dimensions
      idimp = size(ppart,1); nppmx = size(ppart,2)
      mx1 = size(kpic,1)
! call low level procedure
      if (idimp > 4) then
         call FNPTRAJ13(ppart,kpic,idimp,nppmx,mx1,nprobt)
      else if (idimp > 2) then
         call FNPTRAJ1(ppart,kpic,idimp,nppmx,mx1,nprobt)
      else
         write (*,*) 'mfnptraj1 error: idimp=', idimp
         irc = 1
      endif
      end subroutine
!
!-----------------------------------------------------------------------
      subroutine mptraj1(ppart,kpic,partt,tdiag,irc)
! this copies tagged particles in ppart to array partt
! irc = (0,1) = (no,yes) error condition exists
      implicit none
      integer, intent(inout) :: irc
      real, intent(inout) :: tdiag
      real, dimension(:,:,:), intent(in) :: ppart
      integer, dimension(:), intent(in) :: kpic
      real, dimension(:,:), intent(inout) :: partt
! local data
      integer :: idimp, nppmx, mx1, nprobt
      integer, dimension(4) :: itime
      double precision :: dtime
      irc = 0
! extract dimensions
      idimp = size(ppart,1); nppmx = size(ppart,2)
      mx1 = size(kpic,1); nprobt = size(partt,2)
! initialize timer
      call dtimer(dtime,itime,-1)
! call low level procedure
      if (idimp > 4) then
         call PTRAJ13(ppart,kpic,partt,idimp,nppmx,mx1,nprobt)
      else if (idimp > 2) then
         call PTRAJ1(ppart,kpic,partt,idimp,nppmx,mx1,nprobt)
      else
         write (*,*) 'mptraj1 error: idimp=', idimp
         irc = 1
      endif
! record time
      call dtimer(dtime,itime,1)
      tdiag = tdiag + real(dtime)
      end subroutine
!
!-----------------------------------------------------------------------
      subroutine setmbeam1(part,npx,irc)
! marks beam particles by setting a particle id in particle location
! 3 or 5
! irc = (0,1) = (no,yes) error condition exists
      integer, intent(in) :: npx
      integer, intent(inout) :: irc
      real, dimension(:,:), intent(inout) :: part
! local data
      integer :: idimp, nop
      idimp = size(part,1); nop = size(part,2)
      irc = 0
! call low level procedure
      if (idimp > 4) then
         call STPBEAM13(part,npx,idimp,nop)
      else if (idimp > 2) then
         call STPBEAM1(part,npx,idimp,nop)
      else
         write (*,*) 'setbeamc1 error: idimp=', idimp
         irc = 1
      endif
      end subroutine
!
      end module
