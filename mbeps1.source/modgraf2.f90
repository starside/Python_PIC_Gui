!-----------------------------------------------------------------------
!
      module graf2
!
! Fortran90 interface to 2d PIC Fortran77 library libgks2.f
! dscaler2 displays 2d scalar field in real space.
!          calls CARPET or CONTUR
! dscalerl2 displays (color map of 2d scalar field in real space
!           calls CARPETL
! written by viktor k. decyk, ucla
! copyright 2000, regents of the university of california
! update: september 28, 2016
!
      use libgraf2_h
      implicit none
!
      contains
!
!-----------------------------------------------------------------------
      subroutine dscaler2(f,label,itime,isc,ist,idt,nx,ny,irc)
! displays 2d scalar field in real space
! f = 2d scalar field in real space
! label = field label
! itime = current time step
! isc = power of 2 scale of range of values of pot
! if abs(isc) < 116, then the isc value passed is used for scale.
! if abs(isc) > 116, then the program finds the minimum value of isc
! ist = flag for choosing positive and/or negative values
! the range of values of f are given by fmax and fmin.
! if ist = 0, then fmax = 2**isc and fmin = -2**isc.
! if ist = 1, then fmax = 2**isc and fmin = 0.
! if ist = -1, then fmax = 0 and fmin = -2**isc.
! if ist = 2, then fmax = fmin + 2**ir,
! where fmin/fmax are the function minimum/maximum, 
! and ir = power of 2 scale for (fmax - fmin)
! idt = (1,2,3) = display (color map,contour plot,both)
! nx/ny = system length in x/y direction
! irc = return code (0 = normal return)
      implicit none
      integer, intent(in) :: itime, isc, ist, idt, nx, ny
      integer, intent(inout) :: irc
      character(len=*), intent(in) :: label
      real, dimension(:,:), intent(in) :: f
! local data
      integer :: nxv, lx, ly
! ntc = number of valid colors, should be power of 2, <= 256
! nc = number of contour lines
      integer :: ntc = 16, nc = 16
      character(len=12) :: lbl
      integer, dimension(size(f,1),size(f,2)) :: lf
   91 format(' T = ',i7)
      nxv = size(f,1)
      lx = nx; ly = ny
! plot guard cells if present
      if ((lx+1) <= nxv) lx = lx + 1
      if ((ly+1) <= size(f,2)) ly = ly + 1
      write (lbl,91) itime
! color map plot for all values
      if (idt /= 2) then
         call CARPET(f,label,isc,ist,lx,ly,nxv,lbl,ntc,irc)
      endif
! contour map for all values
      if (idt /= 1) then
         call CONTUR(f,lf,label,isc,ist,lx,ly,nxv,lbl,nc,irc)
      endif
      end subroutine
!
!-----------------------------------------------------------------------
      subroutine dscalerl2(f,label,xmin,xmax,ymin,ymax,itime,isc,ist,nx,&
     &ny,irc)
! displays (color map of 2d scalar field in real space
! f = 2d scalar field in real space
! label = field label
! xmin/xmax = numerical labels for x axis
! ymin/ymax = numerical labels for y axis
! itime = current time step
! isc = power of 2 scale of range of values of pot
! if abs(isc) < 116, then the isc value passed is used for scale.
! if abs(isc) > 116, then the program finds the minimum value of isc
! ist = flag for choosing positive and/or negative values
! the range of values of f are given by fmax and fmin.
! if ist = 0, then fmax = 2**isc and fmin = -2**isc.
! if ist = 1, then fmax = 2**isc and fmin = 0.
! if ist = -1, then fmax = 0 and fmin = -2**isc.
! if ist = 2, then fmax = fmin + 2**ir,
! where fmin/fmax are the function minimum/maximum, 
! and ir = power of 2 scale for (fmax - fmin)
! nx/ny = system length in x/y direction
! irc = return code (0 = normal return)
      implicit none
      integer, intent(in) :: itime, isc, ist, nx, ny
      integer, intent(inout) :: irc
      character(len=*), intent(in) :: label
      real, dimension(:,:), intent(in) :: f
! local data
      integer :: nxv, lx, ly
      real :: xmin, xmax, ymin, ymax
! ntc = number of valid colors, should be power of 2, <= 256
      integer :: ntc = 16
      character(len=12) :: lbl
   91 format(' T = ',i7)
      nxv = size(f,1)
      lx = nx; ly = ny
! plot guard cells if present
      if ((lx+1) <= nxv) lx = lx + 1
      if ((ly+1) <= size(f,2)) ly = ly + 1
      write (lbl,91) itime
! color map plot for all values
      call CARPETL(f,label,xmin,xmax,ymin,ymax,isc,ist,lx,ly,nxv,lbl,ntc&
     &,irc)
      end subroutine
!
      end module
