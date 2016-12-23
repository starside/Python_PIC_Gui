!-----------------------------------------------------------------------
! Fortran Library for initialization of particles
! 1D OpenMP PIC Codes:
! NEXTRAN1 = skips over nextrand groups of random numbers
! UDISTR1 calculates initial particle co-ordinates with uniform density
!         for 1d or 1-2/2d code
! LDISTR1 calculates initial particle co-ordinates with linear density
!         profile for 1d or 1-2/2d code
! FDISTR1 calculates initial particle co-ordinates with general density
!         profile where density in x is given by various functions
! GFDISTR1 calculates initial particle co-ordinates with general density
!          profile where density in x is given by various functions
!          and co-ordinates are restricted to xmin <= x < xmax 
! VDISTR1 calculates initial particle velocity with maxwellian velocity
!         with drift for 1d code
! VDISTR1H calculates initial particle velocities with maxwellian
!          velocity with drift for 1-2/2d code
! WDISTR1 calculates initial particle velocity with waterbag velocity
!         distribution with drift for 1d code
! WDISTR1H calculates initial particle velocities with waterbag velocity
!          distribution with drift for 1-2/2d code
! DBLKP1L finds the maximum number of particles in each tile
! ranorm gaussian random number generator
! randum uniform random number generator
! The following functions are used by FDISTR1 and GFDISTR1:
! DLDISTR1 calculates either a density function or its integral
!          for a linear density profile with uniform background
! DSDISTR1 calculates either a density function or its integral
!          for a sinusoidal density profile with uniform background
! DGDISTR1 calculates either a density function or its integral
!          for a gaussian density profile with uniform background
! DHDISTR1 calculates either a density function or its integral
!          for a hyperbolic secant squared density profile
!          with uniform background
! DEDISTR1 calculates either a density function or its integral
!          for an exponential density profile with uniform background
! DGDISTR0 calculates either a density function or its integral
!          for a gaussian density profile with no background density
! written by Viktor K. Decyk, UCLA
! copyright 2016, regents of the university of california
! update: november 12, 2016
!-----------------------------------------------------------------------
      subroutine NEXTRAN1(nextrand,ndim,np)
! for 1d code, this subroutine skips over nextrand groups of random
! numbers in order to initialize different random ensembles
! nextrand = (0,N) = generate (default,Nth block) of random numbers
! ndim = number of velocity dimensions = 1 or 3
! np = number of particles in distribution
      implicit none
      integer nextrand, ndim, np
! local data
      integer j, n
      double precision d
      double precision ranorm
      n = ndim*np*nextrand
      do 10 j = 1, n
      d = ranorm()
   10 continue
      return
      end
!-----------------------------------------------------------------------
      subroutine UDISTR1(part,jstart,npx,idimp,nop,nx,ipbc)
! for 1d or 1-2/2d code, this subroutine calculates initial particle
! co-ordinate with uniform density
! part(1,n) = position x of particle n
! jstart = location of initial particle
! npx = number of particles distributed in x direction
! idimp = size of phase space = 2 or 4
! nop = maximum number of particles
! nx = system length in x direction
! ipbc = particle boundary condition = (0,1,2) =
! (none,2d periodic,2d reflecting)
! ranorm = gaussian random number with zero mean and unit variance
      implicit none
      integer jstart, npx, idimp, nop, nx, ipbc
      real part
      dimension part(idimp,nop)
! local data
      integer j, js
      real edgelx, at1
      js = jstart - 1
      if ((js+npx).gt.nop) return
! set boundary values
      edgelx = 0.0
      at1 = real(nx)/real(npx)
      if (ipbc.eq.2) then
         edgelx = 1.0
         at1 = real(nx-2)/real(npx)
      endif
! uniform density profile
      do 10 j = 1, npx
      part(1,j+js) = edgelx + at1*(real(j) - 0.5)
   10 continue
      return
      end
!-----------------------------------------------------------------------
      subroutine LDISTR1(part,anlx,jstart,npx,idimp,nop,nx,ipbc)
! for 1d or 1-2/2d code, this subroutine calculates initial particle
! co-ordinates with the following linear density profile:
! n(x) = n0x*(1. + anlx*(x/nx - 0.5)) where n0x = npx/(nx - 2*edgelx)
! part(1,n) = position x of particle n
! jstart = location of initial particle
! anlx = initial linear density weight in x direction
! npx = initial number of particles distributed in x direction
! idimp = size of phase space = 2 or 4
! nop = maximum number of particles
! nx = system length in x direction
! ipbc = particle boundary condition = (0,1,2) =
! (none,2d periodic,2d reflecting)
      implicit none
      integer jstart, npx, idimp, nop, nx, ipbc
      real anlx
      real part
      dimension part(idimp,nop)
! local data
      integer j, js
      real edgelx, at1, bt1, antx
      js = jstart - 1
      if ((js+npx).gt.nop) return
! set boundary values
      edgelx = 0.0
      at1 = real(nx)/real(npx)
      if (ipbc.eq.2) then
         edgelx = 1.0
         at1 = real(nx-2)/real(npx)
      endif
      if (anlx.ne.0.0) then
         antx = anlx/real(nx)
         at1 = 2.0*antx*at1
         bt1 = 1.0 - 0.5*antx*(real(nx) - 2.0*edgelx)
      endif
! linear density in x
      if (anlx.ne.0.0) then
         do 10 j = 1, npx
         part(1,j+js) = edgelx + (sqrt(bt1*bt1 + at1*(real(j) - 0.5))   &
     &                            - bt1)/antx
   10    continue
! uniform density in x
      else
         do 20 j = 1, npx
         part(1,j+js) = edgelx + at1*(real(j) - 0.5)
   20    continue
      endif
      return
      end
!-----------------------------------------------------------------------
      subroutine FDISTR1(part,fnx,argx1,argx2,argx3,jstart,npx,idimp,nop&
     &,nx,ipbc,ierr)
! for 1d code, this subroutine calculates initial particle co-ordinates
! with general density profile where density in x is given by
! n(x) = fnx(x,argx1,argx2,argx3,0) and integral of the density is given
! by fnx(x,argx1,argx2,argx3,1)
! part(1,n) = position x of particle n
! fnx = density and density integral function in x direction
! argx1,argx2,argx3 = arguments to fnx
! jstart = location of initial particle
! npx = initial number of particles distributed in x direction
! idimp = size of phase space = 2 or 4
! nop = maximum number of particles
! nx = system length in x direction
! ipbc = particle boundary condition = (0,1,2) =
! (none,2d periodic,2d reflecting)
! ierr = (0,1) = (no,yes) error condition exists
      implicit none
      integer jstart, npx, idimp, nop, nx, ipbc, ierr
      double precision argx1, argx2, argx3
      real part
      dimension part(idimp,nop)
      double precision fnx
      external fnx
! local data
      integer imax, i, j, js
      real edgelx, anx, bnx, xt0, x0
      real xn, eps, big, f, fp
      double precision xt
      ierr = 0
      js = jstart - 1
      if ((js+npx).gt.nop) then
         ierr = -1
         return
      endif
! eps = convergence criterion
      imax = nx
      eps = 0.0001
      big = 0.25
! set boundary value
      edgelx = 0.0
      if (ipbc.eq.2) then
         edgelx = 1.0
      endif
! find normalization for function
      anx = real(nx) - edgelx
      x0 = fnx(dble(edgelx),argx1,argx2,argx3,1)
      bnx = real(npx)/(fnx(dble(anx),argx1,argx2,argx3,1) - x0)
      x0 = bnx*x0 - 0.5
! density profile in x
      xt0 = edgelx
      xt = xt0
      fp = bnx*fnx(xt,argx1,argx2,argx3,0)
      if (fp.gt.0.0) xt = xt + 0.5/fp
      do 20 j = 1, npx
      xn = real(j) + x0
! guess next value for xt
      if (j.gt.1) then
         fp = bnx*fnx(xt,argx1,argx2,argx3,0)
         if (fp.eq.0.0) fp = 1.0
         xt = xt + 1.0/fp
      endif
      xt = max(edgelx,min(xt,anx))
      i = 0
   10 f = bnx*fnx(xt,argx1,argx2,argx3,1) - dble(xn)
! find improved value for xt
      if (abs(f).ge.eps) then
         fp = bnx*fnx(xt,argx1,argx2,argx3,0)
! newton's method
         if ((abs(f).lt.big).and.(fp.gt.0.0)) then
            xt0 = xt
            xt = xt - f/fp
            xt = max(edgelx,min(xt,anx))
! bisection method
         else if (f.gt.0.) then
            fp = 0.5*abs(xt0 - xt)
            xt = xt0 - fp
         else
            fp = abs(xt - xt0)
            xt0 = xt
            xt = xt + fp
         endif
         i = i + 1
         if (i.lt.imax) go to 10
!        write (*,*) j,'newton iteration max exceeded, xt = ', xt
         ierr = ierr + 1
      endif
! store co-ordinate
      part(1,j+js) = xt
      xt0 = xt
   20 continue
      return
      end
!-----------------------------------------------------------------------
      subroutine GFDISTR1(part,fnx,argx1,argx2,argx3,xmin,xmax,jstart,  &
     &npx,idimp,nop,nx,ierr)
! for 1d code, this subroutine calculates initial particle co-ordinates
! with general density profile where density in x is given by
! n(x) = fnx(x,argx1,argx2,argx3,0) and integral of the density is given
! by fnx(x,argx1,argx2,argx3,1)
! x co-ordinates will be within range xmin <= x < xmax
! part(1,n) = position x of particle n
! fnx = density and density integral function in x direction
! argx1,argx2,argx3 = arguments to fnx
! xmin/xmax = minimum/maximum range of particle coordinates in x
! xmin must be >= 0 and xmax must be <= nx
! jstart = location of initial particle
! npx = initial number of particles distributed in x direction
! idimp = size of phase space = 2 or 4
! nop = maximum number of particles
! nx = system length in x direction
! ierr = (0,1) = (no,yes) error condition exists
      implicit none
      integer jstart, npx, idimp, nop, nx, ierr
      real xmin, xmax
      double precision argx1, argx2, argx3
      real part
      dimension part(idimp,nop)
      double precision fnx
      external fnx
! local data
      integer imax, i, j, js
      real edgelx, anx, bnx, xt0, x0
      real xn, eps, big, f, fp
      double precision xt
      ierr = 0
      js = jstart - 1
      if ((js+npx).gt.nop) then
         ierr = -1
         return
      endif
! eps = convergence criterion
      imax = nx
      eps = 0.0001
      big = 0.25
! set boundary value
      if ((xmin.lt.0.0).or.(xmin.ge.xmax).or.(xmax.gt.real(nx))) then
         ierr = -2
         return
      endif
      edgelx = xmin
! find normalization for function
      anx = xmax
      x0 = fnx(dble(edgelx),argx1,argx2,argx3,1)
      bnx = real(npx)/(fnx(dble(anx),argx1,argx2,argx3,1) - x0)
      x0 = bnx*x0 - 0.5
! density profile in x
      xt0 = edgelx
      xt = xt0
      fp = bnx*fnx(xt,argx1,argx2,argx3,0)
      if (fp.gt.0.0) xt = xt + 0.5/fp
      do 20 j = 1, npx
      xn = real(j) + x0
! guess next value for xt
      if (j.gt.1) then
         fp = bnx*fnx(xt,argx1,argx2,argx3,0)
         if (fp.eq.0.0) fp = 1.0
         xt = xt + 1.0/fp
      endif
      xt = max(edgelx,min(xt,anx))
      i = 0
   10 f = bnx*fnx(xt,argx1,argx2,argx3,1) - dble(xn)
! find improved value for xt
      if (abs(f).ge.eps) then
         fp = bnx*fnx(xt,argx1,argx2,argx3,0)
! newton's method
         if ((abs(f).lt.big).and.(fp.gt.0.0)) then
            xt0 = xt
            xt = xt - f/fp
            xt = max(edgelx,min(xt,anx))
! bisection method
         else if (f.gt.0.) then
            fp = 0.5*abs(xt0 - xt)
            xt = xt0 - fp
         else
            fp = abs(xt - xt0)
            xt0 = xt
            xt = xt + fp
         endif
         i = i + 1
         if (i.lt.imax) go to 10
!        write (*,*) j,'newton iteration max exceeded, xt = ', xt
         ierr = ierr + 1
      endif
! store co-ordinate
      part(1,j+js) = xt
      xt0 = xt
   20 continue
      return
      end
!-----------------------------------------------------------------------
      subroutine VDISTR1(part,vtx,vdx,jstart,npx,idimp,nop)
! for 1d code, this subroutine calculates initial particle
! velocity with maxwellian velocity with drift
! part(2,n) = velocity vx of particle n
! vtx = thermal velocity of electrons in x direction
! vdx = drift velocity of beam electrons in x direction
! jstart = starting location in particle array
! npx = number of particles distributed in x direction
! idimp = size of phase space = 2
! nop = maximum number of particles
! ranorm = gaussian random number with zero mean and unit variance
      implicit none
      integer jstart, npx, idimp, nop
      real part, vtx, vdx
      dimension part(idimp,nop)
! local data
      integer j, js
      real sum1
      double precision dsum1
      double precision ranorm
      js = jstart - 1
      if ((js+npx).gt.nop) return
! maxwellian velocity distribution
      do 10 j = 1, npx
      part(2,j+js) = vtx*ranorm()
   10 continue
! add correct drift
      dsum1 = 0.0d0
      do 20 j = 1, npx
      dsum1 = dsum1 + part(2,j+js)
   20 continue
      sum1 = real(dsum1)/real(npx) - vdx
      do 30 j = 1, npx
      part(2,j+js) = part(2,j+js) - sum1
   30 continue
      return
      end
!-----------------------------------------------------------------------
      subroutine VDISTR1H(part,vtx,vty,vtz,vdx,vdy,vdz,jstart,npx,idimp,&
     &nop)
! for 1-2/2d code, this subroutine calculates initial particle
! velocities with maxwellian velocity with drift
! part(2,n) = velocity vx of particle n
! part(3,n) = velocity vy of particle n
! part(4,n) = velocity vz of particle n
! vtx/vty/vtz = thermal velocity of electrons in x/y/z direction
! vdx/vdy/vdz = drift velocity of beam electrons in x/y/z direction
! jstart = starting location in particle array
! npx = number of particles distributed in x direction
! idimp = size of phase space = 4
! nop = maximum number of particles
! ranorm = gaussian random number with zero mean and unit variance
      implicit none
      integer jstart, npx, idimp, nop
      real part, vtx, vty, vtz, vdx, vdy, vdz
      dimension part(idimp,nop)
! local data
      integer j, js
      real at1, sum1, sum2, sum3
      double precision dsum1, dsum2, dsum3
      double precision ranorm
      js = jstart - 1
      if ((js+npx).gt.nop) return
! maxwellian velocity distribution
      do 10 j = 1, npx
      part(2,j+js) = vtx*ranorm()
      part(3,j+js) = vty*ranorm()
      part(4,j+js) = vtz*ranorm()
   10 continue
! add correct drift
      dsum1 = 0.0d0
      dsum2 = 0.0d0
      dsum3 = 0.0d0
      do 20 j = 1, npx
      dsum1 = dsum1 + part(2,j+js)
      dsum2 = dsum2 + part(3,j+js)
      dsum3 = dsum3 + part(4,j+js)
   20 continue
      sum1 = dsum1
      sum2 = dsum2
      sum3 = dsum3
      at1 = 1.0/real(npx)
      sum1 = at1*real(dsum1) - vdx
      sum2 = at1*real(dsum2) - vdy
      sum3 = at1*real(dsum3) - vdz
      do 30 j = 1, npx
      part(2,j+js) = part(2,j+js) - sum1
      part(3,j+js) = part(3,j+js) - sum2
      part(4,j+js) = part(4,j+js) - sum3
   30 continue
      return
      end
!-----------------------------------------------------------------------
      subroutine WDISTR1(part,vtx,vdx,jstart,npx,idimp,nop)
! for 1d code, this subroutine calculates initial particle
! velocity with waterbag velocity distribution with drift
! part(2,n) = velocity vx of particle n
! vtx = maximum velocity of electrons in x direction
! vdx = drift velocity of beam electrons in x direction
! jstart = starting location in particle array
! npx = number of particles distributed in x direction
! idimp = size of phase space = 2
! nop = maximum number of particles
! ranorm = gaussian random number with zero mean and unit variance
      implicit none
      integer jstart, npx, idimp, nop
      real part, vtx, vdx
      dimension part(idimp,nop)
! local data
      integer j, js
      real vmx, sum1
      double precision dsum1
      double precision randum
      js = jstart - 1
      if ((js+npx).gt.nop) return
      vmx = 2.0*sqrt(3.0)*vtx
! maxwellian velocity distribution
      do 10 j = 1, npx
      part(2,j+js) = vmx*(randum() - 0.5d0)
   10 continue
! add correct drift
      dsum1 = 0.0d0
      do 20 j = 1, npx
      dsum1 = dsum1 + part(2,j+js)
   20 continue
      sum1 = real(dsum1)/real(npx) - vdx
      do 30 j = 1, npx
      part(2,j+js) = part(2,j+js) - sum1
   30 continue
      return
      end
!-----------------------------------------------------------------------
      subroutine WDISTR1H(part,vtx,vty,vtz,vdx,vdy,vdz,jstart,npx,idimp,&
     &nop)
! for 1-2/2d code, this subroutine calculates initial particle
! velocities with waterbag velocity distribution with drift
! part(2,n) = velocity vx of particle n
! part(3,n) = velocity vy of particle n
! part(4,n) = velocity vz of particle n
! vtx/vty/vtz = maximum velocity of electrons in x/y/z direction
! vdx/vdy/vdz = drift velocity of beam electrons in x/y/z direction
! jstart = starting location in particle array
! npx = number of particles distributed in x direction
! idimp = size of phase space = 4
! nop = maximum number of particles
! ranorm = gaussian random number with zero mean and unit variance
      implicit none
      integer jstart, npx, idimp, nop
      real part, vtx, vty, vtz, vdx, vdy, vdz
      dimension part(idimp,nop)
! local data
      integer j, js
      real vmx, vmy, vmz, at1, sum1, sum2, sum3
      double precision dsum1, dsum2, dsum3
      double precision randum
      js = jstart - 1
      if ((js+npx).gt.nop) return
      vmx = 2.0*sqrt(3.0)*vtx
      vmy = 2.0*sqrt(3.0)*vty
      vmz = 2.0*sqrt(3.0)*vtz
! maxwellian velocity distribution
      do 10 j = 1, npx
      part(2,j+js) = vmx*(randum() - 0.5d0)
      part(3,j+js) = vmy*(randum() - 0.5d0)
      part(4,j+js) = vmz*(randum() - 0.5d0)
   10 continue
! add correct drift
      dsum1 = 0.0d0
      dsum2 = 0.0d0
      dsum3 = 0.0d0
      do 20 j = 1, npx
      dsum1 = dsum1 + part(2,j+js)
      dsum2 = dsum2 + part(3,j+js)
      dsum3 = dsum3 + part(4,j+js)
   20 continue
      sum1 = dsum1
      sum2 = dsum2
      sum3 = dsum3
      at1 = 1.0/real(npx)
      sum1 = at1*real(dsum1) - vdx
      sum2 = at1*real(dsum2) - vdy
      sum3 = at1*real(dsum3) - vdz
      do 30 j = 1, npx
      part(2,j+js) = part(2,j+js) - sum1
      part(3,j+js) = part(3,j+js) - sum2
      part(4,j+js) = part(4,j+js) - sum3
   30 continue
      return
      end
!-----------------------------------------------------------------------
      subroutine DBLKP1L(part,kpic,nppmx,idimp,np,nop,mx,mx1,irc)
! this subroutine finds the maximum number of particles in each tile of
! mx to calculate size of segmented particle array ppart
! linear interpolation
! input: all except kpic, nppmx, output: kpic, nppmx
! part = input particle array
! part(1,n) = position x of particle n
! kpic = output number of particles per tile
! nppmx = return maximum number of particles in tile
! idimp = size of phase space = 2
! np = number of particles
! nop = maximum number of particles
! mx = number of grids in sorting cell in x
! mx1 = (system length in x direction - 1)/mx + 1
! irc = maximum overflow, returned only if error occurs, when irc > 0
      implicit none
      integer kpic, nppmx, idimp, np, nop, mx, mx1, irc
      real part
      dimension part(idimp,nop), kpic(mx1)
! local data
      integer j, k, n, isum, ist, npx, ierr
      ierr = 0
! clear counter array
      do 10 k = 1, mx1
      kpic(k) = 0
   10 continue
! find how many particles in each tile
      do 20 j = 1, np
      n = part(1,j)
      n = n/mx + 1
      if (n.le.mx1) then
         kpic(n) = kpic(n) + 1
      else
         ierr = max(ierr,n-mx1)
      endif
   20 continue
! find maximum
      isum = 0
      npx = 0
      do 30 k = 1, mx1
      ist = kpic(k)
      npx = max(npx,ist)
      isum = isum + ist
   30 continue
      nppmx = npx
! check for errors
      if (ierr.gt.0) then
         irc = ierr
      else if (isum.ne.np) then
         irc = -1
      endif
      return
      end
!-----------------------------------------------------------------------
      function ranorm()
! this program calculates a random number y from a gaussian distribution
! with zero mean and unit variance, according to the method of
! mueller and box:
!    y(k) = (-2*ln(x(k)))**1/2*sin(2*pi*x(k+1))
!    y(k+1) = (-2*ln(x(k)))**1/2*cos(2*pi*x(k+1)),
! where x is a random number uniformly distributed on (0,1).
! written for the ibm by viktor k. decyk, ucla
      implicit none
      integer iflg,isc,i1,r1,r2,r4,r5
      double precision ranorm,h1l,h1u,h2l,r0,r3,asc,bsc,temp
      save iflg,r1,r2,r4,r5,h1l,h1u,h2l,r0
      data r1,r2,r4,r5 /885098780,1824280461,1396483093,55318673/
      data h1l,h1u,h2l /65531.0d0,32767.0d0,65525.0d0/
      data iflg,r0 /0,0.0d0/
      if (iflg.eq.0) go to 10
      ranorm = r0
      r0 = 0.0d0
      iflg = 0
      return
   10 isc = 65536
      asc = dble(isc)
      bsc = asc*asc
      i1 = r1 - (r1/isc)*isc
      r3 = h1l*dble(r1) + asc*h1u*dble(i1)
      i1 = r3/bsc
      r3 = r3 - dble(i1)*bsc
      bsc = 0.5d0*bsc
      i1 = r2/isc
      isc = r2 - i1*isc
      r0 = h1l*dble(r2) + asc*h1u*dble(isc)
      asc = 1.0d0/bsc
      isc = r0*asc
      r2 = r0 - dble(isc)*bsc
      r3 = r3 + (dble(isc) + 2.0d0*h1u*dble(i1))
      isc = r3*asc
      r1 = r3 - dble(isc)*bsc
      temp = dsqrt(-2.0d0*dlog((dble(r1) + dble(r2)*asc)*asc))
      isc = 65536
      asc = dble(isc)
      bsc = asc*asc
      i1 = r4 - (r4/isc)*isc
      r3 = h2l*dble(r4) + asc*h1u*dble(i1)
      i1 = r3/bsc
      r3 = r3 - dble(i1)*bsc
      bsc = 0.5d0*bsc
      i1 = r5/isc
      isc = r5 - i1*isc
      r0 = h2l*dble(r5) + asc*h1u*dble(isc)
      asc = 1.0d0/bsc
      isc = r0*asc
      r5 = r0 - dble(isc)*bsc
      r3 = r3 + (dble(isc) + 2.0d0*h1u*dble(i1))
      isc = r3*asc
      r4 = r3 - dble(isc)*bsc
      r0 = 6.28318530717959d0*((dble(r4) + dble(r5)*asc)*asc)
      ranorm = temp*dsin(r0)
      r0 = temp*dcos(r0)
      iflg = 1
      return
      end
!-----------------------------------------------------------------------
      function randum()
! this is a version of the random number generator dprandom due to
! c. bingham and the yale computer center, producing numbers
! in the interval (0,1).  written for the sun by viktor k. decyk, ucla
      implicit none
      integer isc,i1,r1,r2
      double precision randum,h1l,h1u,r0,r3,asc,bsc
      save r1,r2,h1l,h1u
      data r1,r2 /1271199957,1013501921/
      data h1l,h1u /65533.0d0,32767.0d0/
      isc = 65536
      asc = dble(isc)
      bsc = asc*asc
      i1 = r1 - (r1/isc)*isc
      r3 = h1l*dble(r1) + asc*h1u*dble(i1)
      i1 = r3/bsc
      r3 = r3 - dble(i1)*bsc
      bsc = 0.5d0*bsc
      i1 = r2/isc
      isc = r2 - i1*isc
      r0 = h1l*dble(r2) + asc*h1u*dble(isc)
      asc = 1.0d0/bsc
      isc = r0*asc
      r2 = r0 - dble(isc)*bsc
      r3 = r3 + (dble(isc) + 2.0d0*h1u*dble(i1))
      isc = r3*asc
      r1 = r3 - dble(isc)*bsc
      randum = (dble(r1) + dble(r2)*asc)*asc
      return
      end
!-----------------------------------------------------------------------
      function DLDISTR1(x,anlx,anxi,shift,intg)
! this function calculates either a density function or its integral
! for a linear density profile.  Used in initializing particle
! coordinates.  The three parameters are redundant, and one can set one
! of them arbitrarily.  A convenient choice is to set  anxi = 1/Lx,
! anlx = NH - NL, shift = (1 - NL)/(NH - NL), where NL is the density
! at the left, and NH at the right compared to the average density
! if intg = 0, n(x) = 1. + anlx*(x*anxi - shift)
! if intg = 1, n(x) = x + .5*anlx*x*(x*anxi - 2.*shift)
      implicit none
      integer intg
      double precision x, anlx, anxi, shift
! local data
      double precision DLDISTR1, f
      if (intg.eq.0) then
         f = 1.0d0 + anlx*(x*anxi - shift)
      else if (intg.eq.1) then
         if (anxi.eq.0.0d0) then
            f = x
         else
            f = x + 0.5d0*anlx*x*(x*anxi - 2.0d0*shift)
         endif
      else
         f = -1.0d0
      endif
      if (f.lt.0.0d0) write (*,*) 'DLDISTR1 Error: f = ', f
      DLDISTR1 = f
      return
      end
!-----------------------------------------------------------------------
      function DSDISTR1(x,ans,dkx,phase,intg)
! this function calculates either a density function or its integral
! for a sinusoidal density profile.  Used in initializing particle
! coordinates.
! if intg = 0, n(x) = 1.0 + ans*sin(dkx*x - phase)
! if intg = 1, n(x) = x - (ans/dkx)*(cos(dkx*x - phase) - cos(phase))
      implicit none
      integer intg
      double precision x, ans, dkx, phase
! local data
      double precision DSDISTR1, f
      if (intg.eq.0) then
         f = 1.0d0 + ans*sin(dkx*x - phase)
      else if (intg.eq.1) then
         if (dkx.eq.0.0d0) then
            f = x - ans*sin(phase)*x
         else
            f = x - (ans/dkx)*(cos(dkx*x - phase) - cos(phase))
         endif
      else
         f = -1.0d0
      endif
      if (f.lt.0.0d0) write (*,*) 'DSDISTR1 Error: f = ', f
      DSDISTR1 = f
      return
      end
!-----------------------------------------------------------------------
      function DGDISTR1(x,ang,wi,x0,intg)
! this function calculates either a density function or its integral
! for a gaussian density profile.  Used in initializing particle
! coordinates.
! if intg = 0, n(x) = 1.0 + ang*exp(-((x-x0)*wi)**2/2.)
! if intg = 1, n(x) = x + (ang*sqrt(pi/2)/wi)*
!                         (erf((x-x0)*wi/sqrt(2)) + erf(x0*wi/sqrt(2)))
      implicit none
      integer intg
      double precision x, ang, x0, wi
! local data
      double precision DGDISTR1, f, sqrt2i, sqtpih, aw, t, derfn
      external derfn
      data sqrt2i, sqtpih /0.7071067811865476,1.253314137397325/
      save sqrt2i, sqtpih
      aw = wi*sqrt2i
      t = (x - x0)*aw
      if (intg.eq.0) then
         if (abs(t).lt.8.0d0) then
            f = 1.0d0 + ang*exp(-t**2)
         else
            f = 1.0d0
         endif
      else if (intg.eq.1) then
         if (wi.eq.0.0d0) then
            f = (1.0d0 + ang)*x
         else
            f = x + (ang*sqtpih/wi)*(derfn(t) + derfn(x0*aw))
         endif
      else
         f = -1.0d0
      endif
      if (f.lt.0.0d0) write (*,*) 'DGDISTR1 Error: f = ', f
      DGDISTR1 = f
      return
      end
!-----------------------------------------------------------------------
      function DHDISTR1(x,anh,wi,x0,intg)
! this function calculates either a density function or its integral
! for a hyperbolic secant squared density profile.  Used in initializing
! particle coordinates.
! if intg = 0, n(x) = 1.0 + anh*sech((x-x0)*wi)**2
! if intg = 1, n(x) = x + (anh/wi)*(tanh((x-x0)*wi) + tanh(x0*wi))
      implicit none
      integer intg
      double precision x, anh, x0, wi
! local data
      double precision DHDISTR1, f, g, t, u
      t = (x - x0)*wi
      if (intg.eq.0) then
         if (abs(t).lt.32.0d0) then
            u = exp(-abs(t))
            f = 1.0d0 + anh*(2.0d0*u/(1.0d0 + u*u))**2
         else
            f = 1.0d0
         endif
      else if (intg.eq.1) then
         if (wi.eq.0.0d0) then
            f = (1.0d0 + anh)*x
         else
            if (abs(t).lt.32.0d0) then
               u = exp(-abs(t))**2
               f = (1.0d0 - u)/(1.0d0 + u)
            else
               f = 1.0d0
            endif
            if (t.lt.0.0d0) f = -f
            t = x0*wi
            if (abs(t).lt.32.0d0) then
               u = exp(-abs(t))**2
               g = (1.0d0 - u)/(1.0d0 + u)
            else
               g = 1.0d0
            endif
            if (t.lt.0.0d0) g = -g
            f = x + (anh/wi)*(f + g)
         endif
      else
         f = -1.0d0
      endif
      if (f.lt.0.0d0) write (*,*) 'DHDISTR1 Error: f = ', f
      DHDISTR1 = f
      return
      end
!-----------------------------------------------------------------------
      function DEDISTR1(x,ane,wi,x0,intg)
! this function calculates either a density function or its integral
! for an exponential density profile.  Used in initializing particle
! coordinates.
! if intg = 0, n(x) = 1.0 + ane*exp((x-x0)*wi)
! if intg = 1, n(x) = x + (ane/wi)*(exp((x-x0)*wi) - exp(-x0*wi)
      implicit none
      integer intg
      double precision x, ane, x0, wi
! local data
      double precision DEDISTR1, f, t
      t = (x - x0)*wi
      if (intg.eq.0) then
         if (t.gt.(-64.0d0)) then
            f = 1.0d0 + ane*exp(t)
         else
            f = 1.0d0
         endif
      else if (intg.eq.1) then
         if (wi.eq.0.0d0) then
            f = (1.0d0 + ane)*x
         else
            f = x0*wi
            if (f.gt.64) then
               f = 0.0d0
            else
               f = exp(-f)
            endif
            f = x + (ane/wi)*(exp(t) - f)
         endif
      else
         f = -1.0d0
      endif
      if (f.lt.0.0d0) write (*,*) 'DEDISTR1 Error: f = ', f
      DEDISTR1 = f
      return
      end
!-----------------------------------------------------------------------
      function DGDISTR0(x,ang,wi,x0,intg)
! this function calculates either a density function or its integral
! for a gaussian density profile.  Used in initializing particle
! coordinates.  No background density
! if intg = 0, n(x) = ang*exp(-((x-x0)*wi)**2/2.)
! if intg = 1, n(x) = (ang*sqrt(pi/2)/wi)*
!                         (erf((x-x0)*wi/sqrt(2)) + erf(x0*wi/sqrt(2)))
      implicit none
      integer intg
      double precision x, ang, x0, wi
! local data
      double precision DGDISTR0, f, sqrt2i, sqtpih, aw, t, derfn
      external derfn
      data sqrt2i, sqtpih /0.7071067811865476,1.253314137397325/
      save sqrt2i, sqtpih
      aw = wi*sqrt2i
      t = (x - x0)*aw
      if (intg.eq.0) then
         if (abs(t).lt.8.0d0) then
            f = ang*exp(-t**2)
         else
            f = 0.0d0
         endif
      else if (intg.eq.1) then
         if (wi.eq.0.0d0) then
            f = ang*x
         else
            f = (ang*sqtpih/wi)*(derfn(t) + derfn(x0*aw))
         endif
      else
         f = -1.0d0
      endif
      if (f.lt.0.0d0) write (*,*) 'DGDISTR0 Error: f = ', f
      DGDISTR0 = f
      return
      end
!-----------------------------------------------------------------------
      function derfn(x)
! this function calculates the real error function, according to the
! formulae given in Abramowitz and Stegun, Handbook of Mathematical
! Functions, p. 299.  Error is < 1.5 x 10-7.
      implicit none
      double precision x
! local data
      double precision derfn, p, a1, a2, a3, a4, a5, t, f
      data p, a1, a2 /0.3275911,0.254829592,-0.284496736/
      data a3, a4, a5 /1.421413741,-1.453152027,1.061405429/
      save p, a1, a2, a3, a4, a5
      f = abs(x)
      t = 1.0d0/(1.0d0 + p*f)
      if (f.le.8.0d0) then
         derfn = 1.0d0 - t*(a1 + t*(a2 + t*(a3 + t*(a4 + t*a5))))       &
     &                    *exp(-x*x)
      else
         derfn = 1.0d0
      endif
      if (x.lt.0.0d0) derfn = -derfn
      return
      end