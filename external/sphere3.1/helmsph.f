C
C  . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .
C  .                                                             .
C  .                  Copyright (C) 1998 by UCAR                 .
C  .                                                             .
C  .       University Corporation for Atmospheric Research       .
C  .                                                             .
C  .                      All Rights Reserved                    .
C  .                                                             .
C  .                                                             .
C  .                         SPHEREPACK                          .
C  .                                                             .
C  . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .
C
C
C
C ... file helmsph.f
c
c     this file contains a program for solving the Helmholtz
c     equation with constant 1.0 on a ten degree grid on the full sphere
c
c ... required spherepack3.0 files
c
c     islapec.f, shaec.f, shsec.f, sphcom.f, hrfft.f
c
c ... description
c
c     let theta be latitude and phi be east longitude in radians.
c     and let
c
c
c       x = cos(theta)*sin(phi)
c       y = cos(theta)*cos(phi)
c       z = sint(theta)
c
c     be the cartesian coordinates corresponding to theta and phi.
c     on the unit sphere.  The exact solution
c
c        ue(theta,phi) = (1.+x*y)*exp(z)
c
c     is used to set the right hand side and compute error.
c
c
c **********************************************************************
C
c OUTPUT FROM EXECUTING THE PROGRAM BELOW
c WITH 32 AND 64 BIT FLOATING POINT ARITHMETIC
c
c Helmholtz approximation on a ten degree grid
c nlat = 19   nlon = 36
c xlmbda =  1.00   pertrb =  0.000E+00
c maximum error =  0.715E-06 *** (32 BIT)
c maximum error =  0.114E-12 *** (64 BIT)
c
c ***********************************************
c ***********************************************
      program helmsph
c
c     set grid size with parameter statements
c
      implicit none
      integer nnlat,nnlon,nn15,llsave,llwork,lldwork
      parameter (nnlat=19,nnlon=36)
c
c     set saved and unsaved work space lengths in terms of nnlat,nnlon
c     (see documentation for shaec,shsec,islapec)
c
      parameter (nn15=nnlon+15)
      parameter (llsave=nnlat*(nnlat+1)+3*((nnlat-2)*(nnlat-1)+nn15))
      parameter (llwork=nnlat*(2*nnlon+3*(nnlat+1)+2*nnlat+1))
c
c     set double precision work space length for initializations
c
      parameter (lldwork = nnlat+1)
c
c     dimension arrays
c
      real u(nnlat,nnlon),r(nnlat,nnlon)
      real sint(nnlat),cost(nnlat),sinp(nnlon),cosp(nnlon)
      real work(llwork),wshaec(llsave),wshsec(llsave)
      double precision dwork(lldwork)
      real a(nnlat,nnlat),b(nnlat,nnlat)
      integer nlat,nlon,i,j,lshaec,lshsec,lwork,ierror,isym,nt
      integer ldwork
      real pi,x,y,z,dlat,dlon,theta,phi,xlmbda,pertrb,ez,ue,errm
      pi = 4.0*atan(1.0)
c
c     set helmholtz constant
c
      xlmbda = 1.0
c
c     set work space length arguments
c
      lwork = llwork
      ldwork = lldwork
      lshaec = llsave
      lshsec = llsave
c
c     set grid size arguments
c
      nlat = nnlat
      nlon = nnlon
c
c     set sine and cosine vectors
c
      dlat = pi/(nlat-1)
      dlon = (pi+pi)/nlon
      do i=1,nlat
       theta = -0.5*pi+(i-1)*dlat
       sint(i) = sin(theta)
       cost(i) = cos(theta)
      end do
      do j=1,nlon
       phi = (j-1)*dlon
       sinp(j) = sin(phi)
       cosp(j) = cos(phi)
      end do
c
c     set right hand side as helmholtz operator
c     applied to ue = (1.+x*y)*exp(z)
c
      do j=1,nlon
       do i=1,nlat
	x = cost(i)*cosp(j)
	y = cost(i)*sinp(j)
	z = sint(i)
	r(i,j) = -(x*y*(z*z+6.*(z+1.))+z*(z+2.))*exp(z)
       end do
      end do
c
c     initialize saved work space arrays for scalar harmonic
c     analysis and Helmholtz inversion of r
c
      CALL SHAECI(NLAT,NLON,WSHAEC,LSHAEC,DWORK,LDWORK,IERROR)
      if (ierror .gt. 0) then
      write (6,200) ierror
  200 format(' shaeci, ierror = ',i2)
      call exit(0)
      end if
      CALL SHSECI(NLAT,NLON,WSHSEC,LSHSEC,DWORK,LDWORK,IERROR)
      if (ierror .gt. 0) then
      write (6,201) ierror
  201 format(' shseci, ierror = ',i2)
      call exit(0)
      end if
c
c     set no symmetry and one array
c
      isym = 0
      nt = 1
c
c     compute coefficients of r for input to islapec
c
      call shaec(nlat,nlon,isym,nt,r,nlat,nlon,a,b,nlat,nlat,
     +           wshaec,lshaec,work,lwork,ierror)
      if (ierror .gt. 0) then
      write(*,202) ierror
  202 format(' shaec , ierror = ',i2)
      call exit(0)
      end if
c
c     solve Helmholtz equation on the sphere in u
c
      write (6,100) nlat,nlon
  100 format(' helmholtz approximation on a ten degree grid'
     +       /' nlat = ',i3,2x,' nlon = ', i3)
      call islapec(nlat,nlon,isym,nt,xlmbda,u,nlat,nlon,a,b,nlat,nlat,
     +             wshsec,lshsec,work,lwork,pertrb,ierror)
      if (ierror .ne. 0) then
      write (6,103) ierror
  103 format(' islapec, ierror = ',i2)
      if (ierror .gt. 0) call exit(0)
      end if
c
c     compute and print maximum error in u
c
      errm = 0.0
      do j=1,nlon
       do i=1,nlat
	x = cost(i)*cosp(j)
	y = cost(i)*sinp(j)
	z = sint(i)
	ez = exp(z)
	ue = (1.+x*y)*ez
	errm = amax1(errm,abs(u(i,j)-ue))
       end do
      end do
      write(*,204) xlmbda,pertrb,errm
  204 format(' xlmbda = ',f5.2,2x, ' pertrb = ' ,e10.3,
     +      /' maximum error = ',e10.3)
      end
