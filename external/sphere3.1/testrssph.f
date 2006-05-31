c
c  . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .
c  .                                                             .
c  .                  copyright (c) 1998 by UCAR                 .
c  .                                                             .
c  .       University Corporation for Atmospheric Research       .
c  .                                                             .
c  .                      all rights reserved                    .
c  .                                                             .
c  .                                                             .
c  .                         SPHEREPACK                       .
c  .                                                             .
c  . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .
c
c
C
C
C ... file testrssph.f
c
c     this file contains a test program for subroutine trssph
c
c ... author
c
c     John C Adams (1996, NCAR)
c
c ... required files
c
c     subroutine trssph and the entire spherepack library
c
c ... description (see documentation in file trssph.f)
c
c     Subroutine trssph is used to demonstrate data transfer between a coarse
c     ten degree equally spaced grid and a higher resolution T64 Global Spectral
c     Gaussian grid.  The equally spaced data is stored in a 19 X 36 colatitude-
c     longitude array DATAE which runs north to south with increasing subscript
c     values.  The Gaussian grid data is stored in a 192 X 94 longitude-latitude
c     array DATAG which runs south to north with increasing latitude subscript.
c     First trssph is used to transfer DATAE to DATAG.  Then trssph is used to
c     transfer DATAG back to DATAE.
c
c     For testing purposes, DATAE is set equal the analytic function
c
c                       x*y*z
c           f(x,y,z) = e
c
c     in Cartesian coordinates x,y,z restricted to the surface of the sphere.
c     The same function is used to compute error in DATAG after the data transfer
c     with trssph.  Finally this is used to compute error in DATAE after the transfer
c     back with trssph.  Output from executing the test program on machines with
c     32 bit and 64 bit arithmetic is listed below.  The minimum required saved
c     and unsaved work space lengths were predetermined by an earlier call to
c     trssph with nlone=36,nlate=19,nlong=192,nlatg=94,lsave=1,lwork=1 and printout
c     of lsvmin and lwkmin.
c
c
c **********************************************************************
c
c     OUTPUT FROM EXECUTING CODE IN THIS FILE
C
c **********************************************************************
c
c     EQUALLY SPACED TO GAUSSIAN GRID TRANSFER
c
c     trssph input parameters:
c     intl =  0
c     igride(1) = -1   igride(2) =  1
c     nlone =  36   nlate =  19
c     igridg(1) =  2   igridg(2) =  0
c     nlong = 194   nlatg =  92
c     lsave =   22213  lwork =   53347
c
c     trssph output:
c     ier =  0  lsvmin =   22213  lwkmin =   53347
c     *** 32 BIT ARITHMETIC
c     least squares error =  0.201E-06
c     *** 64 BIT ARITHMETIC
c     least squares error =  0.763E-11
c
c
c     GAUSSIAN TO EQUALLY SPACED GRID TRANSFER
c
c     trssph input parameters:
c     intl =  0
c     igridg(1) =  2   igridg(2) =  0
c     nlong = 194   nlatg =  92
c     igride(1) = -1   igride(2) =  1
c     nlone =  36   nlate =  19
c     lsave =   22213  lwork =   53347
c
c     trssph output:
c     ier =  0  lsvmin =   22213  lwkmin =   53347
c     *** 32 BIT ARITHMETIC
c     least squares error =  0.618E-06
c     *** 64 BIT ARITHMETIC
c     least squares error =  0.547E-11
c
C **********************************************************************
c
c     END OF OUTPUT ... CODE FOLLOWS
c
c **********************************************************************
C
      program testrssph
      implicit none
c
c     set grid sizes with parameter statements
c
      integer nnlatg,nnlong,nnlate,nnlone,llwork,llsave,lldwork
      parameter (nnlatg=92, nnlong=194, nnlate=19,nnlone=36)
c
c     set predetermined minimum saved and unsaved work space lengths
c
      parameter (llwork = 53347, llsave = 22213)
c
c     set double precision work space lengt (see shagci.f documentation)
c
      parameter (lldwork = nnlatg*(nnlatg+4))
c
c     dimension and type data arrays and grid vectors and internal variables
c
      real DATAE(nnlate,nnlone), DATAG(nnlong,nnlatg)
      real work(llwork),wsave(llsave),thetag(nnlatg)
      double precision dwork(lldwork)
      double precision dtheta(nnlatg),dwts(nnlatg)
      integer igride(2),igridg(2)
      integer nlatg,nlong,nlate,nlone,lwork,lsave,ldwork
      real pi,dlate,dlone,dlong,cp,sp,ct,st,xyz,err2,t,p,dif
      integer i,j,intl,ier,lsvmin,lwkmin
c
c     set grid sizes and dimensions from parameter statements
c
      nlatg = nnlatg
      nlong = nnlong
      nlate = nnlate
      nlone = nnlone
      lwork = llwork
      ldwork = lldwork
      lsave = llsave
c
c     set equally spaced grid increments
c
      pi = 4.0*atan(1.0)
      dlate = pi/(nlate-1)
      dlone = (pi+pi)/nlone
      dlong = (pi+pi)/nlong
c
c     set given data in DATAE from f(x,y,z)= exp(x*y*z) restricted
c     to nlate by nlone equally spaced grid on the sphere
c
      do  j=1,nlone
	p = (j-1)*dlone
	cp = cos(p)
	sp = sin(p)
	do i=1,nlate
c
c     set north to south oriented colatitude point
c
	  t = (i-1)*dlate
	  ct = cos(t)
	  st = sin(t)
	  xyz = (st*(st*ct*sp*cp))
	  DATAE(i,j) = exp(xyz)
	end do
      end do
c
c     set initial call flag
c
      intl = 0
c
c     flag DATAE grid as north to south equally spaced
c
      igride(1) = -1
c
c     flag DATAE grid as colatitude by longitude
c
      igride(2) = 1
c
c     flag DATAG grid as south to north Gaussian
c
      igridg(1) = 2
c
c     flag DATAG grid as longitude by latitude
c
      igridg(2) = 0
c
c     print trssph input parameters
c
      write(*,100) intl,igride(1),igride(2),nlone,nlate,
     +igridg(1),igridg(2),nlong,nlatg,lsave,lwork,ldwork
  100 format(//' EQUALLY SPACED TO GAUSSIAN GRID TRANSFER ' ,
     +/' trssph input arguments: ' ,
     +/' intl = ',i2,
     +/' igride(1) = ',i2,2x,' igride(2) = ',i2,
     +/' nlone = ',i3,2x,' nlate = ',i3,
     +/' igridg(1) = ',i2,2x,' igridg(2) = ',i2,
     +/' nlong = ',i3,2x,' nlatg = ',i3,
     +/' lsave = ',i7,2x,' lwork = ',i7,2x,' ldwork = ',i5)
c
c     transfer data from DATAE to DATAG
c
      call trssph(intl,igride,nlone,nlate,datae,igridg,nlong,
     +nlatg,datag,wsave,lsave,lsvmin,work,lwork,lwkmin,dwork,
     +ldwork,ier)
c
c     print output parameters
c
      write (*,200) ier, lsvmin, lwkmin
  200 format(//' trssph output: '
     +/ ' ier = ', i2,2x, 'lsvmin = ',i7, 2x,'lwkmin = ',i7)
      if (ier .eq. 0) then
c
c     compute nlatg gaussian colatitude points using spherepack routine "gaqd"
c     and set in single precision vector thetag with south to north orientation
c     for computing error in DATAG
c
      call gaqd(nlatg,dtheta,dwts,dwork,ldwork,ier)
      do  i=1,nlatg
	thetag(i) = pi-dtheta(i)
      end do
c
c     compute the least squares error in DATAG
c
      err2 = 0.0
      do j=1,nlong
	p = (j-1)*dlong
	cp = cos(p)
	sp = sin(p)
	do i=1,nlatg
	  t = thetag(i)
	  ct = cos(t)
	  st = sin(t)
	  xyz = (st*(st*ct*sp*cp))
	  dif = abs(DATAG(j,i)-exp(xyz))
	  err2 = err2+dif*dif
	end do
      end do
      err2 = sqrt(err2/(nlong*nlatg))
      write (6,300) err2
  300 format(' least squares error = ',e10.3)
      end if
c
c     set DATAE to zero
c
      do j=1,nlone
	do i=1,nlate
	  DATAE(i,j) = 0.0
	end do
      end do

      write(*,400) intl,igridg(1),igridg(2),nlong,nlatg,igride(1),
     +             igride(2),nlone,nlate,lsave,lwork,ldwork
  400 format(/' GAUSSIAN TO EQUALLY SPACED GRID TRANSFER ' ,
     +/' trssph input arguments: ' ,
     +/' intl = ',i2,
     +/' igridg(1) = ',i2,2x,' igridg(2) = ',i2,
     +/' nlong = ',i3,2x,' nlatg = ',i3,
     +/' igride(1) = ',i2,2x,' igride(2) = ',i2,
     +/' nlone = ',i3,2x,' nlate = ',i3,
     +/' lsave = ',i7,2x,'lwork = ',i7, 2x, 'ldwork = ',i7)
c
c     transfer DATAG back to DATAE
c
      CALL TRSSPH(INTL,IGRIDG,NLONG,NLATG,datag,IGRIDE,NLONE,
     +NLATE,datae,WSAVE,LSAVE,LSVMIN,WORK,LWORK,LWKMIN,DWORK,
     +ldwork,ier)
c
c     print output parameters
c
      write (*,200) ier, lsvmin, lwkmin
      if (ier .eq. 0) then
c
c     compute the least squares error in DATAE
c
      err2 = 0.0
      do j=1,nlone
	p = (j-1)*dlone
	cp = cos(p)
	sp = sin(p)
	do i=1,nlate
	  t = (i-1)*dlate
	  ct = cos(t)
	  st = sin(t)
	  xyz = (st*(st*ct*sp*cp))
	  dif = abs(DATAE(i,j)-exp(xyz))
	  err2 = err2+dif*dif
	end do
      end do
      err2 = sqrt(err2/(nlate*nlone))
      write (6,300) err2
      end if
      end
