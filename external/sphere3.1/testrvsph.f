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
c  .                         SPHEREPACK                          .
c  .                                                             .
c  . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .
c
c
c ... file testrvsph.f
c
c     this file contains a test program for trvsph.f
c
c ... required files
c
c     trvsph.f, sphcom.f, hrfft.f, gaqd.f, vhaec.f, vhsec.f, vhagc.f, vhsgc.f
c
c ... description (see documentation in file trvsph.f)
c
c     Subroutine trvsph is used to demonstrate vector data transfer between a coarse
c     ten degree equally spaced grid and a higher resolution T42 Global Spectral
c     Gaussian grid.
c
c     Ten Degree Grid (Mathematical Spherical Coordinates)
c
c     The equally spaced vector data, given in mathematical spherical
c     coordinates,  is stored in 19 X 36 colatitude-longitude arrays (ue,ve).  The
c     colatitudinal values are stored north to south with increasing colatitude
c     subscript values.  ue is the east longitudinal component and ve is the
c     colatitudinal component of the vector.
c
c     Gaussian Grid (Geophysical Spherical Coordinates)
c
c     The T42 Gaussian grid vector data is in geophysical spherical coordinates.
c     It is stored in 128 X 64 longitude-latitude arrays (ug,vg).  Values are
c     stored south to north with increasing latitude subscript value.  ug
c     is the longitudinal component and vg is the latitudinal componenet of the
c     vector data.
c
c     For testing purposes we use an analytic vector field (u,v).  Let t and p
c     be colatitude and longitude and x=sin(t)cos(p), y=sint(t)*sin(p), z=cos(t)
c     be the cartesian coordinates restricted to the sphere in mathematical
c     coordinates. We derive the vector field (u,v) from the stream function
c     S and velocity function P given by
c
c               y    -z           x    z
c          S = e  + e  ,     P = e  + e
c
c     The corresponding vector field has the form
c
c                x          -z          y
c          u = -e sin(p) + e  sin(t) + e cos(t)sin(p)
c
c                y          z          x
c          v = -e cos(p) - e sin(t) + e cos(t)cos(p)
c
c
c     in mathematical spherical coordinates.  Values in geophysical
c     coordinates can be obtained by negating v.
c
c     In the code below, (ue,ve) is set equal to (u,v) and trvsph is used to
c     transfer (ue,ve) to (ug,vg).  (ug,vg) is then compared with (u,v) in
c     geophysical coordinates on the T42 Gaussian grid.  Finally, trvsph is
c     used to transfer (ug,vg) back to (ue,vg) and this is compared with (u,v).
c     Output from executing the test program on separate platforms with 32 bit
c     and 64 bit floating point arithmetic is listed below.  The minimum required
c     saved and unsaved work space lengths were predetermined by an earlier call
c     trvsph with nlone=36,nlate=19,nlong=128,nlatg=64,lsave=0,lwork=0 and printout
c     of lsvmin and lwkmin.
c
c
c **********************************************************************
c
c     OUTPUT FROM EXECUTING CODE IN THIS FILE
C
c **********************************************************************
c
c      EQUALLY SPACED TO GAUSSIAN GRID TRANSFER
c      trvsph input arguments:
c      intl =  0
c      igride(1) = -1   igride(2) =  1
c      nlone =  36   nlate =  19
c      ive =  1
c      igridg(1) =  2   igridg(2) =  0
c      nlong = 128   nlatg =  64
c      ivg =  0
c      lsave =   21814   lwork =   75173
c
c      trvsph output:
c      ier =        0  lsvmin =   21792  lwkmin =   75173
C ***  32 BIT FLOATING POINT ARITHMETIC
c      least squares error in u =  0.307E-06
c      least squares error in v =  0.272E-06
C ***  64 BIT FLOATING POINT ARITHMETIC
c      least squares error in u =  0.841E-12
c      least squares error in v =  0.603E-12
c
c      GAUSSIAN TO EQUALLY SPACED GRID TRANSFER
c      trvsph input arguments:
c      intl =  0
c      igridg(1) =  2   igridg(2) =  0
c      nlong = 128   nlatg =  64
c      ivg =  0
c      igride(1) = -1   igride(2) =  1
c      nlone =  36   nlate =  19
c      ive =  1
c      lsave =   21814   lwork =   75173
c
c      trvsph output:
c      ier =        0  lsvmin =   21814  lwkmin =   75173
C ***  32 BIT FLOATING POINT ARITHMETIC
c      least squares error in u =  0.374E-06
c      least squares error in v =  0.364E-06
C ***  64 BIT FLOATING POINT ARITHMETIC
c      least squares error in u =  0.170E-12
c      least squares error in v =  0.161E-12
c
C **********************************************************************
c
c     END OF OUTPUT ... CODE FOLLOWS
c
c **********************************************************************
      program testrvsph
      implicit none
c
c     set grid sizes with parameter statements
c
      integer nnlatg,nnlong,nnlate,nnlone,llwork,llsave,lldwork
      parameter (nnlatg=64, nnlong=128, nnlate=19,nnlone=36)
c
c     set predetermined minimum saved and unsaved work space lengths
c
      parameter (llwork =  76000)
      parameter (llsave = 21814)
c
c     set double precision work space length
c
      parameter (lldwork = 2*nnlatg*(nnlatg+1)+1)
c
c     dimension and type data arrays and grid vectors and internal variables
c
      real ue(nnlate,nnlone), ve(nnlate,nnlone)
      real ug(nnlong,nnlatg), vg(nnlong,nnlatg)
      real work(llwork),wsave(llsave),thetag(nnlatg)
      DOUBLE PRECISION DTHETA(NNLATG),DWTS(NNLATG),DWORK(LLDWORK)
      integer igride(2),igridg(2),ive,ivg
      integer nlatg,nlong,nlate,nlone,lwork,lsave,ldwork
      real pi,dlate,dlone,dlong,t,p,cosp,sinp,cost,sint,x,y,z
      real erru2,errv2,ex,ey,ez,emz,uee,vee
      integer i,j,ib,intl,ier,lsvmin,lwkmin
c
c     set grid sizes and dimensions from parameter statements
c
      nlatg = nnlatg
      nlong = nnlong
      nlate = nnlate
      nlone = nnlone
      lwork = llwork
      lsave = llsave
      ldwork = lldwork
c
c     set equally spaced grid increments
c
      pi = 4.0*atan(1.0)
      dlate = pi/(nlate-1)
      dlone = (pi+pi)/nlone
      dlong = (pi+pi)/nlong
c
c     set vector data in (ue,ve)
c
      do  j=1,nlone
	p = (j-1)*dlone
	cosp = cos(p)
	sinp = sin(p)
	do i=1,nlate
	  t = (i-1)*dlate
	  cost = cos(t)
	  sint = sin(t)
	  x = sint*cosp
	  y = sint*sinp
	  z = cost
	  ex = exp(x)
	  ey = exp(y)
	  ez = exp(z)
	  emz = exp(-z)
	  ve(i,j) = ex*cost*cosp-ey*cosp-ez*sint
	  ue(i,j) = -ex*sinp + emz*sint + ey*cost*sinp
	end do
      end do
c
c     set initial call flag
c
      intl = 0
c
c     flag (ue,ve) grid as north to south equally spaced
c
      igride(1) = -1
c
c     flag (ue,ve) as nlate by nlone arrays
c
      igride(2) = 1
c
c     flag ve as colatitude component of vector
      ive = 1
c
c     flag (ug,vg) as south to north gaussian
c
      igridg(1) = +2
c
c     flag (ug,vg) as nlong by nlatg arrays
c
      igridg(2) = 0
c
c     flag vg as latitude component of vector
      ivg = 0
c
c     print trvsph input arguments
c
      write(*,100) intl,igride(1),igride(2),nlone,nlate,ive,
     +igridg(1),igridg(2),nlong,nlatg,ivg,lsave,lwork
  100 format(//' EQUALLY SPACED TO GAUSSIAN GRID TRANSFER ' ,
     +/' trvsph input arguments: ' ,
     +/' intl = ',i2,
     +/' igride(1) = ',i2,2x,' igride(2) = ',i2,
     +/' nlone = ',i3,2x,' nlate = ',i3,
     +/' ive = ',i2,
     +/' igridg(1) = ',i2,2x,' igridg(2) = ',i2,
     +/' nlong = ',i3,2x,' nlatg = ',i3,
     +/' ivg = ',i2
     +/' lsave = ',i7,2x,' lwork = ',i7)
c
c     transfer  (ue,ve) to (ug,vg)
c
      call trvsph(intl,igride,nlone,nlate,ive,ue,ve,igridg,nlong,
     +nlatg,ivg,ug,vg,wsave,lsave,lsvmin,work,lwork,lwkmin,dwork,ier)
c
c     print output arguments
c
      write (*,200) ier, lsvmin, lwkmin
  200 format(//' trvsph output: '
     +/ ' ier = ', i8,2x, 'lsvmin = ',i7, 2x,'lwkmin = ',i7)

      if (ier .eq. 0) then
c
c     compute nlatg gaussian colatitude points and
c     set with south to north orientation in thetag
c
      call gaqd(nlatg,dtheta,dwts,dwork,ldwork,ier)
      do  i=1,nlatg
	ib = nlatg-i+1
	thetag(i) = dtheta(ib)
      end do
c
c     compute the least squares error in (ug,vg)
c     by comparing with exact geophysical vector
c
      errv2 = 0.0
      erru2 = 0.0
      do  j=1,nlong
	p = (j-1)*dlong
	cosp = cos(p)
	sinp = sin(p)
	do i=1,nlatg
	  t = thetag(i)
	  cost = cos(t)
	  sint = sin(t)
	  x = sint*cosp
	  y = sint*sinp
	  z = cost
	  ex = exp(x)
	  ey = exp(y)
	  ez = exp(z)
	  emz = exp(-z)
	  vee = -ex*cost*cosp+ey*cosp+ez*sint
	  uee = -ex*sinp + emz*sint + ey*cost*sinp
	  erru2 = erru2 + (ug(j,i)-uee)**2
	  errv2 = errv2 + (vg(j,i)-vee)**2
	end do
      end do
      erru2 = sqrt(erru2/(nlong*nlatg))
      errv2 = sqrt(errv2/(nlong*nlatg))
      write (6,300) erru2, errv2
  300 format(' least squares error in u = ', e10.3
     +      /' least squares error in v = ', e10.3)
      end if
c
c     now transfer (ug,vg) back to (ue,ve)
c
      do j=1,nlone
	do i=1,nlate
	  ue(i,j) = 0.0
	  ve(i,j) = 0.0
      end do
      end do
      write(*,101) intl,igridg(1),igridg(2),nlong,nlatg,ivg,
     +igride(1),igride(2),nlone,nlate,ive,lsave,lwork,ldwork
  101 format(//' GAUSSIAN TO EQUALLY SPACED GRID TRANSFER ' ,
     +/' trvsph input arguments: ' ,
     +/' intl = ',i2,
     +/' igridg(1) = ',i2,2x,' igridg(2) = ',i2,
     +/' nlong = ',i3,2x,' nlatg = ',i3,
     +/' ivg = ',i2,
     +/' igride(1) = ',i2,2x,' igride(2) = ',i2,
     +/' nlone = ',i3,2x,' nlate = ',i3,
     +/' ive = ',i2
     +/' lsave = ',i7,2x,' lwork = ',i7,2x,' ldwork = ',i5)
      call trvsph(intl,igridg,nlong,nlatg,ivg,ug,vg,igride,nlone,nlate,
     +ive,ue,ve,wsave,lsave,lsvmin,work,lwork,lwkmin,dwork,ldwork,ier)
c
c     print output arguments
c
      write (*,200) ier, lsvmin, lwkmin
      if (ier .eq. 0) then
c
c     compute the least squares error in (ue,ve)
c     by comparing with exact mathematical vector
c
      errv2 = 0.0
      erru2 = 0.0
      do  j=1,nlone
	p = (j-1)*dlone
	cosp = cos(p)
	sinp = sin(p)
	do i=1,nlate
	  t = (i-1)*dlate
	  cost = cos(t)
	  sint = sin(t)
	  x = sint*cosp
	  y = sint*sinp
	  z = cost
	  ex = exp(x)
	  ey = exp(y)
	  ez = exp(z)
	  emz = exp(-z)
	  vee =  ex*cost*cosp-ey*cosp-ez*sint
	  uee = -ex*sinp + emz*sint + ey*cost*sinp
	  erru2 = erru2 + (ue(i,j)-uee)**2
	  errv2 = errv2 + (ve(i,j)-vee)**2
	end do
      end do
      erru2 = sqrt(erru2/(nlone*nlate))
      errv2 = sqrt(errv2/(nlone*nlate))
      write (6,300) erru2, errv2
      end if
      end
