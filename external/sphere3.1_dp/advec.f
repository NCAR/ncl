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
c
c ... file advec.f
c
c     subroutine advec solves the time-dependent linear advection
c     equation for geopotential phi using the SPHEREPACK 3.0 software
c
c          d(phi)/dt = -(u,v) DOT gradient(phi)
c
c                    = -(u*gdphl + v*gdpht)
c
c ... required files
c
c     gradgc.f,shagc.f,shsgc.f,vhsgc.f,sphcom.f hrfft.f,gaqd.f
c
c
c definitions:
c
c
c     nlat          number of gaussian latitudes excluding poles
c     nlon          number of distinct longitudes
c     omega         rotation rate of earth in radians per second
c     alpha         angle between axis of rotation and the coordinate
c                   axis
c     beta          latitude of the cosine bell
c     aa            radius of earth in meters
c     ncycle        cycle number
c     time          model time in seconds
c     dt            time step
c     lambda        longitude
c     theta         latitude
c
c   the first dimension of the following two dimensional arrays
c   corresponds to the latitude index with values i=1,...,nlat
c   where i=1 is the northern most gaussian point thetag(i)
c   and i=nlat is the southern most gaussian point thetag(nlat).
c   the second dimension is longitude with values j=1,...,nlon
c   where j=1 corresponds to zero longitude and j=nlon corresponds
c   to 2pi minus 2pi/nlon.
c
c
c     thetag(i)    vector of gaussian points on the full sphere which
c                  have north to south orientation as i=1,...,nlat
c
c     u(i,j)       east longitudinal velocity component
c     v(i,j)       latitudinal velocity component
c
c     phi(i,j)     the geopotential at t = time
c
c     phnew(i,j)   the geopotential at t=time+dt
c
c     phold(i,j)   the geopotential at t=time-dt
c
c     gdphl(i,j)   the longitudinal derivative component of
c                  the gradient of phi
c
c                       gdphl = 1/(cos(theta))*d(phi)/dlambda

c
c     gdpht(i,j)   the latitudinal derivative component of
c                  the gradient of phi
c
c                       gdpht = d(phi)/dtheta

c
c   the following two dimensional arrays are nonzero in the triangle
c   n=1,...,nlat and m less than or equal to n.
c
c     ar(m,n),br(m,n)    spectral coefficients of phi
c
      PROGRAM ADVEC
      DOUBLE PRECISION DDT
      DOUBLE PRECISION PI
      DOUBLE PRECISION OMEGA
      DOUBLE PRECISION P0
      DOUBLE PRECISION RE
      DOUBLE PRECISION HZERO
      DOUBLE PRECISION ALPHAD
      DOUBLE PRECISION ALPHA
      DOUBLE PRECISION BETA
      DOUBLE PRECISION DT
      DOUBLE PRECISION TDT
      DOUBLE PRECISION CA
      DOUBLE PRECISION SA
      DOUBLE PRECISION DLON
      DOUBLE PRECISION XLM
      DOUBLE PRECISION SL
      DOUBLE PRECISION CL
      DOUBLE PRECISION ST
      DOUBLE PRECISION CT
      DOUBLE PRECISION STH
      DOUBLE PRECISION CTHCLH
      DOUBLE PRECISION CTHSLH
      DOUBLE PRECISION XLHAT
      DOUBLE PRECISION DATANXY
      DOUBLE PRECISION CLH
      DOUBLE PRECISION SLH
      DOUBLE PRECISION CTH
      DOUBLE PRECISION UHAT
      DOUBLE PRECISION P2
      DOUBLE PRECISION PMAX
      DOUBLE PRECISION TIME
      DOUBLE PRECISION ERR2
      DOUBLE PRECISION ERRM
      DOUBLE PRECISION HTIME
c
c     set grid size with parameter statements
c
      INTEGER NNLAT,NNLON,NN15,LLWORK,LLDWORK,LLVHSGC,LLSHAGC
      INTEGER NLAT,NLON,LWORK,LDWORK,LVHSGC,LSHAGC

c     parameter (nnlat=12,nnlon=23,ddt=1200.)
      PARAMETER (NNLAT=23,NNLON=45,DDT=600.D0)
c     parameter (nnlat=45,nnlon=90,ddt=300.)
c
c     set saved and unsaved work space lengths in terms of nnlat,nnlon
c     (see documentation for shagc,vhsgc,vhsgci,gradgc for estimates)
c
      PARAMETER (NN15=NNLON+15)
      PARAMETER (LLWORK=4*NNLAT*NNLON+2*NNLAT* (NNLAT+1))
      PARAMETER (LLDWORK=2*NNLAT* (NNLAT+1)+1)
      PARAMETER (LLVHSGC=7*NNLAT*NNLAT+NNLON+15)
      PARAMETER (LLSHAGC=5*NNLAT*NNLAT+NNLON+15)
c
c     dimension arrays
c
      DOUBLE PRECISION U(NNLAT,NNLON),V(NNLAT,NNLON)
      DOUBLE PRECISION PHOLD(NNLAT,NNLON),PHNEW(NNLAT,NNLON),
     +                 PHI(NNLAT,NNLON)
      DOUBLE PRECISION PEXACT(NNLAT,NNLON)
      DOUBLE PRECISION DPDT(NNLAT,NNLON)
      DOUBLE PRECISION GDPHL(NNLAT,NNLON),GDPHT(NNLAT,NNLON),
     +                 WORK(LLWORK)
C*PT*WARNING* Already double-precision
      DOUBLE PRECISION DWORK(LLDWORK)
      DOUBLE PRECISION WSHAGC(LLSHAGC),WVHSGC(LLVHSGC),WSHSGC(LLSHAGC)
      DOUBLE PRECISION AR(NNLAT,NNLAT),BR(NNLAT,NNLAT)
      DOUBLE PRECISION THETAG(NNLAT),COLAT(NNLAT)
C*PT*WARNING* Already double-precision
      DOUBLE PRECISION DTHETA(NNLAT),DWTS(NNLAT)
c
c     set constants
c
      PI = 4.0D0*ATAN(1.0D0)
      OMEGA = (PI+PI)/ (12.D0*24.D0*3600.D0)
      P0 = 1000.D0
      RE = 1.0D0/3.0D0
      HZERO = 1000.D0
      ALPHAD = 60.D0
      ALPHA = PI*ALPHAD/180.D0
      BETA = PI/6.D0
c
c     set one array and no equatorial symmetry
c
      NT = 1
      ISYM = 0
c
c     set time step depending on resolution
c
      DT = DDT
      TDT = DT + DT
c
c     set work space length arguments
c
      LWORK = LLWORK
      LDWORK = LLDWORK
      LSHAGC = LLSHAGC
      LVHSGC = LLVHSGC
c
c     set grid size arguments
c
      NLAT = NNLAT
      NLON = NNLON
c
c     compute nlat latitudinal gaussian points in thetag  with
c     north to south orientation using gaqd from SPHEREPACK3.0
c
      CALL DGAQD(NLAT,DTHETA,DWTS,DWORK,LDWORK,IER)
      DO I = 1,NLAT
          THETAG(I) = 0.5D0*PI - DTHETA(I)
          COLAT(I) = DTHETA(I)
      END DO
c
c     preset saved work spaces for gradgc and shagc and shsgc
c
      CALL DVHSGCI(NLAT,NLON,WVHSGC,LVHSGC,DWORK,LDWORK,IERROR)
      IF (IERROR.NE.0) WRITE (*,FMT=10) IERROR
   10 FORMAT (' error in vsgci = ',i5)
      CALL DSHAGCI(NLAT,NLON,WSHAGC,LSHAGC,DWORK,LDWORK,IERROR)
      IF (IERROR.NE.0) WRITE (*,FMT=20) IERROR
   20 FORMAT (' error in shagci = ',i5)
      CALL DSHSGCI(NLAT,NLON,WSHSGC,LSHAGC,DWORK,LDWORK,IERROR)
      IF (IERROR.NE.0) WRITE (*,FMT=21) IERROR
   21 FORMAT (' error in shsgci = ',i5)
c
c     set vector velocities and cosine bell in geopotential
c
      CA = COS(ALPHA)
      SA = SIN(ALPHA)
      DLON = (PI+PI)/NLON
      DO J = 1,NLON
          XLM = (J-1)*DLON
          SL = SIN(XLM)
          CL = COS(XLM)
          DO I = 1,NLAT
              ST = COS(COLAT(I))
              CT = SIN(COLAT(I))
              STH = CA*ST + SA*CT*CL
              CTHCLH = CA*CT*CL - SA*ST
              CTHSLH = CT*SL
              XLHAT = DATANXY(CTHCLH,CTHSLH)
              CLH = COS(XLHAT)
              SLH = SIN(XLHAT)
              CTH = CLH*CTHCLH + SLH*CTHSLH
              UHAT = OMEGA*CTH
              U(I,J) = (CA*SL*SLH+CL*CLH)*UHAT
              V(I,J) = (CA*ST*CL*SLH-ST*SL*CLH+SA*CT*SLH)*UHAT
          END DO
      END DO
c
c       compute geopotential at t=-dt in phold and at t=0.0 in phi
c       to start up leapfrog scheme
c
      CALL DGPOT(-DT,ALPHA,BETA,OMEGA,HZERO,RE,NLAT,NLON,NLAT,COLAT,
     +          PHOLD)
      CALL DGPOT(0.D0,ALPHA,BETA,OMEGA,HZERO,RE,NLAT,NLON,NLAT,COLAT,
     +           PHI)
c
C*PL*ERROR* Comment line too long
c     smooth geopotential at t=-dt and t=0. by synthesizing after analysis
c
      CALL DSHAGC(NLAT,NLON,ISYM,NT,PHOLD,NLAT,NLON,AR,BR,NLAT,NLAT,
     +           WSHAGC,LSHAGC,WORK,LWORK,IERROR)
      IF (IERROR.NE.0) WRITE (*,FMT=26) IERROR
      CALL DSHSGC(NLAT,NLON,ISYM,NT,PHOLD,NLAT,NLON,AR,BR,NLAT,NLAT,
     +            WSHSGC,LSHAGC,WORK,LWORK,IERROR)
      IF (IERROR.NE.0) WRITE (*,FMT=28) IERROR
      CALL DSHAGC(NLAT,NLON,ISYM,NT,PHI,NLAT,NLON,AR,BR,NLAT,NLAT,
     +            WSHAGC,LSHAGC,WORK,LWORK,IERROR)
      IF (IERROR.NE.0) WRITE (*,FMT=26) IERROR
      CALL DSHSGC(NLAT,NLON,ISYM,NT,PHI,NLAT,NLON,AR,BR,NLAT,NLAT,
     +            WSHSGC,LSHAGC,WORK,LWORK,IERROR)
      IF (IERROR.NE.0) WRITE (*,FMT=28) IERROR
   28 FORMAT (' ierror in shsgc = ',i5)
c
c     compute l2 and max norms of geopotential at t=0.
c
      P2 = 0.0D0
      PMAX = 0.0D0
      DO J = 1,NLON
          DO I = 1,NLAT
              PMAX = DMAX1(ABS(PHI(I,J)),PMAX)
              P2 = P2 + PHI(I,J)**2
          END DO
      END DO
      P2 = SQRT(P2)
c
c     set number of time steps for 12 days
c     (time to circumvent the earth)
c
      NTIME = INT((12.D0*24.D0*3600.D0)/DT+0.5D0)
      MPRINT = NTIME/12
      TIME = 0.0D0
      NCYCLE = 0
      DO K = 1,NTIME + 1
c
c       compute harmonic coefficients for phi at current time
c
          CALL DSHAGC(NLAT,NLON,ISYM,NT,PHI,NLAT,NLON,AR,BR,NLAT,NLAT,
     +               WSHAGC,LSHAGC,WORK,LWORK,IERROR)
          IF (IERROR.NE.0) WRITE (*,FMT=26) IERROR
   26     FORMAT (' ierror in shagc = ',i5)
c
c       compute gradient of phi at current time
c
          CALL DGRADGC(NLAT,NLON,ISYM,NT,GDPHT,GDPHL,NLAT,NLON,AR,BR,
     +                NLAT,NLAT,WVHSGC,LVHSGC,WORK,LWORK,IERROR)
          IF (IERROR.NE.0) WRITE (*,FMT=27) IERROR
   27     FORMAT (' ierror in gradgc = ',i5)
c
c       compute the time derivative of phi, note that the sign
c       of the last term is positive because the gradient is
C       computed with respect to colatitude rather than latitude.
c
          DO J = 1,NLON
              DO I = 1,NLAT
                  DPDT(I,J) = -U(I,J)*GDPHL(I,J) + V(I,J)*GDPHT(I,J)
              END DO
          END DO
c
          IF (MOD(NCYCLE,MPRINT).EQ.0) THEN
c
c     write variables
c
              ERR2 = 0.0D0
              ERRM = 0.0D0
              CALL DGPOT(TIME,ALPHA,BETA,OMEGA,HZERO,RE,NLAT,NLON,NLAT,
     +                  COLAT,PEXACT)
              DO J = 1,NLON
                  DO I = 1,NLAT
                      ERR2 = ERR2 + (PEXACT(I,J)-PHI(I,J))**2
                      ERRM = DMAX1(ABS(PEXACT(I,J)-PHI(I,J)),ERRM)
                  END DO
              END DO
              ERRM = ERRM/PMAX
              ERR2 = SQRT(ERR2)/P2
              HTIME = TIME/3600.D0
              WRITE (*,FMT=390) NCYCLE,HTIME,DT,NLAT,NLON,OMEGA,HZERO,
     +          ALPHAD,ERRM,ERR2
  390         FORMAT (/,/,' advecting cosine bell, test case 2',/,
     +               ' cycle number              ',i10,
     +               ' model time in  hours      ',f10.2,/,
     +               ' time step in seconds      ',f10.0,
     +               ' number of latitudes       ',i10,/,
     +               ' number of longitudes      ',i10,
     +               ' rotation rate        ',1p,D15.6,/,
     +               ' mean height          ',1p,D15.6,
     +               ' tilt angle                ',0p,f10.2,/,
     +               ' max geopot. error    ',1p,D15.6,
     +               ' RMS geopot. error    ',1p,D15.6)

          END IF
          TIME = TIME + DT
          NCYCLE = NCYCLE + 1
c
c       update phold,phi for next time step
c
          DO J = 1,NLON
              DO I = 1,NLAT
                  PHNEW(I,J) = PHOLD(I,J) + TDT*DPDT(I,J)
                  PHOLD(I,J) = PHI(I,J)
                  PHI(I,J) = PHNEW(I,J)
              END DO
          END DO
c
c     end of time loop
c
      END DO
      END
      SUBROUTINE DGPOT(T,ALPHA,BETA,OMEGA,HZERO,RE,NLAT,NLON,IDIM,COLAT,
     +                H)
      DOUBLE PRECISION T
      DOUBLE PRECISION ALPHA
      DOUBLE PRECISION BETA
      DOUBLE PRECISION OMEGA
      DOUBLE PRECISION HZERO
      DOUBLE PRECISION RE
      DOUBLE PRECISION COLAT
      DOUBLE PRECISION H
      DOUBLE PRECISION XC
      DOUBLE PRECISION YC
      DOUBLE PRECISION ZC
      DOUBLE PRECISION CA
      DOUBLE PRECISION SA
      DOUBLE PRECISION PI
      DOUBLE PRECISION TPI
      DOUBLE PRECISION DLON
      DOUBLE PRECISION CL
      DOUBLE PRECISION SL
      DOUBLE PRECISION THETA
      DOUBLE PRECISION ST
      DOUBLE PRECISION CT
      DOUBLE PRECISION STH
      DOUBLE PRECISION CTHCLH
      DOUBLE PRECISION CTHSLH
      DOUBLE PRECISION DATANXY
      DOUBLE PRECISION CLH
      DOUBLE PRECISION SLH
      DOUBLE PRECISION CTH
      DOUBLE PRECISION THAT
      DOUBLE PRECISION X1
      DOUBLE PRECISION Y1
      DOUBLE PRECISION Z1
      DOUBLE PRECISION DIST
      DOUBLE PRECISION R
c
c     computes advecting cosine bell on a tilted grid a time t.
c
c input parameters
c
c     t      time in seconds
c
c     alpha  tilt angle in radians
c
c     beta   colatitude of cosine bell in untilted coordinate
c            system in radians
c
c     omega  angular velocity in radians per second
c
c     hzero  maximum value of cosine bell
c
c     re     radius of support for cosine bell in radians
c
c     nlat   number of latitudes including the poles
c
c     nlon   number of distinct longitude lines
c
c     idim   first dimension of output array h
c
c     colat  vector of Gauss colatitude grid points
c
c output parameter
c
c     h      an nlat by nlon array containing the geopotential
c
c             on a tilted grid
c
      DIMENSION H(IDIM,NLON),COLAT(NLAT)
      DOUBLE PRECISION LAMBDA,LAMBDC,LHAT

      LAMBDC = OMEGA*T
      CALL DSTOC(1.D0,BETA,LAMBDC,XC,YC,ZC)
      CA = COS(ALPHA)
      SA = SIN(ALPHA)
      PI = 4.D0*ATAN(1.D0)
      TPI = PI + PI
      DLON = TPI/NLON
      DO 10 J = 1,NLON
          LAMBDA = (J-1)*DLON
          CL = COS(LAMBDA)
          SL = SIN(LAMBDA)
          DO 10 I = 1,NLAT
              THETA = COLAT(I)
              ST = COS(THETA)
              CT = SIN(THETA)
              STH = CA*ST + SA*CT*CL
              CTHCLH = CA*CT*CL - SA*ST
              CTHSLH = CT*SL
              LHAT = DATANXY(CTHCLH,CTHSLH)
              CLH = COS(LHAT)
              SLH = SIN(LHAT)
              CTH = CLH*CTHCLH + SLH*CTHSLH
              THAT = DATANXY(STH,CTH)
              CALL DSTOC(1.D0,THAT,LHAT,X1,Y1,Z1)
              DIST = SQRT((X1-XC)**2+ (Y1-YC)**2+ (Z1-ZC)**2)
              H(I,J) = 0.D0
              IF (DIST.GE.RE) GO TO 10
              R = 2.D0*ASIN(DIST/2.D0)
              IF (R.GE.RE) GO TO 10
              H(I,J) = HZERO*.5D0* (COS(R*PI/RE)+1.D0)
   10 CONTINUE
      RETURN
      END
      FUNCTION DATANXY(X,Y)
      DOUBLE PRECISION DATANXY
      DOUBLE PRECISION X
      DOUBLE PRECISION Y

      DATANXY = 0.D0
      IF (X.EQ.0.D0 .AND. Y.EQ.0.D0) RETURN
      DATANXY = ATAN2(Y,X)
      RETURN
      END
      SUBROUTINE DCTOS(X,Y,Z,R,THETA,PHI)
      DOUBLE PRECISION X
      DOUBLE PRECISION Y
      DOUBLE PRECISION Z
      DOUBLE PRECISION R
      DOUBLE PRECISION THETA
      DOUBLE PRECISION PHI
      DOUBLE PRECISION R1

      R1 = X*X + Y*Y
      IF (R1.NE.0.D0) GO TO 10
      PHI = 0.D0
      THETA = 0.D0
      IF (Z.LT.0.D0) THETA = 4.D0*ATAN(1.D0)
      RETURN
   10 R = SQRT(R1+Z*Z)
      R1 = SQRT(R1)
      PHI = ATAN2(Y,X)
      THETA = ATAN2(R1,Z)
      RETURN
      END
      SUBROUTINE DSTOC(R,THETA,PHI,X,Y,Z)
      DOUBLE PRECISION R
      DOUBLE PRECISION THETA
      DOUBLE PRECISION PHI
      DOUBLE PRECISION X
      DOUBLE PRECISION Y
      DOUBLE PRECISION Z
      DOUBLE PRECISION ST

      ST = SIN(THETA)
      X = R*ST*COS(PHI)
      Y = R*ST*SIN(PHI)
      Z = R*COS(THETA)
      RETURN
      END
