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
      PROGRAM HELMSPH
c
c     set grid size with parameter statements
c
      IMPLICIT NONE
      INTEGER NNLAT,NNLON,NN15,LLSAVE,LLWORK,LLDWORK
      PARAMETER (NNLAT=19,NNLON=36)
c
c     set saved and unsaved work space lengths in terms of nnlat,nnlon
c     (see documentation for shaec,shsec,islapec)
c
      PARAMETER (NN15=NNLON+15)
      PARAMETER (LLSAVE=NNLAT* (NNLAT+1)+3* ((NNLAT-2)* (NNLAT-1)+NN15))
      PARAMETER (LLWORK=NNLAT* (2*NNLON+3* (NNLAT+1)+2*NNLAT+1))
c
c     set double precision work space length for initializations
c
      PARAMETER (LLDWORK=NNLAT+1)
c
c     dimension arrays
c
      DOUBLE PRECISION U(NNLAT,NNLON),R(NNLAT,NNLON)
      DOUBLE PRECISION SINT(NNLAT),COST(NNLAT),SINP(NNLON),COSP(NNLON)
      DOUBLE PRECISION WORK(LLWORK),WSHAEC(LLSAVE),WSHSEC(LLSAVE)
C*PT*WARNING* Already double-precision
      DOUBLE PRECISION DWORK(LLDWORK)
      DOUBLE PRECISION A(NNLAT,NNLAT),B(NNLAT,NNLAT)
      INTEGER NLAT,NLON,I,J,LSHAEC,LSHSEC,LWORK,IERROR,ISYM,NT
      INTEGER LDWORK
      DOUBLE PRECISION PI,X,Y,Z,DLAT,DLON,THETA,PHI,XLMBDA,PERTRB,EZ,UE,
     +                 ERRM

      PI = 4.0D0*ATAN(1.0D0)
c
c     set helmholtz constant
c
      XLMBDA = 1.0D0
c
c     set work space length arguments
c
      LWORK = LLWORK
      LDWORK = LLDWORK
      LSHAEC = LLSAVE
      LSHSEC = LLSAVE
c
c     set grid size arguments
c
      NLAT = NNLAT
      NLON = NNLON
c
c     set sine and cosine vectors
c
      DLAT = PI/ (NLAT-1)
      DLON = (PI+PI)/NLON
      DO I = 1,NLAT
          THETA = -0.5D0*PI + (I-1)*DLAT
          SINT(I) = SIN(THETA)
          COST(I) = COS(THETA)
      END DO
      DO J = 1,NLON
          PHI = (J-1)*DLON
          SINP(J) = SIN(PHI)
          COSP(J) = COS(PHI)
      END DO
c
c     set right hand side as helmholtz operator
c     applied to ue = (1.+x*y)*exp(z)
c
      DO J = 1,NLON
          DO I = 1,NLAT
              X = COST(I)*COSP(J)
              Y = COST(I)*SINP(J)
              Z = SINT(I)
              R(I,J) = - (X*Y* (Z*Z+6.D0* (Z+1.D0))+Z* (Z+2.D0))*EXP(Z)
          END DO
      END DO
c
c     initialize saved work space arrays for scalar harmonic
c     analysis and Helmholtz inversion of r
c
      CALL DSHAECI(NLAT,NLON,WSHAEC,LSHAEC,DWORK,LDWORK,IERROR)
      IF (IERROR.GT.0) THEN
          WRITE (6,FMT=200) IERROR
  200     FORMAT (' shaeci, ierror = ',i2)
          CALL EXIT(0)
      END IF
      CALL DSHSECI(NLAT,NLON,WSHSEC,LSHSEC,DWORK,LDWORK,IERROR)
      IF (IERROR.GT.0) THEN
          WRITE (6,FMT=201) IERROR
  201     FORMAT (' shseci, ierror = ',i2)
          CALL EXIT(0)
      END IF
c
c     set no symmetry and one array
c
      ISYM = 0
      NT = 1
c
c     compute coefficients of r for input to islapec
c
      CALL DSHAEC(NLAT,NLON,ISYM,NT,R,NLAT,NLON,A,B,NLAT,NLAT,WSHAEC,
     +           LSHAEC,WORK,LWORK,IERROR)
      IF (IERROR.GT.0) THEN
          WRITE (*,FMT=202) IERROR
  202     FORMAT (' shaec , ierror = ',i2)
          CALL EXIT(0)
      END IF
c
c     solve Helmholtz equation on the sphere in u
c
      WRITE (6,FMT=100) NLAT,NLON
  100 FORMAT (' helmholtz approximation on a ten degree grid',/,
     +       ' nlat = ',i3,2x,' nlon = ',i3)
      CALL DISLAPEC(NLAT,NLON,ISYM,NT,XLMBDA,U,NLAT,NLON,A,B,NLAT,NLAT,
     +             WSHSEC,LSHSEC,WORK,LWORK,PERTRB,IERROR)
      IF (IERROR.NE.0) THEN
          WRITE (6,FMT=103) IERROR
  103     FORMAT (' islapec, ierror = ',i2)
          IF (IERROR.GT.0) CALL EXIT(0)
      END IF
c
c     compute and print maximum error in u
c
      ERRM = 0.0D0
      DO J = 1,NLON
          DO I = 1,NLAT
              X = COST(I)*COSP(J)
              Y = COST(I)*SINP(J)
              Z = SINT(I)
              EZ = EXP(Z)
              UE = (1.D0+X*Y)*EZ
              ERRM = DMAX1(ERRM,ABS(U(I,J)-UE))
          END DO
      END DO
      WRITE (*,FMT=204) XLMBDA,PERTRB,ERRM
  204 FORMAT (' xlmbda = ',f5.2,2x,' pertrb = ',D10.3,/,
     +       ' maximum error = ',D10.3)
      END
