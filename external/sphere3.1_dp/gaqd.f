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
c  .                          SPHEREPACK                          .
c  .                                                             .
c  . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .
c
c                             August 2003
c
c
c     This version of gaqd implements the method presented in:
c     P. N. swarztrauber, Computing the points and weights for
c     Gauss-Legendre quadrature, SIAM J. Sci. Comput.,
c     24(2002) pp. 945-954.
c
c     It the version that is new to spherepack 3.1
c     The w and lwork arrays are dummy and included only to
c     permit a simple pluggable exchange with the
c     old gaqd in spherepack 3.0.
c
c
c
      SUBROUTINE DGAQD(NLAT,THETA,WTS,W,LWORK,IERROR)
      DOUBLE PRECISION EPS
      DOUBLE PRECISION DDZEPS
      DOUBLE PRECISION SGND
C*PT*WARNING* Already double-precision
c
c  . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .
c  .                                                             .
c  .                  copyright (c) 2001 by ucar                 .
c  .                                                             .
c  .       university corporation for atmospheric research       .
c  .                                                             .
c  .                      all rights reserved                    .
c  .                                                             .
c  . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .
c
c                             February 2002
c
c     gauss points and weights are computed using the fourier-newton
c     described in "on computing the points and weights for
c     gauss-legendre quadrature", paul n. swarztrauber, siam journal
c     on scientific computing that has been accepted for publication.
c     This routine is faster and more accurate than older program
c     with the same name.
c
c     subroutine gaqd computes the nlat gaussian colatitudes and weights
c     in double precision. the colatitudes are in radians and lie in the
c     in the interval (0,pi).
c
c     input parameters
c
c     nlat    the number of gaussian colatitudes in the interval (0,pi)
c             (between the two poles).  nlat must be greater than zero.
c
c     w       unused double precision variable that permits a simple
c             exchange with the old routine with the same name
c             in spherepack.
c
c     lwork   unused variable that permits a simple exchange with the
c             old routine with the same name in spherepack.
c
c     output parameters
c
c     theta   a double precision array with length nlat
c             containing the gaussian colatitudes in
c             increasing radians on the interval (0,pi).
c
c     wts     a double precision array with lenght nlat
c             containing the gaussian weights.
c
c     ierror = 0 no errors
c            = 1 if nlat.le.0
c
c  *****************************************************************
c
      DOUBLE PRECISION THETA(NLAT),WTS(NLAT),W,X,PI,PIS2,DTHETA,DTHALF,
     +                 CMAX,ZPREV,ZLAST,ZERO,ZHOLD,PB,DPB,DCOR,SUM,CZ
c
c     check work space length
c
      IERROR = 1
      IF (NLAT.LE.0) RETURN
      IERROR = 0
c
c     compute weights and points analytically when nlat=1,2
c
      IF (NLAT.EQ.1) THEN
C*PT*WARNING* Constant already double-precision
          THETA(1) = DACOS(0.0d0)
C*PT*WARNING* Constant already double-precision
          WTS(1) = 2.0d0
          RETURN
      END IF
      IF (NLAT.EQ.2) THEN
C*PT*WARNING* Constant already double-precision
C*PT*WARNING* Constant already double-precision
          X = DSQRT(1.0d0/3.0d0)
          THETA(1) = DACOS(X)
          THETA(2) = DACOS(-X)
C*PT*WARNING* Constant already double-precision
          WTS(1) = 1.0d0
C*PT*WARNING* Constant already double-precision
          WTS(2) = 1.0d0
          RETURN
      END IF
C*PT*WARNING* Constant already double-precision
      EPS = SQRT(DDZEPS(1.0d0))
      EPS = EPS*SQRT(EPS)
C*PT*WARNING* Constant already double-precision
C*PT*WARNING* Constant already double-precision
      PIS2 = 2.0d0*DATAN(1.0d0)
      PI = PIS2 + PIS2
      MNLAT = MOD(NLAT,2)
      NS2 = NLAT/2
      NHALF = (NLAT+1)/2
      IDX = NS2 + 2
c
      CALL DCPDP(NLAT,CZ,THETA(NS2+1),WTS(NS2+1))
c
      DTHETA = PIS2/NHALF
C*PT*WARNING* Constant already double-precision
      DTHALF = DTHETA/2.0d0
C*PT*WARNING* Constant already double-precision
      CMAX = .2d0*DTHETA
c
c     estimate first point next to theta = pi/2
c
      IF (MNLAT.NE.0) THEN
          ZERO = PIS2 - DTHETA
          ZPREV = PIS2
          NIX = NHALF - 1
      ELSE
          ZERO = PIS2 - DTHALF
          NIX = NHALF
      END IF
    9 IT = 0
   10 IT = IT + 1
      ZLAST = ZERO
c
c     newton iterations
c
      CALL DTPDP(NLAT,ZERO,CZ,THETA(NS2+1),WTS(NS2+1),PB,DPB)
      DCOR = PB/DPB
      SGND = 1.0D0
C*PT*WARNING* Constant already double-precision
      IF (DCOR.NE.0.0d0) SGND = DCOR/DABS(DCOR)
      DCOR = SGND*MIN(DABS(DCOR),CMAX)
      ZERO = ZERO - DCOR
      IF (DABS(ZERO-ZLAST).GT.EPS*DABS(ZERO)) GO TO 10
      THETA(NIX) = ZERO
      ZHOLD = ZERO
c      wts(nix) = (nlat+nlat+1)/(dpb*dpb)
c
c     yakimiw's formula permits using old pb and dpb
c
      WTS(NIX) = (NLAT+NLAT+1)/ (DPB+PB*DCOS(ZLAST)/DSIN(ZLAST))**2
      NIX = NIX - 1
      IF (NIX.EQ.0) GO TO 30
      IF (NIX.EQ.NHALF-1) ZERO = 3.0D0*ZERO - PI
      IF (NIX.LT.NHALF-1) ZERO = ZERO + ZERO - ZPREV
      ZPREV = ZHOLD
      GO TO 9
c
c     extend points and weights via symmetries
c
   30 IF (MNLAT.NE.0) THEN
          THETA(NHALF) = PIS2
          CALL DTPDP(NLAT,PIS2,CZ,THETA(NS2+1),WTS(NS2+1),PB,DPB)
          WTS(NHALF) = (NLAT+NLAT+1)/ (DPB*DPB)
      END IF
      DO I = 1,NS2
          WTS(NLAT-I+1) = WTS(I)
          THETA(NLAT-I+1) = PI - THETA(I)
      END DO
C*PT*WARNING* Constant already double-precision
      SUM = 0.0d0
      DO I = 1,NLAT
          SUM = SUM + WTS(I)
      END DO
      DO I = 1,NLAT
C*PT*WARNING* Constant already double-precision
          WTS(I) = 2.0d0*WTS(I)/SUM
      END DO
      RETURN
      END
      SUBROUTINE DCPDP(N,CZ,CP,DCP)
C*PT*WARNING* Already double-precision
c
c     computes the fourier coefficients of the legendre
c     polynomial p_n^0 and its derivative.
c     n is the degree and n/2 or (n+1)/2
c     coefficients are returned in cp depending on whether
c     n is even or odd. The same number of coefficients
c     are returned in dcp. For n even the constant
c     coefficient is returned in cz.
c
      DOUBLE PRECISION CP(N/2+1),DCP(N/2+1),T1,T2,T3,T4,CZ

      NCP = (N+1)/2
C*PT*WARNING* Constant already double-precision
      T1 = -1.0d0
C*PT*WARNING* Constant already double-precision
      T2 = N + 1.0d0
C*PT*WARNING* Constant already double-precision
      T3 = 0.0d0
C*PT*WARNING* Constant already double-precision
      T4 = N + N + 1.0d0
      IF (MOD(N,2).EQ.0) THEN
C*PT*WARNING* Constant already double-precision
          CP(NCP) = 1.0d0
          DO J = NCP,2,-1
C*PT*WARNING* Constant already double-precision
              T1 = T1 + 2.0d0
C*PT*WARNING* Constant already double-precision
              T2 = T2 - 1.0d0
C*PT*WARNING* Constant already double-precision
              T3 = T3 + 1.0d0
C*PT*WARNING* Constant already double-precision
              T4 = T4 - 2.0d0
              CP(J-1) = (T1*T2)/ (T3*T4)*CP(J)
          END DO
C*PT*WARNING* Constant already double-precision
          T1 = T1 + 2.0d0
C*PT*WARNING* Constant already double-precision
          T2 = T2 - 1.0d0
C*PT*WARNING* Constant already double-precision
          T3 = T3 + 1.0d0
C*PT*WARNING* Constant already double-precision
          T4 = T4 - 2.0d0
          CZ = (T1*T2)/ (T3*T4)*CP(1)
          DO J = 1,NCP
              DCP(J) = (J+J)*CP(J)
          END DO
      ELSE
C*PT*WARNING* Constant already double-precision
          CP(NCP) = 1.0d0
          DO J = NCP - 1,1,-1
C*PT*WARNING* Constant already double-precision
              T1 = T1 + 2.0d0
C*PT*WARNING* Constant already double-precision
              T2 = T2 - 1.0d0
C*PT*WARNING* Constant already double-precision
              T3 = T3 + 1.0d0
C*PT*WARNING* Constant already double-precision
              T4 = T4 - 2.0d0
              CP(J) = (T1*T2)/ (T3*T4)*CP(J+1)
          END DO
          DO J = 1,NCP
              DCP(J) = (J+J-1)*CP(J)
          END DO
      END IF
      RETURN
      END
      SUBROUTINE DTPDP(N,THETA,CZ,CP,DCP,PB,DPB)
C*PT*WARNING* Already double-precision
c
c     computes pn(theta) and its derivative dpb(theta) with
c     respect to theta
c
      DOUBLE PRECISION CP(N/2+1),DCP(N/2+1),CZ,PB,DPB,FN,THETA,CDT,SDT,
     +                 CTH,STH,CHH
c
      FN = N
      CDT = DCOS(THETA+THETA)
      SDT = DSIN(THETA+THETA)
      IF (MOD(N,2).EQ.0) THEN
c
c     n even
c
          KDO = N/2
C*PT*WARNING* Constant already double-precision
          PB = .5d0*CZ
C*PT*WARNING* Constant already double-precision
          DPB = 0.0d0
          IF (N.GT.0) THEN
              CTH = CDT
              STH = SDT
              DO 170 K = 1,KDO
c      pb = pb+cp(k)*cos(2*k*theta)
                  PB = PB + CP(K)*CTH
c      dpb = dpb-(k+k)*cp(k)*sin(2*k*theta)
                  DPB = DPB - DCP(K)*STH
                  CHH = CDT*CTH - SDT*STH
                  STH = SDT*CTH + CDT*STH
                  CTH = CHH
  170         CONTINUE
          END IF
      ELSE
c
c     n odd
c
          KDO = (N+1)/2
C*PT*WARNING* Constant already double-precision
          PB = 0.0d0
C*PT*WARNING* Constant already double-precision
          DPB = 0.0d0
          CTH = DCOS(THETA)
          STH = DSIN(THETA)
          DO 190 K = 1,KDO
c      pb = pb+cp(k)*cos((2*k-1)*theta)
              PB = PB + CP(K)*CTH
c      dpb = dpb-(k+k-1)*cp(k)*sin((2*k-1)*theta)
              DPB = DPB - DCP(K)*STH
              CHH = CDT*CTH - SDT*STH
              STH = SDT*CTH + CDT*STH
              CTH = CHH
  190     CONTINUE
      END IF
      RETURN
      END
      DOUBLE PRECISION FUNCTION DDZEPS(X)
C*PT*WARNING* Already double-precision
      DOUBLE PRECISION X
C*PT*WARNING* Already double-precision
c
c     estimate unit roundoff in quantities of size x.
c
      DOUBLE PRECISION A,B,C,EPS
C*PT*WARNING* Constant already double-precision
C*PT*WARNING* Constant already double-precision
c
c     this program should function properly on all systems
c     satisfying the following two assumptions,
c        1.  the base used in representing floating point
c            numbers is not a power of three.
c        2.  the quantity  a  in statement 10 is represented to
c            the accuracy used in floating point variables
c            that are stored in memory.
c     the statement number 10 and the go to 10 are intended to
c     force optimizing compilers to generate code satisfying
c     assumption 2.
c     under these assumptions, it should be true that,
c            a  is not exactly equal to four-thirds,
c            b  has a zero for its last bit or digit,
c            c  is not exactly equal to one,
c            eps  measures the separation of 1.0 from
c                 the next larger floating point number.
c     the developers of eispack would appreciate being informed
c     about any systems where these assumptions do not hold.
c
c     this version dated 4/6/83.
c
      A = 4.0d0/3.0d0
C*PT*WARNING* Constant already double-precision
   10 B = A - 1.0d0
      C = B + B + B
C*PT*WARNING* Constant already double-precision
      EPS = ABS(C-1.0d0)
C*PT*WARNING* Constant already double-precision
      IF (EPS.EQ.0.0d0) GO TO 10
      DDZEPS = EPS*DABS(X)
      RETURN
      END
