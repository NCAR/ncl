c
c
c  . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .
c  .                                                             .
c  .                  copyright (c) 1998 by ucar                 .
c  .                                                             .
c  .       university corporation for atmospheric research       .
c  .                                                             .
c  .                      all rights reserved                    .
c  .                                                             .
c  .                                                             .
c  .                         spherepack3.0                       .
c  .                                                             .
c  . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .
c
c
c
C*PL*ERROR* Comment line too long
c ... file gaqd.f  (rewritten July 30, 1999 ... incorporating new method)
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
c     dwork   a double precision temporary work space.
c
c     ldwork  the length of the work space  in the routine calling gaqd
c             ldwork must be at least nlat+1.
c
c     output parameters
c
c     theta   a double precision vector of length nlat containing the
C*PL*ERROR* Comment line too long
c             nlat gaussian colatitudes on the sphere in increasing radians
c             in the interval (0,pi).
c
c     wts     a double precision vector of length nlat containing the
c             nlat gaussian weights.
c
c     ierror = 0 no errors
c            = 1 if ldwork.lt.nlat+1
c            = 2 if nlat.le.0
c            = 3 if unable to compute gaussian points
c                (failure in in the eigenvalue routine)
c
c  *****************************************************************
c
      SUBROUTINE DGAQD(NLAT,THETA,WTS,DWORK,LDWORK,IERROR)
      DIMENSION DWORK(NLAT+1),THETA(NLAT),WTS(NLAT)
C*PT*WARNING* Already double-precision
      DOUBLE PRECISION X,THETA,WTS,DWORK

      IERROR = 1
c
c     check work space length
c
      IF (LDWORK.LT.NLAT+1) RETURN
      IERROR = 2
      IF (NLAT.LE.0) RETURN
      IERROR = 0
c
c     compute weights and points analytically when nlat=1,2
c
      IF (NLAT.EQ.1) THEN
C*PT*WARNING* Constant already double-precision
          THETA(1) = DACOS(0.0d0)
C*PT*WARNING* Constant already double-precision
          WTS(1) = 2.d0
          RETURN
      END IF
      IF (NLAT.EQ.2) THEN
C*PT*WARNING* Constant already double-precision
C*PT*WARNING* Constant already double-precision
          X = DSQRT(1.0d0/3.0d0)
          THETA(1) = DACOS(X)
          THETA(2) = DACOS(-X)
C*PT*WARNING* Constant already double-precision
          WTS(1) = 1.d0
C*PT*WARNING* Constant already double-precision
          WTS(2) = 1.d0
          RETURN
      END IF
c
      CALL DGAQD1(NLAT,THETA,WTS,DWORK,IERROR)
      IF (IERROR.NE.0) THEN
          IERROR = 3
          RETURN
      END IF
c
c     a newton correction
c
      CALL DGPNEWT(NLAT,THETA,DWORK)
c
c     compute weights
c
      CALL DGWTS(NLAT,THETA,WTS,DWORK)
      RETURN
      END
c
      SUBROUTINE DGAQD1(N,THETA,W,E,IER)
      DIMENSION THETA(N),W(N),E(N)
C*PT*WARNING* Already double-precision
      DOUBLE PRECISION THETA,W,E,TEMP
c
c     set symmetric tridiagnonal matrix subdiagonal and diagonal
c     coefficients for matrix coming from coefficients in the
c     recursion formula for legendre polynomials
c     a(n)*p(n-1)+b(n)*p(n)+c(n)*p(n+1) = 0.
c
      W(1) = 0.D0
      E(1) = 0.D0
      DO 100 J = 2,N
C*PT*WARNING* Already double-precision (DBLE)
C*PT*WARNING* Already double-precision (DBLE)
          E(J) = DBLE((J-1)**2)/DBLE((J+J-1)* (J+J-3))
          W(J) = 0.D0
  100 CONTINUE
c
c     compute zeros as the eigenvalues of a symmetric
c     tridiagonal matrix
c
      CALL DDVAL(N,W,E,IER)
      IF (IER.NE.0) WRITE (*,FMT=11) IER
   11 FORMAT (' error in evals =',i5)
c
c     compute gauss legendre points
c
      DO 101 J = 1,N
          THETA(J) = DACOS(W(J))
  101 CONTINUE
c
c     reverse order of gaussian points to be
c     monotonic increasing in radians
c
      N2 = N/2
      DO 102 I = 1,N2
          TEMP = THETA(I)
          THETA(I) = THETA(N-I+1)
          THETA(N-I+1) = TEMP
  102 CONTINUE
      RETURN
      END
      SUBROUTINE DGWTS(N,THETA,WTS,WORK)
      DOUBLE PRECISION WTS
C*PT*WARNING* Already double-precision
c
c     computes gauss weights as described in swarztrauber
c     and spotz, generalized harmonic transforms
c
      DOUBLE PRECISION THETA(N),WORK(N+1)
c
      CALL DGWTS1(N,THETA,WTS,WORK,WORK(N/2+2))
      RETURN
      END
      SUBROUTINE DGWTS1(N,THETA,WTS,DCP,CP)
C*PT*WARNING* Already double-precision
      DOUBLE PRECISION THETA(N),WTS(N),CP((N-1)/2+1),DCP(N/2+1),FN,SQNN,
     +                 PB,DPB

      FN = N
      SQNN = DSQRT((FN+FN-1)* (FN+FN+1))
      CALL DLFC(0,N-1,CP)
      CALL DLFC(0,N,DCP)
      DO I = 1,N
          CALL DLFT_DP(0,N-1,THETA(I),CP,PB)
          CALL DDLFT(0,N,THETA(I),DCP,DPB)
          WTS(I) = -SQNN*DSIN(THETA(I))/ (FN*PB*DPB)
      END DO
      RETURN
      END
      SUBROUTINE DGPNEWT(N,THETA,WORK)
C*PT*WARNING* Already double-precision
c
c     one newton iteration to correct the gauss points
c
      DOUBLE PRECISION THETA(N),WORK(N/2+1)
c
      CALL DGPNEWT1(N,THETA,WORK)
      RETURN
      END
      SUBROUTINE DGPNEWT1(N,THETA,CP)
C*PT*WARNING* Already double-precision
      DOUBLE PRECISION THETA(N),CP(N/2+1),PB,DPB
c
      CALL DLFC(0,N,CP)
      DO I = 1,N
          CALL DLFT_DP(0,N,THETA(I),CP,PB)
          CALL DDLFT(0,N,THETA(I),CP,DPB)
          THETA(I) = THETA(I) - PB/DPB
      END DO
      RETURN
      END
      SUBROUTINE DDVAL(N,D,E2,IERR)
c
c     name changed from tqlrat to avoid name conflicts
c
      INTEGER I,J,L,M,N,II,L1,MML,IERR
C*PT*WARNING* Already double-precision
      DOUBLE PRECISION D(N),E2(N)
C*PT*WARNING* Already double-precision
      DOUBLE PRECISION B,C,F,G,H,P,R,S,T,DDZEPS,DDPYTHA
c
c     this subroutine is a translation of the algol procedure tqlrat,
c     algorithm 464, comm. acm 16, 689(1973) by reinsch.
c
c     this subroutine finds the eigenvalues of a symmetric
c     tridiagonal matrix by the rational ql method.
c
c     on input
c
c        n is the order of the matrix.
c
c        d contains the diagonal elements of the input matrix.
c
c        e2 contains the squares of the subdiagonal elements of the
c          input matrix in its last n-1 positions.  e2(1) is arbitrary.
c
c      on output
c
c        d contains the eigenvalues in ascending order.  if an
c          error exit is made, the eigenvalues are correct and
c          ordered for indices 1,2,...ierr-1, but may not be
c          the smallest eigenvalues.
c
c        e2 has been destroyed.
c
c        ierr is set to
c          zero       for normal return,
c          j          if the j-th eigenvalue has not been
c                     determined after 30 iterations.
c
c     calls DDPYTHA for  sqrt(a*a + b*b) .
c
c     questions and comments should be directed to burton s. garbow,
c     mathematics and computer science div, argonne national laboratory
c
c     this version dated august 1987.
c     modified by c. moler to fix underflow/overflow difficulties,
c     especially on the vax and other machines where epslon(1.0e0)**2
c     nearly underflows.  see the loop involving statement 102 and
c     the two statements just before statement 200.
c
c     ------------------------------------------------------------------
c
      IERR = 0
      IF (N.EQ.1) GO TO 1001
c
      DO 100 I = 2,N
  100 E2(I-1) = E2(I)
C*PT*WARNING* Constant already double-precision
c
      F = 0.0d0
C*PT*WARNING* Constant already double-precision
      T = 0.0d0
C*PT*WARNING* Constant already double-precision
      E2(N) = 0.0d0
c
      DO 290 L = 1,N
          J = 0
          H = DABS(D(L)) + DSQRT(E2(L))
          IF (T.GT.H) GO TO 105
          T = H
          B = DDZEPS(T)
          C = B*B
C*PT*WARNING* Constant already double-precision
          IF (C.NE.0.0d0) GO TO 105
c        spliting tolerance underflowed.  look for larger value.
          DO 102 I = L,N
              H = DABS(D(I)) + DSQRT(E2(I))
              IF (H.GT.T) T = H
  102     CONTINUE
          B = DDZEPS(T)
          C = B*B
c     .......... look for small squared sub-diagonal element ..........
  105     DO 110 M = L,N
              IF (E2(M).LE.C) GO TO 120
c     .......... e2(n) is always zero, so there is no exit
c                through the bottom of the loop ..........
  110     CONTINUE
c
  120     IF (M.EQ.L) GO TO 210
  130     IF (J.EQ.30) GO TO 1000
          J = J + 1
c     .......... form shift ..........
          L1 = L + 1
          S = DSQRT(E2(L))
          G = D(L)
          P = (D(L1)-G)/ (2.0D0*S)
C*PT*WARNING* Constant already double-precision
          R = DDPYTHA(P,1.0d0)
          D(L) = S/ (P+SIGN(R,P))
          H = G - D(L)
c
          DO 140 I = L1,N
  140     D(I) = D(I) - H
c
          F = F + H
c     .......... rational ql transformation ..........
          G = D(M)
C*PT*WARNING* Constant already double-precision
          IF (G.EQ.0.0d0) G = B
          H = G
C*PT*WARNING* Constant already double-precision
          S = 0.0d0
          MML = M - L
c     .......... for i=m-1 step -1 until l do -- ..........
          DO 200 II = 1,MML
              I = M - II
              P = G*H
              R = P + E2(I)
              E2(I+1) = S*R
              S = E2(I)/R
              D(I+1) = H + S* (H+D(I))
              G = D(I) - E2(I)/G
c           avoid division by zero on next pass
              IF (G.EQ.0.0D0) G = DDZEPS(D(I))
              H = G* (P/R)
  200     CONTINUE
c
          E2(L) = S*G
          D(L) = H
C*PT*WARNING* Constant already double-precision
c     .......... guard against underflow in convergence test ..........
          IF (H.EQ.0.0d0) GO TO 210
          IF (DABS(E2(L)).LE.DABS(C/H)) GO TO 210
          E2(L) = H*E2(L)
C*PT*WARNING* Constant already double-precision
          IF (E2(L).NE.0.0d0) GO TO 130
  210     P = D(L) + F
c     .......... order eigenvalues ..........
          IF (L.EQ.1) GO TO 250
c     .......... for i=l step -1 until 2 do -- ..........
          DO 230 II = 2,L
              I = L + 2 - II
              IF (P.GE.D(I-1)) GO TO 270
              D(I) = D(I-1)
  230     CONTINUE
c
  250     I = 1
  270     D(I) = P
  290 CONTINUE
c
      GO TO 1001
c     .......... set error -- no convergence to an
c                eigenvalue after 30 iterations ..........
 1000 IERR = L
 1001 RETURN
      END
C*PT*WARNING* Already double-precision
      DOUBLE PRECISION FUNCTION DDPYTHA(A,B)
C*PT*WARNING* Already double-precision
      DOUBLE PRECISION A,B
C*PT*WARNING* Already double-precision
c     DDPYTHA is a double precision modification of pythag off eispack
c     for use by dimtql

c
c     finds sqrt(a**2+b**2) without overflow or destructive underflow
c
      DOUBLE PRECISION P,R,S,T,U

      P = DABS(A)
      IF (DABS(B).GE.DABS(A)) P = DABS(B)
C*PT*WARNING* Constant already double-precision
      IF (P.EQ.0.0d0) GO TO 20
      R = (DABS(A)/P)**2
      IF (DABS(B).LT.DABS(A)) R = (DABS(B)/P)**2
   10 CONTINUE
C*PT*WARNING* Constant already double-precision
      T = 4.0d0 + R
C*PT*WARNING* Constant already double-precision
      IF (T.EQ.4.0d0) GO TO 20
      S = R/T
C*PT*WARNING* Constant already double-precision
C*PT*WARNING* Constant already double-precision
      U = 1.0d0 + 2.0d0*S
      P = U*P
      R = (S/U)**2*R
      GO TO 10
   20 DDPYTHA = P
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
      SUBROUTINE DLFC(M,N,CP)
C*PT*WARNING* Already double-precision
c
      DOUBLE PRECISION CP,FNUM,FDEN,FNMH,A1,B1,C1,CP2,FNNP1,FNMSQ,FK,T1,
     +                 T2,PM1,SC10,SC20,SC40
      DIMENSION CP(1)
C*PT*WARNING* Constant already double-precision
      PARAMETER (SC10=1024.d0)
      PARAMETER (SC20=SC10*SC10)
      PARAMETER (SC40=SC20*SC20)
c
      CP(1) = 0.D0
      MA = IABS(M)
      IF (MA.GT.N) RETURN
      IF (N-1) 2,3,5
C*PT*WARNING* Constant already double-precision
    2 CP(1) = DSQRT(2.d0)
      RETURN
    3 IF (MA.NE.0) GO TO 4
C*PT*WARNING* Constant already double-precision
      CP(1) = DSQRT(1.5d0)
      RETURN
C*PT*WARNING* Constant already double-precision
    4 CP(1) = DSQRT(.75d0)
      IF (M.EQ.-1) CP(1) = -CP(1)
      RETURN
    5 IF (MOD(N+MA,2).NE.0) GO TO 10
      NMMS2 = (N-MA)/2
      FNUM = N + MA + 1
      FNMH = N - MA + 1
C*PT*WARNING* Constant already double-precision
      PM1 = 1.d0
      GO TO 15
   10 NMMS2 = (N-MA-1)/2
      FNUM = N + MA + 2
      FNMH = N - MA + 2
C*PT*WARNING* Constant already double-precision
      PM1 = -1.d0
C*PT*WARNING* Constant already double-precision
c      t1 = 1.
c      t1 = 2.d0**(n-1)
c      t1 = 1.d0/t1
   15 T1 = 1.d0/SC20
      NEX = 20
C*PT*WARNING* Constant already double-precision
      FDEN = 2.d0
      IF (NMMS2.LT.1) GO TO 20
      DO 18 I = 1,NMMS2
          T1 = FNUM*T1/FDEN
          IF (T1.GT.SC20) THEN
              T1 = T1/SC40
              NEX = NEX + 40
          END IF
          FNUM = FNUM + 2.D0
          FDEN = FDEN + 2.D0
   18 CONTINUE
C*PT*WARNING* Constant already double-precision
   20 T1 = T1/2.d0** (N-1-NEX)
      IF (MOD(MA/2,2).NE.0) T1 = -T1
      T2 = 1.D0
      IF (MA.EQ.0) GO TO 26
      DO 25 I = 1,MA
          T2 = FNMH*T2/ (FNMH+PM1)
          FNMH = FNMH + 2.D0
   25 CONTINUE
C*PT*WARNING* Constant already double-precision
   26 CP2 = T1*DSQRT((N+.5d0)*T2)
      FNNP1 = N* (N+1)
C*PT*WARNING* Constant already double-precision
      FNMSQ = FNNP1 - 2.d0*MA*MA
      L = (N+1)/2
      IF (MOD(N,2).EQ.0 .AND. MOD(MA,2).EQ.0) L = L + 1
      CP(L) = CP2
      IF (M.GE.0) GO TO 29
      IF (MOD(MA,2).NE.0) CP(L) = -CP(L)
   29 IF (L.LE.1) RETURN
      FK = N
      A1 = (FK-2.D0)* (FK-1.D0) - FNNP1
      B1 = 2.D0* (FK*FK-FNMSQ)
      CP(L-1) = B1*CP(L)/A1
   30 L = L - 1
      IF (L.LE.1) RETURN
      FK = FK - 2.D0
      A1 = (FK-2.D0)* (FK-1.D0) - FNNP1
      B1 = -2.D0* (FK*FK-FNMSQ)
      C1 = (FK+1.D0)* (FK+2.D0) - FNNP1
      CP(L-1) = - (B1*CP(L)+C1*CP(L+1))/A1
      GO TO 30
      END
      SUBROUTINE DLFT_DP(M,N,THETA,CP,PB)
C*PT*WARNING* Already double-precision
      DOUBLE PRECISION CP(*),PB,THETA,CDT,SDT,CTH,STH,CHH

      CDT = DCOS(THETA+THETA)
      SDT = DSIN(THETA+THETA)
      NMOD = MOD(N,2)
      MMOD = MOD(M,2)
      IF (NMOD) 1,1,2
    1 IF (MMOD) 3,3,4
c
c     n even, m even
c
    3 KDO = N/2
      PB = .5D0*CP(1)
      IF (N.EQ.0) RETURN
      CTH = CDT
      STH = SDT
      DO 170 K = 1,KDO
c     pb = pb+cp(k+1)*dcos(2*k*theta)
          PB = PB + CP(K+1)*CTH
          CHH = CDT*CTH - SDT*STH
          STH = SDT*CTH + CDT*STH
          CTH = CHH
  170 CONTINUE
      RETURN
c
c     n even, m odd
c
    4 KDO = N/2
      PB = 0.D0
      CTH = CDT
      STH = SDT
      DO 180 K = 1,KDO
c     pb = pb+cp(k)*dsin(2*k*theta)
          PB = PB + CP(K)*STH
          CHH = CDT*CTH - SDT*STH
          STH = SDT*CTH + CDT*STH
          CTH = CHH
  180 CONTINUE
      RETURN
    2 IF (MMOD) 13,13,14
c
c     n odd, m even
c
   13 KDO = (N+1)/2
      PB = 0.D0
      CTH = DCOS(THETA)
      STH = DSIN(THETA)
      DO 190 K = 1,KDO
c     pb = pb+cp(k)*dcos((2*k-1)*theta)
          PB = PB + CP(K)*CTH
          CHH = CDT*CTH - SDT*STH
          STH = SDT*CTH + CDT*STH
          CTH = CHH
  190 CONTINUE
      RETURN
c
c     n odd, m odd
c
   14 KDO = (N+1)/2
      PB = 0.D0
      CTH = DCOS(THETA)
      STH = DSIN(THETA)
      DO 200 K = 1,KDO
c     pb = pb+cp(k)*dsin((2*k-1)*theta)
          PB = PB + CP(K)*STH
          CHH = CDT*CTH - SDT*STH
          STH = SDT*CTH + CDT*STH
          CTH = CHH
  200 CONTINUE
      RETURN
      END
      SUBROUTINE DDLFT(M,N,THETA,CP,PB)
c
c     computes the derivative of pmn(theta) with respect to theta
c
      DIMENSION CP(1)
C*PT*WARNING* Already double-precision
      DOUBLE PRECISION CP,PB,THETA,CDT,SDT,CTH,STH,CHH

      CDT = DCOS(THETA+THETA)
      SDT = DSIN(THETA+THETA)
      NMOD = MOD(N,2)
      MMOD = MOD(ABS(M),2)
      IF (NMOD) 1,1,2
    1 IF (MMOD) 3,3,4
c
c     n even, m even
c
    3 KDO = N/2
C*PT*WARNING* Constant already double-precision
      PB = 0.d0
      IF (N.EQ.0) RETURN
      CTH = CDT
      STH = SDT
      DO 170 K = 1,KDO
C*PT*WARNING* Constant already double-precision
c     pb = pb+cp(k+1)*dcos(2*k*theta)
          PB = PB - 2.d0*K*CP(K+1)*STH
          CHH = CDT*CTH - SDT*STH
          STH = SDT*CTH + CDT*STH
          CTH = CHH
  170 CONTINUE
      RETURN
c
c     n even, m odd
c
    4 KDO = N/2
      PB = 0.D0
      CTH = CDT
      STH = SDT
      DO 180 K = 1,KDO
C*PT*WARNING* Constant already double-precision
c     pb = pb+cp(k)*dsin(2*k*theta)
          PB = PB + 2.d0*K*CP(K)*CTH
          CHH = CDT*CTH - SDT*STH
          STH = SDT*CTH + CDT*STH
          CTH = CHH
  180 CONTINUE
      RETURN
    2 IF (MMOD) 13,13,14
c
c     n odd, m even
c
   13 KDO = (N+1)/2
      PB = 0.D0
      CTH = DCOS(THETA)
      STH = DSIN(THETA)
      DO 190 K = 1,KDO
C*PT*WARNING* Constant already double-precision
c     pb = pb+cp(k)*dcos((2*k-1)*theta)
          PB = PB - (2.d0*K-1)*CP(K)*STH
          CHH = CDT*CTH - SDT*STH
          STH = SDT*CTH + CDT*STH
          CTH = CHH
  190 CONTINUE
      RETURN
c
c     n odd, m odd
c
   14 KDO = (N+1)/2
      PB = 0.D0
      CTH = DCOS(THETA)
      STH = DSIN(THETA)
      DO 200 K = 1,KDO
C*PT*WARNING* Constant already double-precision
c     pb = pb+cp(k)*dsin((2*k-1)*theta)
          PB = PB + (2.d0*K-1)*CP(K)*CTH
          CHH = CDT*CTH - SDT*STH
          STH = SDT*CTH + CDT*STH
          CTH = CHH
  200 CONTINUE
      RETURN
      END
