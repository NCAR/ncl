C
C	$Id: cssig2.f,v 1.5 2008-07-27 03:10:08 haley Exp $
C                                                                      
C                Copyright (C)  2000
C        University Corporation for Atmospheric Research
C                All Rights Reserved
C
C The use of this Software is governed by a License Agreement.
C
      DOUBLE PRECISION FUNCTION CSSIG2 (N1,N2,N,X,Y,Z,H,LIST,
     .                                LPTR,LEND,GRAD,TOL,
     .                                IFLGS, SIGMA, IER)
      INTEGER N1, N2, N, LIST(*), LPTR(*), LEND(N), IFLGS,
     .        IER
      DOUBLE PRECISION X(N), Y(N), Z(N), H(N), GRAD(3,N),
     .                 TOL, SIGMA(*)
C
C***********************************************************
C
C                                              From SSRFPACK
C                                            Robert J. Renka
C                                  Dept. of Computer Science
C                                       Univ. of North Texas
C                                           renka@cs.unt.edu
C                                                   07/25/96
C
C   Given a triangulation of a set of nodes on the unit
C sphere, along with data values H and gradients GRAD at the
C nodes, this function determines the smallest tension fac-
C tor CSSIG2 such that the Hermite interpolatory tension
C spline H(A), defined by CSSIG2 and the endpoint values and
C directional derivatives associated with an arc N1-N2,
C preserves convexity (or concavity) of the data:
C
C   HP1 .LE. S .LE. HP2 implies HPP(A) .GE. 0, and
C   HP1 .GE. S .GE. HP2 implies HPP(A) .LE. 0
C
C for all A in the open interval (A1,A2) corresponding to
C the arc, where HP1 and HP2 are the derivative values of H
C at the endpoints, S is the slope of the linear interpolant
C of the endpoint data values, HPP denotes the second deriv-
C ative of H, and A is arc-length.  Note, however, that
C infinite tension is required if HP1 = S or HP2 = S (unless
C HP1 = HP2 = S).
C
C On input:
C
C       N1,N2 = Nodal indexes of the endpoints of an arc for
C               which the tension factor is to be computed.
C               The indexes must be distinct and lie in the
C               range 1 to N, and if IFLGS .GE. 1, they must
C               correspond to adjacent nodes in the triangu-
C               lation.
C
C       N = Number of nodes in the triangulation.  N .GE. 3.
C
C       X,Y,Z = Arrays of length N containing coordinates of
C               the nodes.  X(I)**2 + Y(I)**2 + Z(I)**2 = 1.
C
C       H = Array of length N containing data values at the
C           nodes.  H(I) is associated with (X(I),Y(I),Z(I))
C           for I = 1 to N.
C
C       LIST,LPTR,LEND = Data structure defining the trian-
C                        gulation.  Refer to STRIPACK
C                        Subroutine CSTRMESH.
C
C       GRAD = Array dimensioned 3 by N whose columns con-
C              gradients at the nodes.  GRAD( ,J) must be
C              orthogonal to node J:  GRAD(1,J)*X(J) +
C              GRAD(2,J)*Y(J) + GRAD(3,J)*Z(J) = 0.  Refer
C              to Subroutines CSGRADG, CSGRADL, and CSSMSURF.
C
C       TOL = Tolerance whose magnitude determines how close
C             CSSIG2 is to its optimal value when nonzero
C             finite tension is necessary and sufficient to
C             satisfy convexity or concavity.  In the case
C             convexity, CSSIG2 is chosen so that 0 .LE.
C             HPPMIN .LE. abs(TOL), where HPPMIN is the
C             minimum value of HPP on the arc.  In the case
C             of concavity, the maximum value of HPP satis-
C             fies -abs(TOL) .LE. HPPMAX .LE. 0.  Thus, the
C             constraint is satisfied but possibly with more
C             tension than necessary.
C
C       IFLGS = Tension array option indicator:
C               IFLGS .LE. 0 if SIGMA is not to be used.
C               IFLGS .GE. 1 if SIGMA is to be updated by
C                            storing CSSIG2 in the appropriate
C                            locations.
C
C The above parameters are not altered by this function.
C
C       SIGMA = Dummy parameter (IFLGS .LE. 0) or array
C               containing tension factors associated with
C               arcs in one-to-one correspondence with LIST
C               entries (IFLGS .GE. 1).  Refer to Subroutine
C               CSGETSIG.
C
C On output:
C
C       SIGMA = Tension factor array updated with the new
C               value if and only if IFLGS .GE. 1 and IER
C               .GE. 0.
C
C       IER = Error indicator:
C             IER = 0 if no errors were encountered and fin-
C                     ite tension is sufficient to satisfy
C                     convexity (or concavity).
C             IER = 1 if no errors were encountered but in-
C                     finite tension is required to satisfy
C                     convexity.
C             IER = 2 if the data does not satisfy convexity
C                     or concavity.
C             IER = -1 if N1, N2, or N is outside its valid
C                      range.
C             IER = -2 if nodes N1 and N2 coincide or IFLGS
C                      .GE. 1 and the nodes are not adja-
C                      cent.
C
C       CSSIG2 = Minimum tension factor defined above unless
C              IER < 0, in which case CSSIG2 = -1.  If IER
C              = 1, CSSIG2 is set to 85, resulting in an
C              approximation to the linear interpolant of
C              the endpoint values.  If IER = 2, CSSIG2 = 0,
C              resulting in the Hermite cubic interpolant.
C
C STRIPACK module required by CSSIG2:  CSSTORE
C
C SSRFPACK modules required by CSSIG2:  CSARCLEN, CSSNHCSH
C
C Intrinsic functions called by CSSIG2:  ABS, EXP, MAX, MIN,
C                                        SQRT
C
C***********************************************************
C
      DOUBLE PRECISION CSARCLEN, CSSTORE
      INTEGER LP1, LP2, LPL, LUN, NIT
      DOUBLE PRECISION AL, COSHM, D1, D1D2, D2, DSIG, DUMMY,
     .                 EMS, F, FP, FTOL, P1(3), P2(3), RTOL,
     .                 S, SBIG, SIG, SINHM, SSM, T, T1, TP1,
     .                 UN(3), UNORM
C
      DATA SBIG/85./,  LUN/-1/
C
C Print a heading.
C
      IF (LUN .GE. 0) WRITE (LUN,100) N1, N2
  100 FORMAT (//1X,'CSSIG2 -- N1 =',I4,', N2 =',I4)
C
C Test for errors and set local parameters.
C
      IER = -1
      IF (MIN(N1,N2) .LT. 1  .OR.  N1 .EQ. N2  .OR.
     .    MAX(N1,N2,3) .GT. N) GO TO 11
      IER = -2
      IF (IFLGS .GT. 0) THEN
C
C   Set LP1 and LP2 to the pointers to N2 as a neighbor of
C     N1 and N1 as a neighbor of N2, respectively.
C
        LPL = LEND(N1)
        LP1 = LPTR(LPL)
    1   IF (LIST(LP1) .EQ. N2) GO TO 2
          LP1 = LPTR(LP1)
          IF (LP1 .NE. LPL) GO TO 1
        IF (ABS(LIST(LP1)) .NE. N2) GO TO 11
C
    2   LPL = LEND(N2)
        LP2 = LPTR(LPL)
    3   IF (LIST(LP2) .EQ. N1) GO TO 4
          LP2 = LPTR(LP2)
          IF (LP2 .NE. LPL) GO TO 3
        IF (ABS(LIST(LP2)) .NE. N1) GO TO 11
      ENDIF
C
C Store nodal coordinates P1 and P2, compute arc-length AL
C   and unit normal UN = (P1 X P2)/UNORM, and test for
C   coincident nodes.
C
    4 P1(1) = X(N1)
      P1(2) = Y(N1)
      P1(3) = Z(N1)
      P2(1) = X(N2)
      P2(2) = Y(N2)
      P2(3) = Z(N2)
      AL = CSARCLEN(P1,P2)
      UN(1) = P1(2)*P2(3) - P1(3)*P2(2)
      UN(2) = P1(3)*P2(1) - P1(1)*P2(3)
      UN(3) = P1(1)*P2(2) - P1(2)*P2(1)
      UNORM = SQRT(UN(1)*UN(1) + UN(2)*UN(2) + UN(3)*UN(3))
      IF (UNORM .EQ. 0.  .OR.  AL .EQ. 0.) GO TO 11
C
C Compute first and second differences and test for infinite
C   tension required.
C
      S = H(N2) - H(N1)
      D1 = S - AL*(GRAD(1,N1)*P2(1) + GRAD(2,N1)*P2(2) +
     .             GRAD(3,N1)*P2(3))/UNORM
      D2 = -AL*(GRAD(1,N2)*P1(1) + GRAD(2,N2)*P1(2) +
     .          GRAD(3,N2)*P1(3))/UNORM - S
      D1D2 = D1*D2
      IER = 1
      SIG = SBIG
      IF (D1D2 .EQ. 0.  .AND.  D1 .NE. D2) GO TO 10
C
C Test for a valid constraint.
C
      IER = 2
      SIG = 0.
      IF (D1D2 .LT. 0.) GO TO 10
C
C Test for SIG = 0 sufficient.
C
      IER = 0
      IF (D1D2 .EQ. 0.) GO TO 10
      T = MAX(D1/D2,D2/D1)
      IF (T .LE. 2.) GO TO 10
C
C Find a zero of F(SIG) = SIG*COSHM(SIG)/SINHM(SIG) - (T+1).
C   Since the derivative of F vanishes at the origin, a
C   quadratic approximation is used to obtain an initial
C   estimate for the Newton method.
C
      TP1 = T + 1.
      SIG = SQRT(10.*T-20.)
      NIT = 0
C
C   Compute an absolute tolerance FTOL = abs(TOL) and a
C     relative tolerance RTOL = 100*Macheps.
C
      FTOL = ABS(TOL)
      RTOL = 1.
    5 RTOL = RTOL/2.
        IF (CSSTORE(RTOL+1.) .GT. 1.) GO TO 5
      RTOL = RTOL*200.
C
C Top of loop:  evaluate F and its derivative FP.
C
    6 IF (SIG .LE. .5) THEN
C
C   Use approximations designed to avoid cancellation error
C     in the hyperbolic functions.
C
        CALL CSSNHCSH (SIG, SINHM,COSHM,DUMMY)
        T1 = COSHM/SINHM
        FP = T1 + SIG*(SIG/SINHM - T1*T1 + 1.)
      ELSE
C
C   Scale SINHM and COSHM by 2*exp(-SIG) in order to avoid
C     overflow.
C
        EMS = EXP(-SIG)
        SSM = 1. - EMS*(EMS+SIG+SIG)
        T1 = (1.-EMS)*(1.-EMS)/SSM
        FP = T1 + SIG*(2.*SIG*EMS/SSM - T1*T1 + 1.)
      ENDIF
C
      F = SIG*T1 - TP1
C
C   Update the number of iterations NIT.
C
      NIT = NIT + 1
      IF (LUN .GE. 0) WRITE (LUN,110) NIT, SIG, F, FP
  110 FORMAT (1X,3X,I2,' -- SIG = ',E15.8,', F = ',
     .        E15.8/1X,31X,'FP = ',E15.8)
C
C   Test for convergence.
C
      IF (FP .LE. 0.) GO TO 10
      DSIG = -F/FP
      IF (ABS(DSIG) .LE. RTOL*SIG  .OR.  (F .GE. 0.  .AND.
     .    F .LE. FTOL)  .OR.  ABS(F) .LE. RTOL) GO TO 10
C
C   Bottom of loop:  update SIG.
C
      SIG = SIG + DSIG
      GO TO 6
C
C No errors encountered.
C
   10 CSSIG2 = SIG
      IF (IFLGS .LE. 0) RETURN
      SIGMA(LP1) = SIG
      SIGMA(LP2) = SIG
      RETURN
C
C Error termination.
C
   11 CSSIG2 = -1.
      RETURN
      END
