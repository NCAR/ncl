C
C	$Id: cssig1.f,v 1.5 2008-07-27 03:10:08 haley Exp $
C                                                                      
C                Copyright (C)  2000
C        University Corporation for Atmospheric Research
C                All Rights Reserved
C
C The use of this Software is governed by a License Agreement.
C
      DOUBLE PRECISION FUNCTION CSSIG1 (N1,N2,N,X,Y,Z,H,LIST,
     .                                LPTR,LEND,GRAD,IFLGB,
     .                                HPBND,TOL,
     .                                IFLGS, SIGMA, IER)
      INTEGER N1, N2, N, LIST(*), LPTR(*), LEND(N), IFLGB,
     .        IFLGS, IER
      DOUBLE PRECISION X(N), Y(N), Z(N), H(N), GRAD(3,N),
     .                 HPBND, TOL, SIGMA(*)
C
C***********************************************************
C
C                                              From SSRFPACK
C                                            Robert J. Renka
C                                  Dept. of Computer Science
C                                       Univ. of North Texas
C                                           renka@cs.unt.edu
C                                                   11/21/96
C
C   Given a triangulation of a set of nodes on the unit
C sphere, along with data values H and gradients GRAD at the
C nodes, this function determines the smallest tension fac-
C tor CSSIG1 such that the first derivative HP(A) of the
C Hermite interpolatory tension spline H(A), defined by CSSIG1
C and the endpoint values and directional derivatives asso-
C ciated with an arc N1-N2, is bounded (either above or
C below) by HPBND for all A in (A1,A2), where (A1,A2) de-
C notes an interval corresponding to the arc and A denotes
C arc-length.
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
C       IFLGB = Bound option indicator:
C               IFLGB = -1 if HPBND is a lower bound on HP.
C               IFLGB = 1 if HPBND is an upper bound on HP.
C
C       HPBND = Bound on HP.  HPBND .LE. min(HP1,HP2,S) if
C               IFLGB = -1 and HPBND .GE. max(HP1,HP2,S) if
C               IFLGB = 1, where HP1 and HP2 are the direc-
C               tional derivatives at the endpoints of the
C               arc N1-N2, and S is the slope of the linear
C               interpolant of the endpoint data values.
C
C       TOL = Tolerance whose magnitude determines how close
C             CSSIG1 is to its optimal value when nonzero
C             finite tension is necessary and sufficient to
C             satisfy the constraint.  For a lower bound,
C             CSSIG1 is chosen so that HPBND .LE. HPMIN .LE.
C             HPBND + abs(TOL), where HPMIN is the minimum
C             value of HP on the arc.  For an upper bound,
C             the maximum of HP satisfies HPBND - abs(TOL)
C             .LE. HPMAX .LE. HPBND.  Thus, the constraint
C             is satisfied but possibly with more tension
C             than necessary.
C
C       IFLGS = Tension array option indicator:
C               IFLGS .LE. 0 if SIGMA is not to be used.
C               IFLGS .GE. 1 if SIGMA is to be updated by
C                            storing CSSIG1 in the appropriate
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
C             IER = 0 if no errors were encountered and the
C                     constraint can be satisfied with fin-
C                     ite tension.
C             IER = 1 if no errors were encountered but in-
C                     finite tension is required to satisfy
C                     the constraint (e.g., IFLGB = -1,
C                     HPBND = S, and HP1 > S).
C             IER = -1 if N1, N2, N, or IFLGB is outside its
C                      valid range.
C             IER = -2 if nodes N1 and N2 coincide or IFLGS
C                      .GE. 1 and the nodes are not adja-
C                      cent.
C             IER = -3 if HPBND is outside its valid range.
C
C       CSSIG1 = Minimum tension factor defined above unless
C              IER < 0, in which case CSSIG1 = -1.  If IER
C              = 1, CSSIG1 is set to 85, resulting in an
C              approximation to the linear interpolant of
C              the endpoint values.
C
C STRIPACK module required by CSSIG1:  CSSTORE
C
C SSRFPACK modules required by CSSIG1:  CSARCLEN, CSSNHCSH
C
C Intrinsic functions called by CSSIG1:   ABS, EXP, MAX, MIN,
C                                         DBLE, SIGN, SQRT
C
C***********************************************************
C
      DOUBLE PRECISION CSARCLEN, CSSTORE
      INTEGER LP1, LP2, LPL, LUN, NIT
      DOUBLE PRECISION A, A0, AL, B0, BND, C0, C1, C2,
     .                 COSHM, COSHMM, D0, D1, D1PD2, D2,
     .                 DMAX, DSIG, E, EMS, EMS2, F, F0,
     .                 FMAX, FNEG, FTOL, P1(3), P2(3), RF,
     .                 RTOL, S, S1, S2, SBIG, SIG, SINH,
     .                 SINHM, STOL, T0, T1, T2, TM, UN(3),
     .                 UNORM
C
      DATA SBIG/85./,  LUN/-1/
      RF = DBLE(IFLGB)
      BND = HPBND
C
C Print a heading.
C
      IF (LUN .GE. 0  .AND.  RF .LT. 0.) WRITE (LUN,100) N1,
     .                                   N2, BND
      IF (LUN .GE. 0  .AND.  RF .GT. 0.) WRITE (LUN,110) N1,
     .                                   N2, BND
  100 FORMAT (//1X,'CSSIG1 -- N1 =',I4,', N2 =',I4,
     .        ', LOWER BOUND = ',E15.8)
  110 FORMAT (//1X,'CSSIG1 -- N1 =',I4,', N2 =',I4,
     .        ', UPPER BOUND = ',E15.8)
C
C Test for errors and store local parameters.
C
      IER = -1
      IF (MIN(N1,N2) .LT. 1  .OR.  N1 .EQ. N2  .OR.
     .    MAX(N1,N2,3) .GT. N  .OR.  ABS(RF) .NE. 1.)
     .   GO TO 11
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
C Compute first difference S and scaled directional deriva-
C   tives S1,S2 at the endpoints (for the direction N1->N2).
C
      S = H(N2) - H(N1)
      S1 = AL*(GRAD(1,N1)*P2(1) + GRAD(2,N1)*P2(2) +
     .         GRAD(3,N1)*P2(3))/UNORM
      S2 = -AL*(GRAD(1,N2)*P1(1) + GRAD(2,N2)*P1(2) +
     .          GRAD(3,N2)*P1(3))/UNORM
C
C Test for a valid constraint.
C
      IER = -3
      IF ((RF .LT. 0.  .AND.  MIN(S1,S2,S) .LT. BND)  .OR.
     .    (RF .GT. 0.  .AND.  BND .LT. MAX(S1,S2,S)))
     .   GO TO 11
C
C Test for infinite tension required.
C
      IER = 1
      SIG = SBIG
      IF (S .EQ. BND  .AND.  (S1 .NE. S  .OR.  S2 .NE. S))
     .   GO TO 10
C
C Test for SIG = 0 sufficient.  The Hermite cubic interpo-
C   lant H0 has derivative HP0(T) = (S2 + 2*B0*R + A0*R**2)/
C   AL, where R = (T2-T)/AL.
C
      IER = 0
      SIG = 0.
      T0 = 3.*S - S1 - S2
      B0 = T0 - S2
      C0 = T0 - S1
      A0 = -B0 - C0
C
C   HP0(R) has an extremum (at R = -B0/A0) in (0,1) iff
C     B0*C0 > 0 and the third derivative of H0 has the
C     sign of A0.
C
      IF (B0*C0 .LE. 0.  .OR.  A0*RF .GT. 0.) GO TO 10
C
C   A0*RF < 0 and HP0(R) = -D0/(DT*A0) at R = -B0/A0.
C
      D0 = T0*T0 - S1*S2
      F0 = (BND + D0/(A0*AL))*RF
      IF (F0 .GE. 0.) GO TO 10
C
C Find a zero of F(SIG) = (BND-HP(R))*RF, where HP has an
C   extremum at R.  F has a unique zero, F(0) = F0 < 0, and
C   F = (BND-S)*RF > 0 for SIG sufficiently large.
C
C Initialize parameters for the secant method.  The method
C   uses three points:  (SG0,F0), (SIG,F), and (SNEG,FNEG),
C   where SG0 and SNEG are defined implicitly by DSIG = SIG
C   - SG0 and DMAX = SIG - SNEG.  SG0 is initially zero and
C   SIG is initialized to the zero of (BND - (SIG*S-S1-S2)/
C   (AL*(SIG-2.)))*RF -- a value for which F(SIG) .GE. 0 and
C   F(SIG) = 0 for SIG sufficiently large that 2*SIG is in-
C   significant relative to exp(SIG).
C
      FMAX = (BND-S/AL)*RF
      SIG = 2. - A0/(3.*(AL*BND-S))
      IF (LUN .GE. 0) WRITE (LUN,120) F0, FMAX, SIG
  120 FORMAT (1X,9X,'F0 = ',E15.8,', FMAX = ',E15.8/
     .        1X,8X,'SIG = ',E15.8/)
      IF (CSSTORE(SIG*EXP(-SIG)+.5) .EQ. .5) GO TO 10
      DSIG = SIG
      DMAX = -2.*SIG
      FNEG = FMAX
      D1 = S - S1
      D2 = S2 - S
      D1PD2 = D1 + D2
      NIT = 0
C
C Compute an absolute tolerance FTOL = abs(TOL), and a
C   relative tolerance RTOL = 100*Macheps.
C
      FTOL = ABS(TOL)
      RTOL = 1.
    5 RTOL = RTOL/2.
        IF (CSSTORE(RTOL+1.) .GT. 1.) GO TO 5
      RTOL = RTOL*200.
C
C Top of loop:  compute F.
C
    6 IF (SIG .LE. .5) THEN
C
C   Use approximations designed to avoid cancellation
C     error (associated with small SIG) in the modified
C     hyperbolic functions.
C
        CALL CSSNHCSH (SIG, SINHM,COSHM,COSHMM)
        C1 = SIG*COSHM*D2 - SINHM*D1PD2
        C2 = SIG*(SINHM+SIG)*D2 - COSHM*D1PD2
        A = C2 - C1
        E = SIG*SINHM - COSHMM - COSHMM
      ELSE
C
C   Scale SINHM and COSHM by 2*exp(-SIG) in order to avoid
C     overflow.
C
        EMS = EXP(-SIG)
        EMS2 = EMS + EMS
        TM = 1. - EMS
        SINH = TM*(1.+EMS)
        SINHM = SINH - SIG*EMS2
        COSHM = TM*TM
        C1 = SIG*COSHM*D2 - SINHM*D1PD2
        C2 = SIG*SINH*D2 - COSHM*D1PD2
        A = EMS2*(SIG*TM*D2 + (TM-SIG)*D1PD2)
        E = SIG*SINH - COSHM - COSHM
      ENDIF
C
C   The second derivative HPP of H(R) has a zero at exp(SIG*
C     R) = SQRT((C2+C1)/A) and R is in (0,1) and well-
C     defined iff HPP(T1)*HPP(T2) < 0.
C
      F = FMAX
      T1 = A*(C2+C1)
      IF (T1 .GE. 0.) THEN
        IF (C1*(SIG*COSHM*D1 - SINHM*D1PD2) .LT. 0.) THEN
C
C   HP(R) = (B+SIGN(A)*SQRT(A*C))/(AL*E) at the critical
C     value of R, where A = C2-C1, B = E*S2-C2, and C = C2 +
C     C1.  Note that RF*A < 0.
C
          F = (BND - (E*S2-C2 - RF*SQRT(T1))/(AL*E))*RF
        ENDIF
      ENDIF
C
C   Update the number of iterations NIT.
C
      NIT = NIT + 1
      IF (LUN .GE. 0) WRITE (LUN,130) NIT, SIG, F
  130 FORMAT (1X,3X,I2,' -- SIG = ',E15.8,', F = ',
     .        E15.8)
      IF (F0*F .LT. 0.) THEN
C
C   F0*F < 0.  Update (SNEG,FNEG) to (SG0,F0) so that F
C     and FNEG always have opposite signs.  If SIG is closer
C     to SNEG than SG0 and abs(F) < abs(FNEG), then swap
C     (SNEG,FNEG) with (SG0,F0).
C
        T1 = DMAX
        T2 = FNEG
        DMAX = DSIG
        FNEG = F0
        IF ( ABS(DSIG) .GT. ABS(T1)  .AND.
     .       ABS(F) .LT. ABS(T2) ) THEN
C
          DSIG = T1
          F0 = T2
        ENDIF
      ENDIF
C
C   Test for convergence.
C
      STOL = RTOL*SIG
      IF (ABS(DMAX) .LE. STOL  .OR.  (F .GE. 0.  .AND.
     .    F .LE. FTOL)  .OR.  ABS(F) .LE. RTOL) GO TO 10
      IF (F0*F .LT. 0.  .OR.  ABS(F) .LT. ABS(F0)) GO TO 8
C
C   F*F0 > 0 and the new estimate would be outside of the
C     bracketing interval of length abs(DMAX).  Reset
C     (SG0,F0) to (SNEG,FNEG).
C
    7 DSIG = DMAX
      F0 = FNEG
C
C   Compute the change in SIG by linear interpolation
C     between (SG0,F0) and (SIG,F).
C
    8 DSIG = -F*DSIG/(F-F0)
      IF (LUN .GE. 0) WRITE (LUN,140) DSIG
  140 FORMAT (1X,8X,'DSIG = ',E15.8)
      IF ( ABS(DSIG) .GT. ABS(DMAX)  .OR.
     .     DSIG*DMAX .GT. 0. ) GO TO 7
C
C   Restrict the step-size such that abs(DSIG) .GE. STOL/2.
C     Note that DSIG and DMAX have opposite signs.
C
      IF (ABS(DSIG) .LT. STOL/2.) DSIG = -SIGN(STOL/2.,DMAX)
C
C   Bottom of loop:  update SIG, DMAX, and F0.
C
      SIG = SIG + DSIG
      DMAX = DMAX + DSIG
      F0 = F
      GO TO 6
C
C No errors encountered.
C
   10 CSSIG1 = SIG
      IF (IFLGS .LE. 0) RETURN
      SIGMA(LP1) = SIG
      SIGMA(LP2) = SIG
      RETURN
C
C Error termination.
C
   11 CSSIG1 = -1.
      RETURN
      END
