C
C	$Id: cssig0.f,v 1.5 2008-07-27 03:10:08 haley Exp $
C                                                                      
C                Copyright (C)  2000
C        University Corporation for Atmospheric Research
C                All Rights Reserved
C
C The use of this Software is governed by a License Agreement.
C
      DOUBLE PRECISION FUNCTION CSSIG0 (N1,N2,N,X,Y,Z,H,LIST,
     .                                LPTR,LEND,GRAD,IFLGB,
     .                                HBND,TOL,IFLGS, SIGMA,
     .                                IER)
      INTEGER N1, N2, N, LIST(*), LPTR(*), LEND(N), IFLGB,
     .        IFLGS, IER
      DOUBLE PRECISION X(N), Y(N), Z(N), H(N), GRAD(3,N),
     .                 HBND, TOL, SIGMA(*)
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
C tor CSSIG0 such that the Hermite interpolatory tension
C spline H(A), defined by CSSIG0 and the endpoint values and
C directional derivatives associated with an arc N1-N2, is
C bounded (either above or below) by HBND for all A in
C (A1,A2), where (A1,A2) denotes an interval corresponding
C to the arc and A is the arc-length.
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
C              tain gradients at the nodes.  GRAD( ,J) must
C              be orthogonal to node J:  GRAD(1,J)*X(J) +
C              GRAD(2,J)*Y(J) + GRAD(3,J)*Z(J) = 0.  Refer
C              to Subroutines CSGRADG, CSGRADL, and CSSMSURF.
C
C       IFLGB = Bound option indicator:
C               IFLGB = -1 if HBND is a lower bound on H.
C               IFLGB = 1 if HBND is an upper bound on H.
C
C       HBND = Bound on H.  HBND .LE. min(H1,H2) if IFLGB =
C              -1 and HBND .GE. max(H1,H2) if IFLGB = 1,
C              where H1 and H2 are the data values at the
C              endpoints of the arc N1-N2.
C
C       TOL = Tolerance whose magnitude determines how close
C             CSSIG0 is to its optimal value when nonzero
C             finite tension is necessary and sufficient to
C             satisfy the constraint.  For a lower bound,
C             CSSIG0 is chosen so that HBND .LE. HMIN .LE.
C             HBND + abs(TOL), where HMIN is the minimum
C             value of H on the arc, and for an upper bound,
C             the maximum of H satisfies HBND - abs(TOL)
C             .LE. HMAX .LE. HBND.  Thus, the constraint is
C             satisfied but possibly with more tension than
C             necessary.
C
C       IFLGS = Tension array option indicator:
C               IFLGS .LE. 0 if SIGMA is not to be used.
C               IFLGS .GE. 1 if SIGMA is to be updated by
C                            storing CSSIG0 in the appropriate
C                            locations.
C
C The above parameters are not altered by this function.
C
C       SIGMA = Dummy parameter (IFLGS .LE. 0) or array con-
C               taining tension factors associated with arcs
C               in one-to-one correspondence with LIST
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
C                     the constraint (e.g., IFLGB = -1, HBND
C                     = H(A1), and the directional deriva-
C                     tive of H at A1 is negative).
C             IER = -1 if N1, N2, N, or IFLGB is outside its
C                      valid range.
C             IER = -2 if nodes N1 and N2 coincide or IFLGS
C                      .GE. 1 and the nodes are not adja-
C                      cent.
C             IER = -3 if HBND is outside its valid range.
C
C       CSSIG0 = Minimum tension factor defined above unless
C              IER < 0, in which case CSSIG0 = -1.  If IER
C              = 1, CSSIG0 is set to 85, resulting in an
C              approximation to the linear interpolant of
C              the endpoint values.
C
C STRIPACK module required by CSSIG0:  CSSTORE
C
C SSRFPACK modules required by CSSIG0:  CSARCLEN, CSSNHCSH
C
C Intrinsic functions called by CSSIG0:  ABS, EXP, LOG, MAX,
C                                        MIN, DBLE, SIGN,
C                                        SQRT
C
C***********************************************************
C
      DOUBLE PRECISION CSARCLEN, CSSTORE
      INTEGER LP1, LP2, LPL, LUN, NIT
      DOUBLE PRECISION A, A0, AA, AL, B, B0, BND, C, C1, C2,
     .                 COSHM, COSHMM, D, D0, D1PD2, D2,
     .                 DMAX, DSIG, E, EMS, F, F0, FMAX,
     .                 FNEG, FTOL, H1, H2, P1(3), P2(3), R,
     .                 RF, RSIG, RTOL, S, S1, S2, SBIG, SCM,
     .                 SIG, SINHM, SNEG, SSINH, SSM, STOL,
     .                 T, T0, T1, T2, TM, UN(3), UNORM
C
      DATA SBIG/85./,  LUN/-1/
      RF = DBLE(IFLGB)
      BND = HBND
C
C Print a heading.
C
      IF (LUN .GE. 0  .AND.  RF .LT. 0.) WRITE (LUN,100) N1,
     .                                   N2, BND
      IF (LUN .GE. 0  .AND.  RF .GT. 0.) WRITE (LUN,110) N1,
     .                                   N2, BND
  100 FORMAT (//1X,'CSSIG0 -- N1 =',I4,', N2 =',I4,
     .        ', LOWER BOUND = ',E15.8)
  110 FORMAT (//1X,'CSSIG0 -- N1 =',I4,', N2 =',I4,
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
C Store endpoint data values and test for valid constraint.
C
      H1 = H(N1)
      H2 = H(N2)
      IER = -3
      IF ((RF .LT. 0.  .AND.  MIN(H1,H2) .LT. BND)  .OR.
     .    (RF .GT. 0.  .AND.  BND .LT. MAX(H1,H2)))
     .   GO TO 11
C
C Compute scaled directional derivatives S1,S2 at the end-
C   points (for the direction N1->N2) and test for infinite
C   tension required.
C
      S1 = AL*(GRAD(1,N1)*P2(1) + GRAD(2,N1)*P2(2) +
     .         GRAD(3,N1)*P2(3))/UNORM
      S2 = -AL*(GRAD(1,N2)*P1(1) + GRAD(2,N2)*P1(2) +
     .          GRAD(3,N2)*P1(3))/UNORM
      IER = 1
      SIG = SBIG
      IF ((H1 .EQ. BND  .AND.  RF*S1 .GT. 0.)  .OR.
     .    (H2 .EQ. BND  .AND.  RF*S2 .LT. 0.)) GO TO 10
C
C Test for SIG = 0 sufficient.
C
      IER = 0
      SIG = 0.
      IF (RF*S1 .LE. 0.  .AND.  RF*S2 .GE. 0.) GO TO 10
C
C   Compute first difference S and coefficients A0 and B0
C     of the Hermite cubic interpolant H0(A) = H2 - (S2*R +
C     B0*R**2 + (A0/3)*R**3), where R(A) = (A2-A)/AL.
C
      S = H2 - H1
      T0 = 3.*S - S1 - S2
      A0 = 3.*(S-T0)
      B0 = T0 - S2
      D0 = T0*T0 - S1*S2
C
C   H0 has local extrema in (A1,A2) iff S1*S2 < 0 or
C     (T0*(S1+S2) < 0 and D0 .GE. 0).
C
      IF (S1*S2 .GE. 0.  .AND.  (T0*(S1+S2) .GE. 0.  .OR.
     .    D0 .LT. 0.)) GO TO 10
      IF (A0 .EQ. 0.) THEN
C
C   H0 is quadratic and has an extremum at R = -S2/(2*B0).
C     H0(R) = H2 + S2**2/(4*B0).  Note that A0 = 0 implies
C     2*B0 = S1-S2, and S1*S2 < 0 implies B0 .NE. 0.
C     Also, the extremum is a min iff HBND is a lower bound.
C
        F0 = (BND - H2 - S2*S2/(4.*B0))*RF
      ELSE
C
C   A0 .NE. 0 and H0 has extrema at R = (-B0 +/- SQRT(D0))/
C     A0 = S2/(-B0 -/+ SQRT(D0)), where the negative root
C     corresponds to a min.  The expression for R is chosen
C     to avoid cancellation error.  H0(R) = H2 + (S2*B0 +
C     2*D0*R)/(3*A0).
C
        T = -B0 - SIGN(SQRT(D0),B0)
        R = T/A0
        IF (RF*B0 .GT. 0.) R = S2/T
        F0 = (BND - H2 - (S2*B0+2.*D0*R)/(3.*A0))*RF
      ENDIF
C
C   F0 .GE. 0 iff SIG = 0 is sufficient to satisfy the
C     constraint.
C
      IF (F0 .GE. 0.) GO TO 10
C
C Find a zero of F(SIG) = (BND-H(R))*RF where the derivative
C   of H, HP, vanishes at R.  F is a nondecreasing function,
C   F(0) < 0, and F = FMAX for SIG sufficiently large.
C
C Initialize parameters for the secant method.  The method
C   uses three points:  (SG0,F0), (SIG,F), and (SNEG,FNEG),
C   where SG0 and SNEG are defined implicitly by DSIG = SIG
C   - SG0 and DMAX = SIG - SNEG.  SG0 is initially zero and
C   SNEG is initialized to a sufficiently large value that
C   FNEG > 0.  This value is used only if the initial value
C   of F is negative.
C
      FMAX = MAX(1.D-3,MIN(ABS(H1-BND),ABS(H2-BND)))
      T = MAX(ABS(H1-BND),ABS(H2-BND))
      SIG = MAX(ABS(S1),ABS(S2))/T
      DMAX = SIG*(1.-T/FMAX)
      SNEG = SIG - DMAX
      IF (LUN .GE. 0) WRITE (LUN,120) SIG, SNEG, F0, FMAX
  120 FORMAT (1X,8X,'SIG = ',E15.8,', SNEG = ',E15.8/
     .        1X,9X,'F0 = ',E15.8,', FMAX = ',E15.8/)
      DSIG = SIG
      FNEG = FMAX
      D2 = S2 - S
      D1PD2 = S2 - S1
      NIT = 0
C
C Compute an absolute tolerance FTOL = abs(TOL) and a
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
    6 EMS = EXP(-SIG)
      IF (SIG .LE. .5) THEN
C
C   Use approximations designed to avoid cancellation error
C     (associated with small SIG) in the modified hyperbolic
C     functions.
C
        CALL CSSNHCSH (SIG, SINHM,COSHM,COSHMM)
        C1 = SIG*COSHM*D2 - SINHM*D1PD2
        C2 = SIG*(SINHM+SIG)*D2 - COSHM*D1PD2
        A = C2 - C1
        AA = A/EMS
        E = SIG*SINHM - COSHMM - COSHMM
      ELSE
C
C   Scale SINHM and COSHM by 2*exp(-SIG) in order to avoid
C     overflow.
C
        TM = 1. - EMS
        SSINH = TM*(1.+EMS)
        SSM = SSINH - 2.*SIG*EMS
        SCM = TM*TM
        C1 = SIG*SCM*D2 - SSM*D1PD2
        C2 = SIG*SSINH*D2 - SCM*D1PD2
        AA = 2.*(SIG*TM*D2 + (TM-SIG)*D1PD2)
        A = EMS*AA
        E = SIG*SSINH - SCM - SCM
      ENDIF
C
C   HP(R) = (S2 - (C1*sinh(SIG*R) - C2*coshm(SIG*R))/E)/DT
C     = 0 for ESR = (-B +/- sqrt(D))/A = C/(-B -/+ sqrt(D))
C     where ESR = exp(SIG*R), A = C2-C1, D = B**2 - A*C, and
C     B and C are defined below.
C
      B = E*S2 - C2
      C = C2 + C1
      D = B*B - A*C
      F = 0.
      IF (AA*C .EQ. 0.  .AND.  B .EQ. 0.) GO TO 7
      F = FMAX
      IF (D .LT. 0.) GO TO 7
      T1 = SQRT(D)
      T = -B - SIGN(T1,B)
      RSIG = 0.
      IF (RF*B .LT. 0.  .AND.  AA .NE. 0.) THEN
        IF (T/AA .GT. 0.) RSIG = SIG + LOG(T/AA)
      ENDIF
      IF ((RF*B .GT. 0.  .OR.  AA .EQ. 0.)  .AND.
     .    C/T .GT. 0.) RSIG = LOG(C/T)
      IF ((RSIG .LE. 0.  .OR.  RSIG .GE. SIG)  .AND.
     .    B .NE. 0.) GO TO 7
C
C   H(R) = H2 - (B*SIG*R + C1 + RF*sqrt(D))/(SIG*E).
C
      F = (BND - H2 + (B*RSIG+C1+RF*T1)/(SIG*E))*RF
C
C   Update the number of iterations NIT.
C
    7 NIT = NIT + 1
      IF (LUN .GE. 0) WRITE (LUN,130) NIT, SIG, F
  130 FORMAT (1X,3X,I2,' -- SIG = ',E15.8,', F = ',
     .        E15.8)
      IF (F0*F .LT. 0.) THEN
C
C   F0*F < 0.  Update (SNEG,FNEG) to (SG0,F0) so that F and
C     FNEG always have opposite signs.  If SIG is closer to
C     SNEG than SG0, then swap (SNEG,FNEG) with (SG0,F0).
C
        T1 = DMAX
        T2 = FNEG
        DMAX = DSIG
        FNEG = F0
        IF (ABS(DSIG) .GT. ABS(T1)) THEN
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
C
C   Test for F0 = F = FMAX or F < 0 on the first iteration.
C
      IF (F0 .NE. F  .AND.  (NIT .GT. 1  .OR.  F .GT. 0.))
     .   GO TO 9
C
C   F*F0 > 0 and either the new estimate would be outside
C     of the bracketing interval of length abs(DMAX) or
C     F < 0 on the first iteration.  Reset (SG0,F0) to
C     (SNEG,FNEG).
C
    8 DSIG = DMAX
      F0 = FNEG
C
C   Compute the change in SIG by linear interpolation
C     between (SG0,F0) and (SIG,F).
C
    9 DSIG = -F*DSIG/(F-F0)
      IF (LUN .GE. 0) WRITE (LUN,140) DSIG
  140 FORMAT (1X,8X,'DSIG = ',E15.8)
      IF ( ABS(DSIG) .GT. ABS(DMAX)  .OR.
     .     DSIG*DMAX .GT. 0. ) GO TO 8
C
C   Restrict the step-size such that abs(DSIG) .GE. STOL/2.
C     Note that DSIG and DMAX have opposite signs.
C
      IF (ABS(DSIG) .LT. STOL/2.) DSIG = -SIGN(STOL/2.,DMAX)
C
C   Bottom of loop:  Update SIG, DMAX, and F0.
C
      SIG = SIG + DSIG
      DMAX = DMAX + DSIG
      F0 = F
      GO TO 6
C
C No errors encountered.
C
   10 CSSIG0 = SIG
      IF (IFLGS .LE. 0) RETURN
      SIGMA(LP1) = SIG
      SIGMA(LP2) = SIG
      RETURN
C
C Error termination.
C
   11 CSSIG0 = -1.
      RETURN
      END
