C
C	$Id: csgetsig.f,v 1.5 2008-07-27 03:10:06 haley Exp $
C                                                                      
C                Copyright (C)  2000
C        University Corporation for Atmospheric Research
C                All Rights Reserved
C
C The use of this Software is governed by a License Agreement.
C
      SUBROUTINE CSGETSIG (N,X,Y,Z,H,LIST,LPTR,LEND,GRAD,
     .                   TOL, SIGMA, DSMAX,IER)
      INTEGER N, LIST(*), LPTR(*), LEND(N), IER
      DOUBLE PRECISION X(N), Y(N), Z(N), H(N), GRAD(3,N),
     .                 TOL, SIGMA(*), DSMAX
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
C nodes, this subroutine determines, for each triangulation
C arc, the smallest (nonnegative) tension factor SIGMA such
C that the Hermite interpolatory tension spline H(A), de-
C fined by SIGMA and the endpoint values and directional
C derivatives, preserves local shape properties of the data.
C In order to define the shape properties on an arc, it is
C convenient to map the arc to an interval (A1,A2).  Then,
C denoting the endpoint data values by H1,H2 and the deriva-
C tives (tangential gradient components) by HP1,HP2, and
C letting S = (H2-H1)/(A2-A1), the data properties are
C
C       Monotonicity:  S, HP1, and HP2 are nonnegative or
C                        nonpositive,
C   and
C
C       Convexity:     HP1 .LE. S .LE. HP2  or  HP1 .GE. S
C                        .GE. HP2.
C
C The corresponding properties of H are constant sign of the
C first and second derivatives, respectively.  Note that,
C unless HP1 = S = HP2, infinite tension is required (and H
C is linear on the interval) if S = 0 in the case of mono-
C tonicity, or if HP1 = S or HP2 = S in the case of
C convexity.
C
C   Note that if gradients are to be computed by Subroutine
C CSGRADG or function values and gradients are computed by
C CSSMSURF, it may be desirable to alternate those computa-
C tions (which require tension factors) with calls to this
C subroutine.  This iterative procedure should terminate
C with a call to CSGETSIG in order to ensure that the shape
C properties are preserved, and convergence can be achieved
C (at the cost of optimality) by allowing only increases in
C tension factors (refer to the parameter descriptions for
C SIGMA, DSMAX, and IER).
C
C   Refer to functions CSSIG0, CSSIG1, and CSSIG2 for means of
C selecting minimum tension factors to preserve more general
C properties.
C
C On input:
C
C       N = Number of nodes in the triangulation.  N .GE. 3.
C
C       X,Y,Z = Arrays of length N containing the Cartesian
C               coordinates of the nodes.
C
C       H = Array of length N containing data values at the
C           nodes.  H(I) is associated with (X(I),Y(I),Z(I))
C           for I = 1,...,N.
C
C       LIST,LPTR,LEND = Data structure defining the tri-
C                        angulation.  Refer to STRIPACK
C                        Subroutine CSTRMESH.
C
C       GRAD = Array dimensioned 3 by N whose columns con-
C              tain gradients at the nodes.  GRAD( ,J) must
C              be orthogonal to node J:  GRAD(1,J)*X(J) +
C              GRAD(2,J)*Y(J) + GRAD(3,J)*Z(J) = 0..  Refer
C              to Subroutines CSGRADG, CSGRADL, and CSSMSURF.
C
C       TOL = Tolerance whose magnitude determines how close
C             each tension factor is to its optimal value
C             when nonzero finite tension is necessary and
C             sufficient to satisfy the constraint --
C             abs(TOL) is an upper bound on the magnitude
C             of the smallest (nonnegative) or largest (non-
C             positive) value of the first or second deriva-
C             tive of H in the interval.  Thus, the con-
C             straint is satisfied, but possibly with more
C             tension than necessary.
C
C The above parameters are not altered by this routine.
C
C       SIGMA = Array of length 2*NA = 6*(N-1)-2*NB, where
C               NA and NB are the numbers of arcs and boun-
C               dary nodes, respectively, containing minimum
C               values of the tension factors.  The tension
C               factors are associated with arcs in one-to-
C               one correspondence with LIST entries.  Note
C               that each arc N1-N2 has two LIST entries and
C               thus, the tension factor is stored in both
C               SIGMA(I) and SIGMA(J) where LIST(I) = N2 (in
C               the adjacency list for N1) and LIST(J) = N1
C               (in the list associated with N2).  SIGMA
C               should be set to all zeros if minimal ten-
C               sion is desired, and should be unchanged
C               from a previous call in order to ensure con-
C               vergence of the iterative procedure describ-
C               ed in the header comments.
C
C On output:
C
C       SIGMA = Array containing tension factors for which
C               H(A) preserves the local data properties on
C               each triangulation arc, with the restriction
C               that SIGMA(I) .LE. 85 for all I (unless the
C               input value is larger).  The factors are as
C               small as possible (within the tolerance) but
C               not less than their input values.  If infin-
C               ite tension is required on an arc, the cor-
C               responding factor is SIGMA(I) = 85 (and H
C               is an approximation to the linear inter-
C               polant on the arc), and if neither property
C               is satisfied by the data, then SIGMA(I) = 0
C               (assuming its input value is 0), and thus H
C               is cubic on the arc.
C
C       DSMAX = Maximum increase in a component of SIGMA
C               from its input value.
C
C       IER = Error indicator and information flag:
C             IER = I if no errors were encountered and I
C                     components of SIGMA were altered from
C                     their input values for I .GE. 0.
C             IER = -1 if N < 3.  SIGMA is not altered in
C                      this case.
C             IER = -2 if duplicate nodes were encountered.
C
C STRIPACK modules required by CSGETSIG:  CSLSTPTR, CSSTORE
C
C SSRFPACK modules required by CSGETSIG:  CSARCLEN, CSSNHCSH
C
C Intrinsic functions called by CSGETSIG:  ABS, EXP, MAX, MIN,
C                                          SIGN, SQRT
C
C***********************************************************
C
      INTEGER CSLSTPTR
      DOUBLE PRECISION CSARCLEN, CSSTORE
      INTEGER ICNT, LP1, LP2, LPL, LUN, N1, N2, NIT, NM1
      DOUBLE PRECISION A, AL, C1, C2, COSHM, COSHMM, D0, D1,
     .                 D1D2, D1PD2, D2, DMAX, DSIG, DSM, E,
     .                 EMS, EMS2, F, F0, FMAX, FNEG, FP,
     .                 FTOL, P1(3), P2(3), RTOL, S, S1, S2,
     .                 SBIG, SCM, SGN, SIG, SIGIN, SINHM,
     .                 SSINH, SSM, STOL, T, T0, T1, T2, TM,
     .                 TP1, UN(3), UNORM
C
      DATA SBIG/85./,  LUN/-1/
      NM1 = N - 1
      IF (NM1 .LT. 2) GO TO 11
C
C Compute an absolute tolerance FTOL = abs(TOL) and a
C   relative tolerance RTOL = 100*Macheps.
C
      FTOL = ABS(TOL)
      RTOL = 1.
    1 RTOL = RTOL/2.
        IF (CSSTORE(RTOL+1.) .GT. 1.) GO TO 1
      RTOL = RTOL*200.
C
C Print a heading.
C
      IF (LUN .GE. 0) WRITE (LUN,100) N, FTOL
  100 FORMAT ('1',13X,'CSGETSIG -- N =',I4,', TOL = ',E10.3//)
C
C Initialize change counter ICNT and maximum change DSM for
C   the loop on arcs.
C
      ICNT = 0
      DSM = 0.
C
C Loop on arcs N1-N2 for which N2 > N1.  LPL points to the
C   last neighbor of N1.
C
      DO 10 N1 = 1,NM1
        LPL = LEND(N1)
        LP1 = LPL
C
C   Top of loop on neighbors N2 of N1.
C
    2   LP1 = LPTR(LP1)
        N2 = ABS(LIST(LP1))
        IF (N2 .LE. N1) GO TO 9
C
C Print a message and compute parameters for the arc:
C   nodal coordinates P1 and P2, arc-length AL,
C   UNORM = magnitude of P1 X P2, and
C   SIGIN = input SIGMA value.
C
        IF (LUN .GE. 0) WRITE (LUN,110) N1, N2
  110   FORMAT (/1X,'ARC',I4,' -',I4)
        P1(1) = X(N1)
        P1(2) = Y(N1)
        P1(3) = Z(N1)
        P2(1) = X(N2)
        P2(2) = Y(N2)
        P2(3) = Z(N2)
        AL = CSARCLEN(P1,P2)
        UN(1) = P1(2)*P2(3) - P1(3)*P2(2)
        UN(2) = P1(3)*P2(1) - P1(1)*P2(3)
        UN(3) = P1(1)*P2(2) - P1(2)*P2(1)
        UNORM = SQRT(UN(1)*UN(1)+UN(2)*UN(2)+UN(3)*UN(3))
        IF (UNORM .EQ. 0.  .OR.  AL .EQ. 0.) GO TO 12
        SIGIN = SIGMA(LP1)
        IF (SIGIN .GE. SBIG) GO TO 9
C
C Compute scaled directional derivatives S1,S2 at the end-
C   points (for the direction N1->N2), first difference S,
C   and second differences D1,D2.
C
        S1 = AL*(GRAD(1,N1)*P2(1) + GRAD(2,N1)*P2(2) +
     .               GRAD(3,N1)*P2(3))/UNORM
        S2 = -AL*(GRAD(1,N2)*P1(1) + GRAD(2,N2)*P1(2) +
     .            GRAD(3,N2)*P1(3))/UNORM
        S = H(N2) - H(N1)
        D1 = S - S1
        D2 = S2 - S
        D1D2 = D1*D2
C
C Test for infinite tension required to satisfy either
C   property.
C
        SIG = SBIG
        IF ((D1D2 .EQ. 0.  .AND.  S1 .NE. S2)  .OR.
     .      (S .EQ. 0.  .AND.  S1*S2 .GT. 0.)) GO TO 8
C
C Test for SIGMA = 0 sufficient.  The data satisfies convex-
C   ity iff D1D2 .GE. 0, and D1D2 = 0 implies S1 = S = S2.
C
        SIG = 0.
        IF (D1D2 .LT. 0.) GO TO 4
        IF (D1D2 .EQ. 0.) GO TO 8
        T = MAX(D1/D2,D2/D1)
        IF (T .LE. 2.) GO TO 8
        TP1 = T + 1.
C
C Convexity:  find a zero of F(SIG) = SIG*coshm(SIG)/
C   sinhm(SIG) - TP1.
C
C   F(0) = 2-T < 0, F(TP1) .GE. 0, the derivative of F
C     vanishes at SIG = 0, and the second derivative of F is
C     .2 at SIG = 0.  A quadratic approximation is used to
C     obtain a starting point for the Newton method.
C
        SIG = SQRT(10.*T-20.)
        NIT = 0
C
C   Top of loop:
C
    3   IF (SIG .LE. .5) THEN
          CALL CSSNHCSH (SIG, SINHM,COSHM,COSHMM)
          T1 = COSHM/SINHM
          FP = T1 + SIG*(SIG/SINHM - T1*T1 + 1.)
        ELSE
C
C   Scale sinhm and coshm by 2*exp(-SIG) in order to avoid
C     overflow with large SIG.
C
          EMS = EXP(-SIG)
          SSM = 1. - EMS*(EMS+SIG+SIG)
          T1 = (1.-EMS)*(1.-EMS)/SSM
          FP = T1 + SIG*(2.*SIG*EMS/SSM - T1*T1 + 1.)
        ENDIF
C
        F = SIG*T1 - TP1
        IF (LUN .GE. 0) WRITE (LUN,120) SIG, F, FP
  120   FORMAT (1X,'CONVEXITY -- SIG = ',E15.8,
     .          ', F(SIG) = ',E15.8/1X,35X,'FP(SIG) = ',
     .          E15.8)
        NIT = NIT + 1
C
C   Test for convergence.
C
        IF (FP .LE. 0.) GO TO 8
        DSIG = -F/FP
        IF (ABS(DSIG) .LE. RTOL*SIG  .OR.  (F .GE. 0.  .AND.
     .      F .LE. FTOL)  .OR.  ABS(F) .LE. RTOL) GO TO 8
C
C   Update SIG.
C
        SIG = SIG + DSIG
        GO TO 3
C
C Convexity cannot be satisfied.  Monotonicity can be satis-
C   fied iff S1*S .GE. 0 and S2*S .GE. 0 since S .NE. 0.
C
    4   IF (S1*S .LT. 0.  .OR.  S2*S .LT. 0.) GO TO 8
        T0 = 3.*S - S1 - S2
        D0 = T0*T0 - S1*S2
C
C SIGMA = 0 is sufficient for monotonicity iff S*T0 .GE. 0
C   or D0 .LE. 0.
C
        IF (D0 .LE. 0.  .OR.  S*T0 .GE. 0.) GO TO 8
C
C Monotonicity:  find a zero of F(SIG) = sign(S)*HP(R),
C   where HPP(R) = 0 and HP, HPP denote derivatives of H.
C   F has a unique zero, F(0) < 0, and F approaches
C   abs(S) as SIG increases.
C
C   Initialize parameters for the secant method.  The method
C     uses three points:  (SG0,F0), (SIG,F), and
C     (SNEG,FNEG), where SG0 and SNEG are defined implicitly
C     by DSIG = SIG - SG0 and DMAX = SIG - SNEG.
C
        SGN = SIGN(1.D0,S)
        SIG = SBIG
        FMAX = SGN*(SIG*S-S1-S2)/(SIG-2.)
        IF (FMAX .LE. 0.) GO TO 8
        STOL = RTOL*SIG
        F = FMAX
        F0 = SGN*D0/(3.*(D1-D2))
        FNEG = F0
        DSIG = SIG
        DMAX = SIG
        D1PD2 = D1 + D2
        NIT = 0
C
C   Top of loop:  compute the change in SIG by linear
C     interpolation.
C
    5   DSIG = -F*DSIG/(F-F0)
        IF (LUN .GE. 0) WRITE (LUN,130) DSIG
  130   FORMAT (1X,'MONOTONICITY -- DSIG = ',E15.8)
        IF ( ABS(DSIG) .GT. ABS(DMAX)  .OR.
     .       DSIG*DMAX .GT. 0. ) GO TO 7
C
C   Restrict the step-size such that abs(DSIG) .GE. STOL/2.
C     Note that DSIG and DMAX have opposite signs.
C
        IF (ABS(DSIG) .LT. STOL/2.) DSIG = -SIGN(STOL/2.,
     .                              DMAX)
C
C   Update SIG, F0, and F.
C
        SIG = SIG + DSIG
        F0 = F
        IF (SIG .LE. .5) THEN
C
C   Use approximations to the hyperbolic functions designed
C     to avoid cancellation error with small SIG.
C
          CALL CSSNHCSH (SIG, SINHM,COSHM,COSHMM)
          C1 = SIG*COSHM*D2 - SINHM*D1PD2
          C2 = SIG*(SINHM+SIG)*D2 - COSHM*D1PD2
          A = C2 - C1
          E = SIG*SINHM - COSHMM - COSHMM
        ELSE
C
C   Scale sinhm and coshm by 2*exp(-SIG) in order to avoid
C     overflow with large SIG.
C
          EMS = EXP(-SIG)
          EMS2 = EMS + EMS
          TM = 1. - EMS
          SSINH = TM*(1.+EMS)
          SSM = SSINH - SIG*EMS2
          SCM = TM*TM
          C1 = SIG*SCM*D2 - SSM*D1PD2
          C2 = SIG*SSINH*D2 - SCM*D1PD2
C
C   R is in (0,1) and well-defined iff HPP(T1)*HPP(T2) < 0.
C
          F = FMAX
          IF (C1*(SIG*SCM*D1 - SSM*D1PD2) .GE. 0.) GO TO 6
          A = EMS2*(SIG*TM*D2 + (TM-SIG)*D1PD2)
          IF (A*(C2+C1) .LT. 0.) GO TO 6
          E = SIG*SSINH - SCM - SCM
        ENDIF
C
        F = (SGN*(E*S2-C2) + SQRT(A*(C2+C1)))/E
C
C   Update the number of iterations NIT.
C
    6   NIT = NIT + 1
        IF (LUN .GE. 0) WRITE (LUN,140) NIT, SIG, F
  140   FORMAT (1X,11X,I2,' -- SIG = ',E15.8,', F = ',
     .          E15.8)
C
C   Test for convergence.
C
        STOL = RTOL*SIG
        IF (ABS(DMAX) .LE. STOL  .OR.  (F .GE. 0.  .AND.
     .      F .LE. FTOL)  .OR.  ABS(F) .LE. RTOL) GO TO 8
        DMAX = DMAX + DSIG
        IF (F0*F .GT. 0.  .AND.  ABS(F) .GE. ABS(F0))
     .     GO TO 7
        IF (F0*F .LE. 0.) THEN
C
C   F and F0 have opposite signs.  Update (SNEG,FNEG) to
C     (SG0,F0) so that F and FNEG always have opposite
C     signs.  If SIG is closer to SNEG than SG0 and abs(F)
C     < abs(FNEG), then swap (SNEG,FNEG) with (SG0,F0).
C
          T1 = DMAX
          T2 = FNEG
          DMAX = DSIG
          FNEG = F0
          IF ( ABS(DSIG) .GT. ABS(T1)  .AND.
     .         ABS(F) .LT. ABS(T2) ) THEN
C
            DSIG = T1
            F0 = T2
          ENDIF
        ENDIF
        GO TO 5
C
C   Bottom of loop:  F0*F > 0 and the new estimate would
C     be outside of the bracketing interval of length
C     abs(DMAX).  Reset (SG0,F0) to (SNEG,FNEG).
C
    7   DSIG = DMAX
        F0 = FNEG
        GO TO 5
C
C  Update SIGMA, ICNT, and DSM if necessary.
C
    8   SIG = MIN(SIG,SBIG)
        IF (SIG .GT. SIGIN) THEN
          SIGMA(LP1) = SIG
          LP2 = CSLSTPTR(LEND(N2),N1,LIST,LPTR)
          SIGMA(LP2) = SIG
          ICNT = ICNT + 1
          DSM = MAX(DSM,SIG-SIGIN)
        ENDIF
C
C Bottom of loop on neighbors N2 of N1.
C
    9   IF (LP1 .NE. LPL) GO TO 2
   10   CONTINUE
C
C No errors encountered.
C
      DSMAX = DSM
      IER = ICNT
      RETURN
C
C N < 3.
C
   11 DSMAX = 0.
      IER = -1
      RETURN
C
C Nodes N1 and N2 coincide.
C
   12 DSMAX = DSM
      IER = -2
      RETURN
      END
