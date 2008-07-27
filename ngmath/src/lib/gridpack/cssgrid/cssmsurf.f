C
C	$Id: cssmsurf.f,v 1.5 2008-07-27 03:10:08 haley Exp $
C                                                                      
C                Copyright (C)  2000
C        University Corporation for Atmospheric Research
C                All Rights Reserved
C
C The use of this Software is governed by a License Agreement.
C
      SUBROUTINE CSSMSURF (N,X,Y,Z,U,LIST,LPTR,LEND,IFLGS,
     .                   SIGMA,W,SM,SMTOL,GSTOL,LPRNT, F,
     .                   GRAD,IER)
      INTEGER N, LIST(*), LPTR(*), LEND(N), IFLGS, LPRNT,
     .        IER
      DOUBLE PRECISION X(N), Y(N), Z(N), U(N), SIGMA(*),
     .                 W(N), SM, SMTOL, GSTOL, F(N),
     .                 GRAD(3,N)
C
C***********************************************************
C
C                                              From SSRFPACK
C                                            Robert J. Renka
C                                  Dept. of Computer Science
C                                       Univ. of North Texas
C                                           renka@cs.unt.edu
C                                                   07/21/98
C
C   Given a triangulation of N nodes on the unit sphere with
C data values U at the nodes and tension factors SIGMA
C associated with the arcs, this routine determines a set of
C nodal function values F and gradients GRAD = (FX,FY,FZ)
C such that a quadratic functional Q1(F,GRAD) is minimized
C subject to the constraint Q2(F) .LE. SM for Q2(F) =
C (U-F)**T*W*(U-F), where W is a diagonal matrix of positive
C weights.  The functional Q1 is an approximation to the
C linearized curvature over the triangulation of a C-1 fun-
C ction F(V), V a unit vector, which interpolates the nodal
C values and gradients.  Subroutines CSINTRC1 and CSUNIF may be
C called to evaluate F at arbitrary points.
C
C   The smoothing procedure is an extension of the method
C for cubic spline smoothing due to C. Reinsch -- Numer.
C Math., 10 (1967) and 16 (1971).  Refer to Function CSFVAL
C for a further description of the interpolant F.  Letting
C D1F(T) and D2F(T) denote first and second derivatives of F
C with respect to a parameter T varying along a triangula-
C tion arc, Q1 is the sum of integrals over the arcs of
C D2F(T)**2 + ((SIGMA/L)*(D1F(T)-S))**2 where L denotes arc-
C length, SIGMA is the appropriate tension factor, and S is
C the slope of the linear function of T which interpolates
C the values of F at the endpoints of the arc.  Introducing
C a smoothing parameter P, and assuming the constraint is
C active, the problem is equivalent to minimizing Q(P,F,
C GRAD) = Q1(F,GRAD) + P*(Q2(F)-SM).  The secant method is
C used to find a zero of G(P) = 1/SQRT(Q2) - 1/SQRT(SM)
C where F(P) satisfies the order-3N symmetric positive def-
C inite linear system obtained by setting the gradient of Q
C (treated as a function of F and GRAD with GRAD tangent to
C the sphere surface) to zero.  The linear system is solved
C by the block Gauss-Seidel method (refer to CSSMSGS).
C
C   Note that the method can also be used to select grad-
C ients for the interpolation problem (F = U, SM = 0, and P
C infinite).  This is achieved by a call to Subroutine
C CSGRADG.
C
C On input:
C
C       N = Number of nodes in the triangulation.  N .GE. 3.
C
C       X,Y,Z = Arrays of length N containing Cartesian
C               coordinates of the nodes.
C
C       U = Array of length N containing data values at the
C           nodes.
C
C       LIST,LPTR,LEND = Data structure defining the trian-
C                        gulation.  Refer to STRIPACK
C                        Subroutine CSTRMESH.
C
C       IFLGS = Tension factor option:
C               IFLGS .LE. 0 if a single uniform tension
C                            factor is to be used.
C               IFLGS .GE. 1 if variable tension is desired.
C
C       SIGMA = Uniform tension factor (IFLGS .LE. 0), or
C               array containing tension factors associated
C               with arcs in one-to-one correspondence with
C               LIST entries (IFLGS .GE. 1).  Refer to Sub-
C               programs CSGETSIG, CSSIG0, CSSIG1, and CSSIG2.
C
C       W = Array of length N containing positive weights
C           associated with the data values.  The recommend-
C           ed value of W(I) is 1/DU**2 where DU is the
C           standard deviation associated with U(I).  DU**2
C           is the expected value of the squared error in
C           the measurement of U(I).  (The mean error is
C           assumed to be zero.)
C
C       SM = Positive parameter specifying an upper bound on
C            Q2(F).  Note that F is constant (and Q2(F)
C            is minimized) if SM is sufficiently large that
C            the constraint is not active.  It is recommend-
C            ed that SM satisfy N-SQRT(2N) .LE. SM .LE. N+
C            SQRT(2N).
C
C       SMTOL = Parameter in the open interval (0,1) speci-
C               fying the relative error allowed in satisfy-
C               ing the constraint -- the constraint is
C               assumed to be satisfied if SM*(1-SMTOL) .LE.
C               Q2 .LE. SM*(1+SMTOL).  A reasonable value
C               for SMTOL is SQRT(2/N).
C
C       GSTOL = Nonnegative tolerance defining the conver-
C               gence criterion for the Gauss-Seidel method.
C               Refer to parameter DFMAX in Subroutine
C               CSSMSGS.  A recommended value is .05*DU**2,
C               where DU is an average standard deviation
C               in the data values.
C
C       LPRNT = Logical unit on which diagnostic messages
C               are printed, or negative integer specifying
C               no diagnostics.  For each secant iteration,
C               the following values are printed:  P, G(P),
C               NIT, DFMAX, and DP, where NIT denotes the
C               number of Gauss-Seidel iterations used in
C               the computation of G, DFMAX denotes the max-
C               imum relative change in a solution component
C               in the last Gauss-Seidel iteration, and DP
C               is the change in P computed by linear inter-
C               polation between the current point (P,G) and
C               a previous point.
C
C Input parameters are not altered by this routine.
C
C On output:
C
C       F = Array of length N containing nodal function val-
C           ues unless IER < 0.
C
C       GRAD = 3 by N array whose columns contain gradients
C              of F at the nodes unless IER < 0.
C
C       IER = Error indicator and information flag:
C             IER = 0 if no errors were encountered and the
C                     constraint is active -- Q2(F) is ap-
C                     proximately equal to SM.
C             IER = 1 if no errors were encountered but the
C                     constraint is not active -- F and GRAD
C                     are the values and gradients of a con-
C                     stant function which minimizes Q2(F),
C                     and Q1 = 0.
C             IER = 2 if the constraint could not be satis-
C                     fied to within SMTOL due to
C                     ill-conditioned linear systems.
C             IER = -1 if N, W, SM, SMTOL, or GSTOL is out-
C                      side its valid range on input.
C             IER = -2 if all nodes are collinear or the
C                      triangulation is invalid.
C             IER = -3 if duplicate nodes were encountered.
C
C SSRFPACK modules required by CSSMSURF:  CSAPLYRT, CSCONSTR,
C                                         CSGRCOEF, CSSMSGS,
C                                         CSSNHCSH
C
C Intrinsic functions called by CSSMSURF:  ABS, SQRT
C
C***********************************************************
C
      INTEGER I, IERR, ITER, ITMAX, LUN, NIT, NITMAX, NN
      DOUBLE PRECISION C, DFMAX, DMAX, DP, G, G0, GNEG, P,
     .                 Q2, Q2MAX, Q2MIN, S, SUMW, TOL, WI
C
C Local parameters:
C
C ITMAX = Maximum number of secant iterations.
C LUN = Local copy of LPRNT.
C NITMAX = Maximum number of Gauss-Seidel iterations for
C          each secant iteration.
C NN = Local copy of N.
C TOL = Local copy of GSTOL.
C
      DATA ITMAX/50/,  NITMAX/40/
C
      NN = N
      TOL = GSTOL
      LUN = LPRNT
      IF (LUN .GT. 99) LUN = -1
C
C Test for errors and initialize F to the weighted least
C   squares fit of a constant function to the data.
C
      IER = -1
      IF (NN .LT. 3  .OR.  SM .LE. 0.  .OR.  SMTOL .LE. 0.
     .    .OR.  SMTOL .GE. 1.  .OR.  TOL .LE. 0.) RETURN
      C = 0.
      SUMW = 0.
      DO 1 I = 1,NN
        WI = W(I)
        IF (WI .LE. 0.) RETURN
        C = C + WI*U(I)
        SUMW = SUMW + WI
    1   CONTINUE
      C = C/SUMW
C
C Compute nodal values and gradients, and accumulate Q2 =
C   (U-F)**T*W*(U-F).
C
      Q2 = 0.
      DO 2 I = 1,NN
        F(I) = C
        GRAD(1,I) = 0.
        GRAD(2,I) = 0.
        GRAD(3,I) = 0.
        Q2 = Q2 + W(I)*(U(I)-F(I))**2
    2   CONTINUE
C
C Compute bounds on Q2 defined by SMTOL, and test for the
C   constraint satisfied by the constant fit.
C
      Q2MIN = SM*(1.-SMTOL)
      Q2MAX = SM*(1.+SMTOL)
      IF (Q2 .LE. Q2MAX) THEN
C
C The constraint is satisfied by a constant function.
C
        IER = 1
        IF (LUN .GE. 0) WRITE (LUN,100)
  100   FORMAT (///1X,'CSSMSURF -- THE CSCONSTRAINT IS NOT ',
     .          'ACTIVE AND THE FITTING FCN IS CONSTANT.')
        RETURN
      ENDIF
C
C Compute G0 = G(0) and print a heading.
C
      IER = 0
      S = 1./SQRT(SM)
      G0 = 1./SQRT(Q2) - S
      IF (LUN .GE. 0) WRITE (LUN,110) SM, TOL, NITMAX, G0
  110 FORMAT (///1X,'CSSMSURF -- SM = ',E10.4,', GSTOL = ',
     .        E7.1,', NITMAX = ',I2,', G(0) = ',E15.8)
C
C G(P) is strictly increasing and concave, and G(0) .LT. 0.
C   Initialize parameters for the secant method.  The method
C   uses three points -- (P0,G0), (P,G), and (PNEG,GNEG)
C   where P0 and PNEG are defined implicitly by DP = P - P0
C   and DMAX = P - PNEG.
C
      P = 10.*SM
      DP = P
      DMAX = 0.
      ITER = 0
C
C Top of loop -- compute G.
C
    3 NIT = NITMAX
      DFMAX = TOL
      CALL CSSMSGS (NN,X,Y,Z,U,LIST,LPTR,LEND,IFLGS,SIGMA,W,
     .            P, NIT,DFMAX,F,GRAD, IERR)
      IF (IERR .LT. 0) IER = IERR
C
C   IERR = -1 in CSSMSGS could be caused by P = 0 as a result
C     of inaccurate solutions to ill-conditioned systems.
C
      IF (IERR .EQ. -1) IER = 2
      IF (IERR .LT. 0) RETURN
      Q2 = 0.
      DO 4 I = 1,NN
        Q2 = Q2 + W(I)*(U(I)-F(I))**2
    4   CONTINUE
      G = 1./SQRT(Q2) - S
      ITER = ITER + 1
      IF (LUN .GE. 0) WRITE (LUN,120) ITER, P, G, NIT, DFMAX
  120 FORMAT (/1X,I2,' -- P = ',E15.8,', G = ',E15.8,
     .        ', NIT = ',I2,', DFMAX = ',E12.6)
C
C   Test for convergence.
C
      IF (Q2MIN .LE. Q2  .AND.  Q2 .LE. Q2MAX) RETURN
      IF (ITER .GE. ITMAX) THEN
        IER = 2
        RETURN
      ENDIF
      IF (DMAX .EQ. 0.  .AND.  G .LE. 0.) THEN
C
C   Increase P until G(P) > 0.
C
        P = 10.*P
        DP = P
        GO TO 3
      ENDIF
C
C   A bracketing interval [P0,P] has been found.
C
      IF (G0*G .LE. 0.) THEN
C
C   G0*G < 0.  Update (PNEG,GNEG) to (P0,G0) so that G
C     and GNEG always have opposite signs.
C
        DMAX = DP
        GNEG = G0
      ENDIF
C
C   Compute the change in P by linear interpolation between
C     (P0,G0) and (P,G).
C
    5 DP = -G*DP/(G-G0)
      IF (LUN .GE. 0) WRITE (LUN,130) DP
  130 FORMAT (1X,5X,'DP = ',E15.8)
      IF (ABS(DP) .GT. ABS(DMAX)) THEN
C
C   G0*G .GT. 0 and the new estimate would be outside of the
C     bracketing interval of length ABS(DMAX).  Reset
C     (P0,G0) to (PNEG,GNEG).
C
        DP = DMAX
        G0 = GNEG
        GO TO 5
      ENDIF
C
C   Bottom of loop -- update P, DMAX, and G0.
C
      P = P + DP
      DMAX = DMAX + DP
      G0 = G
      GO TO 3
      END
