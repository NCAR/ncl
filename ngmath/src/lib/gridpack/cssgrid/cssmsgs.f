C
C	$Id: cssmsgs.f,v 1.5 2008-07-27 03:10:08 haley Exp $
C                                                                      
C                Copyright (C)  2000
C        University Corporation for Atmospheric Research
C                All Rights Reserved
C
C The use of this Software is governed by a License Agreement.
C
      SUBROUTINE CSSMSGS (N,X,Y,Z,U,LIST,LPTR,LEND,IFLGS,
     .                  SIGMA,W,P, NIT,DFMAX,F,GRAD, IER)
      INTEGER N, LIST(*), LPTR(*), LEND(N), IFLGS, NIT, IER
      DOUBLE PRECISION X(N), Y(N), Z(N), U(N), SIGMA(*),
     .                 W(N), P, DFMAX, F(N), GRAD(3,N)
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
C   This subroutine solves the symmetric positive definite
C linear system associated with minimizing the quadratic
C functional Q(F,FX,FY,FZ) described in Subroutine CSSMSURF.
C Since the gradient at node K lies in the plane tangent to
C the sphere surface at K, it is effectively defined by only
C two components -- its X and Y components in the coordinate
C system obtained by rotating K to the north pole.  Thus,
C the minimization problem corresponds to an order-3N system
C which is solved by the block Gauss-Seidel method with 3 by
C 3 blocks.
C
C On input:
C
C       N,X,Y,Z,U,LIST,LPTR,LEND,IFLGS,SIGMA,W = Parameters
C           as described in Subroutine CSSMSURF.
C
C       P = Positive smoothing parameter defining Q.
C
C The above parameters are not altered by this routine.
C
C       NIT = Maximum number of iterations to be used.  This
C             maximum will likely be achieved if DFMAX is
C             smaller than the machine precision.  NIT .GE.
C             0.
C
C       DFMAX = Nonnegative convergence criterion.  The
C               method is terminated when the maximum
C               change in a solution F-component between
C               iterations is at most DFMAX.  The change in
C               a component is taken to be the absolute
C               difference relative to 1 plus the old value.
C
C       F = Initial estimate of the first N solution compo-
C           nents.
C
C       GRAD = 3 by N array containing initial estimates of
C              the last 3N solution components (the gradi-
C              ent with FX, FY, and FZ in rows 1, 2, and 3,
C              respectively).
C
C On output:
C
C       NIT = Number of Gauss-Seidel iterations employed.
C
C       DFMAX = Maximum relative change in a solution F-
C               component at the last iteration.
C
C       F = First N solution components -- function values
C           at the nodes.
C
C       GRAD = Last 3N solution components -- gradients at
C              the nodes.
C
C       IER = Error indicator:
C             IER = 0 if no errors were encountered and the
C                     convergence criterion was achieved.
C             IER = 1 if no errors were encountered but con-
C                     vergence was not achieved within NIT
C                     iterations.
C             IER = -1 if N, P, NIT, or DFMAX is outside its
C                      valid range on input.  F and GRAD are
C                      not altered in this case.
C             IER = -2 if all nodes are collinear or the
C                      triangulation is invalid.
C             IER = -3 if duplicate nodes were encountered.
C
C SSRFPACK modules required by CSSMSGS:  CSAPLYRT, CSCONSTR,
C                                        CSGRCOEF, CSSNHCSH
C
C Intrinsic functions called by CSSMSGS:  ABS, ATAN, MAX, SQRT
C
C***********************************************************
C
      INTEGER IFL, ITER, ITMAX, J, K, LPJ, LPL, NN
      DOUBLE PRECISION ALFA, ALFSQ, C11, C12, C13, C22, C23,
     .                 C33, CC22, CC23, CC33, CX, CY, DEN1,
     .                 DEN2, DET, DF, DFMX, DGK(3), DGX,
     .                 DGY, FK, G1, G2, G3, GJK, GKJ, PP,
     .                 R1, R2, R3, RR2, RR3, SIG, SINAL, SX,
     .                 SY, T, T1, T2, T3, T4, T5, T6, TOL,
     .                 XJ, XK, XS, YJ, YK, YS, ZJ, ZK
C
      NN = N
      IFL = IFLGS
      PP = P
      ITMAX = NIT
      TOL = DFMAX
C
C Test for errors in input and initialize iteration count,
C   tension factor, and output value of DFMAX.
C
      IF (NN .LT. 3  .OR.  PP .LE. 0.  .OR.  ITMAX .LT. 0
     .    .OR.  TOL .LT. 0.) GO TO 5
      ITER = 0
      SIG = SIGMA(1)
      DFMX = 0.
C
C Top of iteration loop.
C
    1 IF (ITER .EQ. ITMAX) GO TO 4
      DFMX = 0.
C
C   Loop on nodes.
C
      DO 3 K = 1,NN
        XK = X(K)
        YK = Y(K)
        ZK = Z(K)
        FK = F(K)
        G1 = GRAD(1,K)
        G2 = GRAD(2,K)
        G3 = GRAD(3,K)
C
C   Construct the rotation mapping node K to the north pole.
C
        CALL CSCONSTR (XK,YK,ZK, CX,SX,CY,SY)
C
C   Initialize components of the order-3 system for the
C     change (DF,DGX,DGY) in the K-th solution components.
C
        C11 = PP*W(K)
        C12 = 0.
        C13 = 0.
        C22 = 0.
        C23 = 0.
        C33 = 0.
        R1 = C11*(U(K)-FK)
        R2 = 0.
        R3 = 0.
C
C   Loop on neighbors J of node K.
C
        LPL = LEND(K)
        LPJ = LPL
    2   LPJ = LPTR(LPJ)
          J = ABS(LIST(LPJ))
C
C   Compute the coordinates of J in the rotated system.
C
          T = SX*Y(J) + CX*Z(J)
          YJ = CX*Y(J) - SX*Z(J)
          ZJ = SY*X(J) + CY*T
          XJ = CY*X(J) - SY*T
C
C   Compute arc-length ALFA between K and J, ALFSQ = ALFA*
C     ALFA, SINAL = SIN(ALFA), DEN1 = ALFA*SIN(ALFA)**2, and
C     DEN2 = ALFSQ*SINAL.
C
          ALFA = 2.D0*ATAN(SQRT((1.D0-ZJ)/(1.D0+ZJ)))
          ALFSQ = ALFA*ALFA
          XS = XJ*XJ
          YS = YJ*YJ
          SINAL = SQRT(XS+YS)
          DEN1 = ALFA*(XS+YS)
          DEN2 = ALFSQ*SINAL
C
C   Test for coincident nodes and compute functions of SIG:
C     T1 = SIG*SIG*COSHM/E, T2 = SIG*SINHM/E, and T3 = SIG*
C     (SIG*COSHM-SINHM)/E for E = SIG*SINH - 2*COSHM.
C
          IF (DEN1 .EQ. 0.) GO TO 7
          IF (IFL .GE. 1) SIG = SIGMA(LPJ)
          CALL CSGRCOEF (SIG, T3,T2)
          T1 = T2 + T3
C
C   Update system components for node J.
C
          T4 = 2.*T1/(ALFA*ALFSQ)
          T5 = T1/DEN2
          T6 = T3/DEN1
          C11 = C11 + T4
          C12 = C12 + T5*XJ
          C13 = C13 + T5*YJ
          C22 = C22 + T6*XS
          C23 = C23 + T6*XJ*YJ
          C33 = C33 + T6*YS
          GKJ = G1*X(J) + G2*Y(J) + G3*Z(J)
          GJK = GRAD(1,J)*XK + GRAD(2,J)*YK + GRAD(3,J)*ZK
          R1 = R1 + T4*(F(J)-FK) + T5*(GJK-GKJ)
          T = T5*(F(J)-FK) - T6*GKJ + T2*GJK/DEN1
          R2 = R2 + T*XJ
          R3 = R3 + T*YJ
C
C   Bottom of loop on neighbors.
C
          IF (LPJ .NE. LPL) GO TO 2
C
C   Solve the system associated with the K-th block.
C
        CC22 = C11*C22 - C12*C12
        CC23 = C11*C23 - C12*C13
        CC33 = C11*C33 - C13*C13
        RR2 = C11*R2 - C12*R1
        RR3 = C11*R3 - C13*R1
        DET = CC22*CC33 - CC23*CC23
        IF (DET .EQ. 0.  .OR.  CC22 .EQ. 0.  .OR.
     .      C11 .EQ. 0.) GO TO 6
        DGY = (CC22*RR3 - CC23*RR2)/DET
        DGX = (RR2 - CC23*DGY)/CC22
        DF = (R1 - C12*DGX - C13*DGY)/C11
C
C   Rotate (DGX,DGY,0) back to the original coordinate
C     system, and update GRAD( ,K), F(K), and DFMX.
C
        CALL CSAPLYRT (DGX,DGY,CX,SX,CY,SY, DGK)
        GRAD(1,K) = G1 + DGK(1)
        GRAD(2,K) = G2 + DGK(2)
        GRAD(3,K) = G3 + DGK(3)
        F(K) = FK + DF
        DFMX = MAX(DFMX,ABS(DF)/(1.+ABS(FK)))
    3   CONTINUE
C
C   Increment ITER and test for convergence.
C
      ITER = ITER + 1
      IF (DFMX .GT. TOL) GO TO 1
C
C The method converged.
C
      NIT = ITER
      DFMAX = DFMX
      IER = 0
      RETURN
C
C The method failed to converge within NIT iterations.
C
    4 DFMAX = DFMX
      IER = 1
      RETURN
C
C Invalid input parameter.
C
    5 NIT = 0
      DFMAX = 0.
      IER = -1
      RETURN
C
C Node K and its neighbors are collinear.
C
    6 NIT = 0
      DFMAX = DFMX
      IER = -2
      RETURN
C
C Nodes J and K coincide.
C
    7 NIT = 0
      DFMAX = DFMX
      IER = -3
      RETURN
      END
