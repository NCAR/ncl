C
C	$Id: csgradg.f,v 1.5 2008-07-27 03:10:06 haley Exp $
C                                                                      
C                Copyright (C)  2000
C        University Corporation for Atmospheric Research
C                All Rights Reserved
C
C The use of this Software is governed by a License Agreement.
C
      SUBROUTINE CSGRADG (N,X,Y,Z,F,LIST,LPTR,LEND,IFLGS,
     .                  SIGMA, NIT,DGMAX,GRAD, IER)
      INTEGER N, LIST(*), LPTR(*), LEND(N), IFLGS, NIT, IER
      DOUBLE PRECISION X(N), Y(N), Z(N), F(N), SIGMA(*),
     .                 DGMAX, GRAD(3,N)
C
C***********************************************************
C
C                                              From SSRFPACK
C                                            Robert J. Renka
C                                  Dept. of Computer Science
C                                       Univ. of North Texas
C                                           renka@cs.unt.edu
C                                                   07/24/96
C
C   Given a triangulation of N nodes on the unit sphere with
C data values F at the nodes and tension factors SIGMA asso-
C ciated with the arcs, this routine uses a global method
C to compute estimated gradients at the nodes.  The method
C consists of minimizing a quadratic functional Q(G) over
C the N-vector G of gradients, where Q approximates the
C linearized curvature of the restriction to arcs of the
C interpolatory function F defined by Function CSFVAL.  The
C restriction of F to an arc of the triangulation is the
C Hermite interpolatory tension spline defined by the data
C values and tangential gradient components at the endpoints
C of the arc.  Letting D1F(A) and D2F(A) denote first and
C second derivatives of F with respect to a parameter A var-
C ying along a triangulation arc, Q is the sum of integrals
C over the arcs of D2F(A)**2 + ((SIGMA/L)*(D1F(A)-S))**2,
C where L denotes arc-length, SIGMA is the appropriate ten-
C sion factor, and S is the slope of the linear function of
C A which interpolates the values of F at the endpoints of
C the arc.
C
C   Since the gradient at node K lies in the plane tangent
C to the sphere surface at K, it is effectively defined by
C only two components -- its X and Y components in the coor-
C dinate system obtained by rotating K to the north pole.
C Thus, the minimization problem corresponds to an order-2N
C symmetric positive-definite sparse linear system which is
C solved by a block Gauss-Seidel method with 2 by 2 blocks.
C
C   An alternative method, Subroutine CSGRADL, computes a
C local approximation to the gradient at a single node and,
C although less efficient when all gradients are needed, was
C found to be generally more accurate (in the case of uni-
C form zero tension) when the nodal distribution is very
C dense, varies greatly, or does not cover the sphere.
C CSGRADG, on the other hand, was found to be slightly more
C accurate on a uniform distribution of 514 nodes.
C
C On input:
C
C       N = Number of nodes in the triangulation.  N .GE. 3.
C
C       X,Y,Z = Arrays of length N containing Cartesian
C               coordinates of the nodes.  X(I)**2 + Y(I)**2
C               + Z(I)**2 = 1 for I = 1,...,N.
C
C       F = Array of length N containing data values at the
C           nodes.  F(I) is associated with (X(I),Y(I),Z(I))
C           for I = 1,...,N.
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
C The above parameters are not altered by this routine.
C
C       NIT = Maximum number of Gauss-Seidel iterations to
C             be applied.  This maximum will likely be a-
C             chieved if DGMAX is smaller than the machine
C             precision.  NIT .GE. 0.
C
C       DGMAX = Nonnegative convergence criterion.  The
C               method is terminated when the maximum change
C               in a gradient between iterations is at most
C               DGMAX.  The change in a gradient is taken to
C               be the Euclidean norm of the difference (in
C               the rotated coordinate system) relative to 1
C               plus the norm of the old gradient value.
C
C       GRAD = 3 by N array whose columns contain initial
C              solution estimates (zero vectors are suffici-
C              ent).  GRAD(I,J) contains component I of the
C              gradient at node J for I = 1,2,3 (X,Y,Z) and
C              J = 1,...,N.  GRAD( ,J) must be orthogonal to
C              node J -- GRAD(1,J)*X(J) + GRAD(2,J)*Y(J) +
C              GRAD(3,J)*Z(J) = 0.
C
C On output:
C
C       NIT = Number of Gauss-Seidel iterations employed.
C
C       DGMAX = Maximum change in a gradient at the last
C               iteration.
C
C       GRAD = Estimated gradients.  See the description
C              under input parameters.  GRAD is not changed
C              if IER = -1.
C
C       IER = Error indicator:
C             IER = 0 if no errors were encountered and the
C                     convergence criterion was achieved.
C             IER = 1 if no errors were encountered but con-
C                     vergence was not achieved within NIT
C                     iterations.
C             IER = -1 if N or DGMAX is outside its valid
C                      range or NIT .LT. 0 on input.
C             IER = -2 if all nodes are collinear or the
C                      triangulation is invalid.
C             IER = -3 if duplicate nodes were encountered.
C
C SSRFPACK modules required by CSGRADG:  CSAPLYRT, CSCONSTR,
C                                        CSGRCOEF, CSSNHCSH
C
C Intrinsic functions called by CSGRADG:  ATAN, MAX, SQRT
C
C***********************************************************
C
      INTEGER IFL, ITER, J, K, LPJ, LPL, MAXIT, NN
      DOUBLE PRECISION ALFA, A11, A12, A22, CX, CY, D, DEN,
     .                 DET, DGK(3), DGMX, DG1, DG2, FK, G1,
     .                 G2, G3, R1, R2, SD, SIG, SINAL, SX,
     .                 SY, T, TOL, XK, YK, ZK, XJ, YJ, ZJ,
     .                 XS, YS
C
C Local parameters:
C
C ALFA =        Arc-length between nodes K and J
C A11,A12,A22 = Matrix components of the 2 by 2 block A*DG
C                 = R where A is symmetric, (DG1,DG2,0) is
C                 the change in the gradient at K, and R is
C                 the residual
C CX,CY =       Components of a rotation mapping K to the
C                 north pole (0,0,1)
C D =           Function of SIG computed by CSGRCOEF -- factor
C                 in the order-2 system
C DEN =         ALFA*SINAL**2 -- factor in the 2 by 2 system
C DET =         Determinant of the order-2 matrix
C DGK =         Change in GRAD( ,K) from the previous esti-
C                 mate in the original coordinate system
C DGMX =        Maximum change in a gradient between itera-
C                 tions
C DG1,DG2 =     Solution of the 2 by 2 system -- first 2
C                 components of DGK in the rotated coordi-
C                 nate system
C FK =          Data value F(K)
C G1,G2,G3 =    Components of GRAD( ,K)
C IFL =         Local copy of IFLGS
C ITER =        Number of iterations used
C J =           Neighbor of K
C K =           DO-loop and node index
C LPJ =         LIST pointer of node J as a neighbor of K
C LPL =         Pointer to the last neighbor of K
C MAXIT =       Input value of NIT
C NN =          Local copy of N
C R1,R2 =       Components of the residual -- derivatives of
C                 Q with respect to the components of the
C                 gradient at node K
C SD =          Function of SIG computed by CSGRCOEF -- factor
C                 in the order-2 system
C SIG =         Tension factor associated with ARC K-J
C SINAL =       SIN(ALFA) -- magnitude of the vector cross
C                 product between nodes K and J
C SX,SY =       Components of a rotation mapping K to the
C                 north pole (0,0,1)
C T =           Temporary storage for factors in the system
C                 components
C TOL =         Local copy of DGMAX
C XK,YK,ZK =    Coordinates of node K -- X(K), Y(K), Z(K)
C XJ,YJ,ZJ =    Coordinates of node J in the rotated coor-
C                 dinate system
C XS,YS =       XJ**2, YJ**2
C
      NN = N
      IFL = IFLGS
      MAXIT = NIT
      TOL = DGMAX
C
C Test for errors in input, and initialize iteration count,
C   tension factor, and output value of DGMAX.
C
      IF (NN .LT. 3  .OR.  MAXIT .LT. 0  .OR.  TOL .LT. 0.)
     .   GO TO 11
      ITER = 0
      SIG = SIGMA(1)
      DGMX = 0.
C
C Top of iteration loop.
C
    1 IF (ITER .EQ. MAXIT) GO TO 4
      DGMX = 0.
C
C Loop on nodes.
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
C   Initialize components of the 2 by 2 system for the
C     change (DG1,DG2,0) in the K-th solution components
C     (symmetric matrix in A and residual in R).
C
        A11 = 0.
        A12 = 0.
        A22 = 0.
        R1 = 0.
        R2 = 0.
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
C   Compute arc-length ALFA between J and K, SINAL =
C     SIN(ALFA), and DEN = ALFA*SIN(ALFA)**2.
C
          ALFA = 2.D0*ATAN(SQRT((1.D0-ZJ)/(1.D0+ZJ)))
          XS = XJ*XJ
          YS = YJ*YJ
          SINAL = SQRT(XS+YS)
          DEN = ALFA*(XS+YS)
C
C   Test for coincident nodes and compute functions of SIG:
C     D = SIG*(SIG*COSHM-SINHM)/E and SD = SIG*SINHM/E for
C     E = SIG*SINH-2*COSHM.
C
          IF (DEN .EQ. 0.) GO TO 13
          IF (IFL .GE. 1) SIG = SIGMA(LPJ)
          CALL CSGRCOEF (SIG, D,SD)
C
C   Update the system components for node J.
C
          T = D/DEN
          A11 = A11 + T*XS
          A12 = A12 + T*XJ*YJ
          A22 = A22 + T*YS
          T = (D+SD)*(FK-F(J))/(ALFA*ALFA*SINAL) +
     .        ( D*(G1*X(J) + G2*Y(J) + G3*Z(J)) -
     .          SD*(GRAD(1,J)*XK + GRAD(2,J)*YK +
     .                 GRAD(3,J)*ZK) )/DEN
          R1 = R1 - T*XJ
          R2 = R2 - T*YJ
C
C   Bottom of loop on neighbors.
C
          IF (LPJ .NE. LPL) GO TO 2
C
C   Solve the 2 by 2 system and update DGMAX.
C
        DET = A11*A22 - A12*A12
        IF (DET .EQ. 0.  .OR.  A11 .EQ. 0.) GO TO 12
        DG2 = (A11*R2 - A12*R1)/DET
        DG1 = (R1 - A12*DG2)/A11
        DGMX = MAX(DGMX,SQRT(DG1*DG1+DG2*DG2)/
     .             (1.+SQRT(G1*G1+G2*G2+G3*G3)))
C
C   Rotate (DG1,DG2,0) back to the original coordinate
C     system and update GRAD( ,K).
C
        CALL CSAPLYRT (DG1,DG2,CX,SX,CY,SY, DGK)
        GRAD(1,K) = G1 + DGK(1)
        GRAD(2,K) = G2 + DGK(2)
        GRAD(3,K) = G3 + DGK(3)
    3   CONTINUE
C
C   Increment ITER and test for convergence.
C
      ITER = ITER + 1
      IF (DGMX .GT. TOL) GO TO 1
C
C The method converged.
C
      NIT = ITER
      DGMAX = DGMX
      IER = 0
      RETURN
C
C The method failed to converge within NIT iterations.
C
    4 DGMAX = DGMX
      IER = 1
      RETURN
C
C Invalid input parameter.
C
   11 NIT = 0
      DGMAX = 0.
      IER = -1
      RETURN
C
C Node K and its neighbors are collinear.
C
   12 NIT = 0
      DGMAX = DGMX
      IER = -2
      RETURN
C
C Nodes K and J coincide.
C
   13 NIT = 0
      DGMAX = DGMX
      IER = -3
      RETURN
      END
