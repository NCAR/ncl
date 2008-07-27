C
C	$Id: csintrc1.f,v 1.7 2008-07-27 03:10:07 haley Exp $
C                                                                      
C                Copyright (C)  2000
C        University Corporation for Atmospheric Research
C                All Rights Reserved
C
C The use of this Software is governed by a License Agreement.
C
      SUBROUTINE CSINTRC1 (N,PLAT,PLON,X,Y,Z,F,LIST,LPTR,LEND,
     .                   IFLGS,SIGMA,IFLGG,GRAD, IST, FP,
     .                   IER)
      INTEGER N, LIST(*), LPTR(*), LEND(N), IFLGS, IFLGG,
     .        IST, IER
      DOUBLE PRECISION PLAT, PLON, X(N), Y(N), Z(N), F(N),
     .                 SIGMA(*), GRAD(3,N), FP
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
C sphere, along with data values and gradients at the nodes,
C this routine computes a value F(P), where F interpolates
C the nodal data and is once-continuously differentiable
C over the convex hull of the nodes.  Refer to Function CSFVAL
C for further details.  If P is not contained in a triangle,
C an extrapolated value is computed by extending F beyond
C the boundary in a continuous fashion.
C
C On input:
C
C       N = Number of nodes in the triangulation.  N .GE. 3
C           and N .GE. 7 if IFLGG .LE. 0.
C
C       PLAT,PLON = Latitude and longitude in radians of the
C                   point P at which F is to be evaluated.
C
C       X,Y,Z = Arrays of length N containing Cartesian
C               coordinates of the nodes.
C
C       F = Array of length N containing values of F at the
C           nodes:  F(I) = F(X(I),Y(I),Z(I)).
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
C               programs CSFVAL, CSGETSIG, CSSIG0, CSSIG1, and CSSIG2.
C
C       IFLGG = Gradient option:
C               IFLGG .LE. 0 if CSINTRC1 is to provide grad-
C                            ient estimates as needed (from
C                            CSGRADL).
C               IFLGG .GE. 1 if gradients are user-provided
C                            in GRAD.  This is more effici-
C                            ent if CSINTRC1 is to be called
C                            several times.
C
C       GRAD = 3 by N array whose I-th column contains
C              an estimated gradient at node I if IFLGG .GE.
C              1, or unused dummy parameter if IFLGG .LE. 0.
C              Refer to Subroutines CSGRADL and CSGRADG.
C
C       IST = Index of the starting node in the search for a
C             triangle containing P.  The output value of
C             IST from a previous call may be a good choice.
C             1 .LE. IST .LE. N.
C
C Input parameters other than IST are not altered by this
C   routine.
C
C On output:
C
C       IST = Index of one of the vertices of the triangle
C             containing P (or a boundary node if P is not
C             contained in a triangle) unless IER = 4.
C
C       FP = Value of F at P unless IER < 0, in which case
C            FP is not defined.
C
C       IER = Error indicator and information flag:
C               =  0 - no error.
C               =  1 - invalid number of input points (must be
C                      greater than 3).
C               =  4 - first three nodes are collinear.
C               =  7 - vertex of a triangle containing an interpolation
C                      point is outside its valid range.
C               =  8 - the angular distance between an interpolated
C                      point and the nearest point of the
C                      triangulation is at least 90 degrees.
C               =  9 - not enough input points to calculate a gradient.
C
C STRIPACK modules required by CSINTRC1: CSJRAND, CSLSTPTR, CSSTORE,
C                                        CSTRFIND
C                    (and optionally)  CSGETNP if IFLGG .LE. 0
C
C SSRFPACK modules required by CSINTRC1:  CSARCINT, CSARCLEN,
C                                         CSFVAL, CSHVAL, CSSNHCSH
C              (and if IFLGG .LE. 0)  CSAPLYR, CSAPLYRT, CSCONSTR,
C                                       CSGIVENS, CSGRADL,
C                                       CSROTATE, CSSETUP
C
C Intrinsic functions called by CSINTRC1:  COS, SIN, SQRT
C
C***********************************************************
C
      INTEGER CSLSTPTR
      DOUBLE PRECISION CSARCLEN, CSFVAL
      INTEGER I, IERR, I1, I2, I3, LP, N1, N2, NN
      DOUBLE PRECISION A, B1, B2, B3, DUM(3), FQ, GQ(3),
     .                 GQN, G1(3), G2(3), G3(3), P(3),
     .                 P1(3), P2(3), P3(3), PTGQ, PTN1,
     .                 PTN2, Q(3), QNORM, S1, S2, S3, S12,
     .                 SUM
C
C Local parameters:
C
C A =        Angular separation between P and Q
C B1,B2,B3 = Barycentric coordinates of the central projec-
C              tion of P onto the underlying planar triangle,
C              or (B1 and B2) projection of Q onto the
C              underlying line segment N1-N2 when P is
C              exterior.  Unnormalized coordinates are
C              computed by CSTRFIND when P is in a triangle.
C DUM =      Dummy parameter for CSARCINT
C FQ,GQ =    Interpolated value and gradient at Q
C GQN =      Negative of the component of GQ in the direction
C              Q->P
C G1,G2,G3 = Gradients at I1, I2, and I3, or (G1 and G2) at
C              N1 and N2
C I =        DO-loop index
C IERR =     Error flag for calls to CSGRADL
C I1,I2,I3 = Vertex indexes returned by CSTRFIND
C LP =       LIST pointer
C N1,N2 =    Indexes of the endpoints of a boundary arc when
C              P is exterior (not contained in a triangle)
C NN =       Local copy of N
C P =        Cartesian coordinates of P
C P1,P2,P3 = Cartesian coordinates of the vertices I1, I2,
C              and I3, or (P1 and P2) coordinates of N1 and
C              N2 if P is exterior
C PTGQ =     Scalar product (P,GQ) -- factor of the component
C              of GQ in the direction Q->P
C PTN1 =     Scalar product (P,N1) -- factor of B1 and B2
C PTN2 =     Scalar product (P,N2) -- factor of B1 and B2
C Q =        Closest boundary point to P when P is exterior
C QNORM =    Factor used to normalize Q
C S1,S2,S3 = Tension factors associated with the triangle
C              sides opposite I1, I2, and I3, or (S1) the
C              boundary arc N1-N2
C S12 =      Scalar product (N1,N2) -- factor of B1 and B2
C SUM =      Quantity used to normalize the barycentric
C              coordinates
C
      NN = N
      IF (NN .LT. 3) THEN
        GO TO 100
      ELSE IF (IFLGG .LE. 0  .AND.  NN .LT. 7) THEN
        GO TO 110
      ELSE IF (IST .LT. 1  .OR.  IST .GT. NN) THEN
        GO TO 120
      ENDIF
C
C Transform (PLAT,PLON) to Cartesian coordinates.
C
      P(1) = COS(PLAT)*COS(PLON)
      P(2) = COS(PLAT)*SIN(PLON)
      P(3) = SIN(PLAT)
C
C Locate P with respect to the triangulation.
C
      CALL CSTRFIND (IST,P,NN,X,Y,Z,LIST,LPTR,LEND, B1,B2,B3,
     .             I1,I2,I3)
      IF (I1 .EQ. 0) GO TO 130
      IST = I1
      IF (I3 .NE. 0) THEN
C
C P is contained in the triangle (I1,I2,I3).  Store the
C   vertex coordinates, gradients, and tension factors in
C   local variables.
C
        P1(1) = X(I1)
        P1(2) = Y(I1)
        P1(3) = Z(I1)
        P2(1) = X(I2)
        P2(2) = Y(I2)
        P2(3) = Z(I2)
        P3(1) = X(I3)
        P3(2) = Y(I3)
        P3(3) = Z(I3)
        IF (IFLGG .GT. 0) THEN
C
C   Gradients are user-provided.
C
          DO 1 I = 1,3
            G1(I) = GRAD(I,I1)
            G2(I) = GRAD(I,I2)
            G3(I) = GRAD(I,I3)
    1       CONTINUE
        ELSE
C
C   Compute gradient estimates at the vertices.
C
          CALL CSGRADL (NN,I1,X,Y,Z,F,LIST,LPTR,LEND, G1,IERR)
          IF (IERR .LT. 0) GO TO 130
          CALL CSGRADL (NN,I2,X,Y,Z,F,LIST,LPTR,LEND, G2,IERR)
          IF (IERR .LT. 0) GO TO 130
          CALL CSGRADL (NN,I3,X,Y,Z,F,LIST,LPTR,LEND, G3,IERR)
          IF (IERR .LT. 0) GO TO 130
        ENDIF
C
        IF (IFLGS .GT. 0) THEN
C
C   Variable tension:
C
          LP = CSLSTPTR(LEND(I2),I3,LIST,LPTR)
          S1 = SIGMA(LP)
          LP = CSLSTPTR(LEND(I3),I1,LIST,LPTR)
          S2 = SIGMA(LP)
          LP = CSLSTPTR(LEND(I1),I2,LIST,LPTR)
          S3 = SIGMA(LP)
        ELSE
C
C   Uniform tension:
C
          S1 = SIGMA(1)
          S2 = S1
          S3 = S1
        ENDIF
C
C Normalize the coordinates.
C
        SUM = B1 + B2 + B3
        B1 = B1/SUM
        B2 = B2/SUM
        B3 = B3/SUM
        FP = CSFVAL(B1,B2,B3,P1,P2,P3,F(I1),F(I2),F(I3),G1,
     .            G2,G3,S1,S2,S3)
        IER = 0
        RETURN
      ENDIF
C
C P is exterior to the triangulation, and I1 and I2 are
C   boundary nodes which are visible from P.  Extrapolate to
C   P by linear (with respect to arc-length) interpolation
C   of the value and directional derivative (gradient comp-
C   onent in the direction Q->P) of the interpolatory
C   surface at Q where Q is the closest boundary point to P.
C
C Determine Q by traversing the boundary starting from I1.
C
      N1 = I1
      PTN1 = P(1)*X(N1) + P(2)*Y(N1) + P(3)*Z(N1)
      IF (I1 .NE. I2) GO TO 3
C
C All boundary nodes are visible from P.  Find a boundary
C   arc N1->N2 such that P Left (N2 X N1)->N1.
C
C Counterclockwise boundary traversal:
C   Set N2 to the first neighbor of N1.
C
    2 LP = LEND(N1)
        LP = LPTR(LP)
        N2 = LIST(LP)
C
C Compute inner products (N1,N2) and (P,N2), and compute
C   B2 = Det(P,N1,N2 X N1).
C
        S12 = X(N1)*X(N2) + Y(N1)*Y(N2) + Z(N1)*Z(N2)
        PTN2 = P(1)*X(N2) + P(2)*Y(N2) + P(3)*Z(N2)
        B2 = PTN2 - S12*PTN1
        IF (B2 .LE. 0.) GO TO 3
C
C P Right (N2 X N1)->N1:  iterate.
C
        N1 = N2
        I1 = N1
        PTN1 = PTN2
        GO TO 2
C
C P Left (N2 X N1)->N1 where N2 is the first neighbor of N1.
C   Clockwise boundary traversal:
C
    3 N2 = N1
        PTN2 = PTN1
C
C Set N1 to the last neighbor of N2 and test for
C   termination.
C
        LP = LEND(N2)
        N1 = -LIST(LP)
        IF (N1 .EQ. I1) GO TO 140
C
C Compute inner products (N1,N2) and (P,N1).
C
        S12 = X(N1)*X(N2) + Y(N1)*Y(N2) + Z(N1)*Z(N2)
        PTN1 = P(1)*X(N1) + P(2)*Y(N1) + P(3)*Z(N1)
C
C Compute B2 = Det(P,N1,N2 X N1) = Det(Q,N1,N2 X N1)*(P,Q).
C
        B2 = PTN2 - S12*PTN1
        IF (B2 .LE. 0.) GO TO 3
C
C Compute B1 = Det(P,N2 X N1,N2) = Det(Q,N2 X N1,N2)*(P,Q).
C
      B1 = PTN1 - S12*PTN2
      IF (B1 .LE. 0.) THEN
C
C Q = N2.  Store value, coordinates, and gradient at Q.
C
        FQ = F(N2)
        Q(1) = X(N2)
        Q(2) = Y(N2)
        Q(3) = Z(N2)
        IF (IFLGG .GT. 0) THEN
          DO 4 I = 1,3
            GQ(I) = GRAD(I,N2)
    4       CONTINUE
        ELSE
          CALL CSGRADL (NN,N2,X,Y,Z,F,LIST,LPTR,LEND, GQ,IERR)
          IF (IERR .LT. 0) GO TO 130
        ENDIF
C
C Extrapolate to P:  FP = FQ + A*(GQ,Q X (PXQ)/SIN(A)),
C   where A is the angular separation between Q and P,
C   and Sin(A) is the magnitude of P X Q.
C
        A = CSARCLEN(Q,P)
        PTGQ = P(1)*GQ(1) + P(2)*GQ(2) + P(3)*GQ(3)
        FP = FQ
        IF (A .NE. 0.) FP = FP + PTGQ*A/SIN(A)
        IER = 0
        RETURN
      ENDIF
C
C P Strictly Left (N2 X N1)->N2 and P Strictly Left
C   N1->(N2 X N1).  Thus Q lies on the interior of N1->N2.
C   Store coordinates of N1 and N2 in local variables.
C
      P1(1) = X(N1)
      P1(2) = Y(N1)
      P1(3) = Z(N1)
      P2(1) = X(N2)
      P2(2) = Y(N2)
      P2(3) = Z(N2)
C
C Compute the central projection of Q onto P2-P1 and
C   normalize to obtain Q.
C
      QNORM = 0.
      DO 5 I = 1,3
        Q(I) = B1*P1(I) + B2*P2(I)
        QNORM = QNORM + Q(I)*Q(I)
    5   CONTINUE
      QNORM = SQRT(QNORM)
      DO 6 I = 1,3
        Q(I) = Q(I)/QNORM
    6   CONTINUE
C
C Store gradients at N1 and N2 and tension factor S1.
C
      IF (IFLGG .GT. 0) THEN
        DO 7 I = 1,3
          G1(I) = GRAD(I,N1)
          G2(I) = GRAD(I,N2)
    7     CONTINUE
      ELSE
        CALL CSGRADL (NN,N1,X,Y,Z,F,LIST,LPTR,LEND, G1,IERR)
        IF (IERR .LT. 0) GO TO 130
        CALL CSGRADL (NN,N2,X,Y,Z,F,LIST,LPTR,LEND, G2,IERR)
        IF (IERR .LT. 0) GO TO 130
      ENDIF
C
      IF (IFLGS .LE. 0) S1 = SIGMA(1)
      IF (IFLGS .GE. 1) S1 = SIGMA(LP)
C
C Compute an interpolated value and normal gradient
C   component at Q.
C
      CALL CSARCINT (Q,P1,P2,F(N1),F(N2),G1,G2,S1, FQ,DUM,GQN)
C
C Extrapolate to P:  the normal gradient component GQN is
C   the negative of the component in the direction Q->P.
C
      FP = FQ - GQN*CSARCLEN(Q,P)
      IER = 0
      RETURN
C
C  Invalid number of input points (must be greater than 3).
C
  100 CONTINUE
      IER = 1
      RETURN
C
C  Not enough input points to calculate a gradient.
C
  110 CONTINUE
      IER = 9
      RETURN
C
C  Vertex of a triangle containing an interpolation point is outside
C  its valid range.
C
  120 CONTINUE
      IER = 7
      RETURN
C
C  First three nodes are collinear.
C
  130 CONTINUE
      IER = 4
      RETURN
C
  140 CONTINUE
      IER = 8
      RETURN
C
      END
