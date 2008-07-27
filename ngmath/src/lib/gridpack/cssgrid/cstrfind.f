C
C	$Id: cstrfind.f,v 1.8 2008-07-27 03:10:08 haley Exp $
C                                                                      
C                Copyright (C)  2000
C        University Corporation for Atmospheric Research
C                All Rights Reserved
C
C The use of this Software is governed by a License Agreement.
C
      SUBROUTINE CSTRFIND (NST,P,N,X,Y,Z,LIST,LPTR,LEND, B1,
     .                   B2,B3,I1,I2,I3)
      INTEGER NST, N, LIST(*), LPTR(*), LEND(N), I1, I2, I3
      DOUBLE PRECISION P(3), X(N), Y(N), Z(N), B1, B2, B3
C
C***********************************************************
C
C                                              From STRIPACK
C                                            Robert J. Renka
C                                  Dept. of Computer Science
C                                       Univ. of North Texas
C                                           renka@cs.unt.edu
C                                                   11/30/99
C
C   This subroutine locates a point P relative to a triangu-
C lation created by Subroutine CSTRMESH.  If P is contained in
C a triangle, the three vertex indexes and barycentric coor-
C dinates are returned.  Otherwise, the indexes of the
C visible boundary nodes are returned.
C
C
C On input:
C
C       NST = Index of a node at which CSTRFIND begins its
C             search.  Search time depends on the proximity
C             of this node to P.
C
C       P = Array of length 3 containing the x, y, and z
C           coordinates (in that order) of the point P to be
C           located.
C
C       N = Number of nodes in the triangulation.  N .GE. 3.
C
C       X,Y,Z = Arrays of length N containing the Cartesian
C               coordinates of the triangulation nodes (unit
C               vectors).  (X(I),Y(I),Z(I)) defines node I
C               for I = 1 to N.
C
C       LIST,LPTR,LEND = Data structure defining the trian-
C                        gulation.  Refer to Subroutine
C                        CSTRMESH.
C
C Input parameters are not altered by this routine.
C
C On output:
C
C       B1,B2,B3 = Unnormalized barycentric coordinates of
C                  the central projection of P onto the un-
C                  derlying planar triangle if P is in the
C                  convex hull of the nodes.  These parame-
C                  ters are not altered if I1 = 0.
C
C       I1,I2,I3 = Counterclockwise-ordered vertex indexes
C                  of a triangle containing P if P is con-
C                  tained in a triangle.  If P is not in the
C                  convex hull of the nodes, I1 and I2 are
C                  the rightmost and leftmost (boundary)
C                  nodes that are visible from P, and
C                  I3 = 0.  (If all boundary nodes are vis-
C                  ible from P, then I1 and I2 coincide.)
C                  I1 = I2 = I3 = 0 if P and all of the
C                  nodes are coplanar (lie on a common great
C                  circle.
C
C Modules required by CSTRFIND:  CSJRAND, CSLSTPTR, CSSTORE
C
C Intrinsic function called by CSTRFIND:  ABS
C
C***********************************************************
C
      INTEGER CSJRAND, CSLSTPTR
      INTEGER IX, IY, IZ, LP, N0, N1, N1S, N2, N2S, N3, N4,
     .        NEXT, NF, NL
      DOUBLE PRECISION CSSTORE
      DOUBLE PRECISION DET, EPS, PTN1, PTN2, Q(3), S12, TOL,
     .                 XP, YP, ZP
      DOUBLE PRECISION X0, X1, X2, Y0, Y1, Y2, Z0, Z1, Z2
C
      SAVE    IX, IY, IZ
      DATA    IX/1/, IY/2/, IZ/3/
C
C Local parameters:
C
C EPS =      Machine precision
C IX,IY,IZ = Integer seeds for CSJRAND
C LP =       LIST pointer
C N0,N1,N2 = Nodes in counterclockwise order defining a
C              cone (with vertex N0) containing P, or end-
C              points of a boundary edge such that P Right
C              N1->N2
C N1S,N2S =  Initially-determined values of N1 and N2
C N3,N4 =    Nodes opposite N1->N2 and N2->N1, respectively
C NEXT =     Candidate for I1 or I2 when P is exterior
C NF,NL =    First and last neighbors of N0, or first
C              (rightmost) and last (leftmost) nodes
C              visible from P when P is exterior to the
C              triangulation
C PTN1 =     Scalar product <P,N1>
C PTN2 =     Scalar product <P,N2>
C Q =        (N2 X N1) X N2  or  N1 X (N2 X N1) -- used in
C              the boundary traversal when P is exterior
C S12 =      Scalar product <N1,N2>
C TOL =      Tolerance (multiple of EPS) defining an upper
C              bound on the magnitude of a negative bary-
C              centric coordinate (B1 or B2) for P in a
C              triangle -- used to avoid an infinite number
C              of restarts with 0 <= B3 < EPS and B1 < 0 or
C              B2 < 0 but small in magnitude
C XP,YP,ZP = Local variables containing P(1), P(2), and P(3)
C X0,Y0,Z0 = Dummy arguments for DET
C X1,Y1,Z1 = Dummy arguments for DET
C X2,Y2,Z2 = Dummy arguments for DET
C
C Statement function:
C
C DET(X1,...,Z0) .GE. 0 if and only if (X0,Y0,Z0) is in the
C                       (closed) left hemisphere defined by
C                       the plane containing (0,0,0),
C                       (X1,Y1,Z1), and (X2,Y2,Z2), where
C                       left is defined relative to an ob-
C                       server at (X1,Y1,Z1) facing
C                       (X2,Y2,Z2).
C
      DET (X1,Y1,Z1,X2,Y2,Z2,X0,Y0,Z0) = X0*(Y1*Z2-Y2*Z1)
     .     - Y0*(X1*Z2-X2*Z1) + Z0*(X1*Y2-X2*Y1)
C
C Initialize variables.
C
      XP = P(1)
      YP = P(2)
      ZP = P(3)
      N0 = NST
      IF (N0 .LT. 1  .OR.  N0 .GT. N)
     .  N0 = CSJRAND(N, IX,IY,IZ )
C
C Compute the relative machine precision EPS and TOL.
C
      EPS = 1.D0
    1 EPS = EPS/2.D0
        IF (CSSTORE(EPS+1.D0) .GT. 1.D0) GO TO 1
      EPS = 2.D0*EPS
      TOL = 4.D0*EPS
C
C Set NF and NL to the first and last neighbors of N0, and
C   initialize N1 = NF.
C
    2 LP = LEND(N0)
      NL = LIST(LP)
      LP = LPTR(LP)
      NF = LIST(LP)
      N1 = NF
C
C Find a pair of adjacent neighbors N1,N2 of N0 that define
C   a wedge containing P:  P CSLEFT N0->N1 and P RIGHT N0->N2.
C
      IF (NL .GT. 0) THEN
C
C   N0 is an interior node.  Find N1.
C
    3   IF ( DET(X(N0),Y(N0),Z(N0),X(N1),Y(N1),Z(N1),
     .           XP,YP,ZP) .LT. 0.D0 ) THEN
          LP = LPTR(LP)
          N1 = LIST(LP)
          IF (N1 .EQ. NL) GO TO 6
          GO TO 3
        ENDIF
      ELSE
C
C   N0 is a boundary node.  Test for P exterior.
C
        NL = -NL
        IF ( DET(X(N0),Y(N0),Z(N0),X(NF),Y(NF),Z(NF),
     .           XP,YP,ZP) .LT. 0.D0 ) THEN
C
C   P is to the right of the boundary edge N0->NF.
C
          N1 = N0
          N2 = NF
          GO TO 9
        ENDIF
        IF ( DET(X(NL),Y(NL),Z(NL),X(N0),Y(N0),Z(N0),
     .           XP,YP,ZP) .LT. 0.D0 ) THEN
C
C   P is to the right of the boundary edge NL->N0.
C
          N1 = NL
          N2 = N0
          GO TO 9
        ENDIF
      ENDIF
C
C P is to the left of arcs N0->N1 and NL->N0.  Set N2 to the
C   next neighbor of N0 (following N1).
C
    4 LP = LPTR(LP)
        N2 = ABS(LIST(LP))
        IF ( DET(X(N0),Y(N0),Z(N0),X(N2),Y(N2),Z(N2),
     .           XP,YP,ZP) .LT. 0.D0 ) GO TO 7
        N1 = N2
        IF (N1 .NE. NL) GO TO 4
      IF ( DET(X(N0),Y(N0),Z(N0),X(NF),Y(NF),Z(NF),
     .         XP,YP,ZP) .LT. 0.D0 ) GO TO 6
C
C P is left of or on arcs N0->NB for all neighbors NB
C   of N0.  Test for P = +/-N0.
C
      IF (CSSTORE(ABS(X(N0)*XP + Y(N0)*YP + Z(N0)*ZP))
     .   .LT. 1.D0-4.D0*EPS) THEN
C
C   All points are collinear iff P Left NB->N0 for all
C     neighbors NB of N0.  Search the neighbors of N0.
C     Note:  N1 = NL and LP points to NL.
C
    5   IF ( DET(X(N1),Y(N1),Z(N1),X(N0),Y(N0),Z(N0),
     .           XP,YP,ZP) .GE. 0.D0 ) THEN
          LP = LPTR(LP)
          N1 = ABS(LIST(LP))
          IF (N1 .EQ. NL) GO TO 14
          GO TO 5
        ENDIF
      ENDIF
C
C P is to the right of N1->N0, or P = +/-N0.  Set N0 to N1
C   and start over.
C
      N0 = N1
      GO TO 2
C
C P is between arcs N0->N1 and N0->NF.
C
    6 N2 = NF
C
C P is contained in a wedge defined by geodesics N0-N1 and
C   N0-N2, where N1 is adjacent to N2.  Save N1 and N2 to
C   test for cycling.
C
    7 N3 = N0
      N1S = N1
      N2S = N2
C
C Top of edge-hopping loop:
C
    8 B3 = DET(X(N1),Y(N1),Z(N1),X(N2),Y(N2),Z(N2),XP,YP,ZP)
      IF (B3 .LT. 0.D0) THEN
C
C   Set N4 to the first neighbor of N2 following N1 (the
C     node opposite N2->N1) unless N1->N2 is a boundary arc.
C
        LP = CSLSTPTR(LEND(N2),N1,LIST,LPTR)
        IF (LIST(LP) .LT. 0) GO TO 9
        LP = LPTR(LP)
        N4 = ABS(LIST(LP))
C
C   Define a new arc N1->N2 which intersects the geodesic
C     N0-P.
C
        IF ( DET(X(N0),Y(N0),Z(N0),X(N4),Y(N4),Z(N4),
     .           XP,YP,ZP) .LT. 0.D0 ) THEN
          N3 = N2
          N2 = N4
          N1S = N1
          IF (N2 .NE. N2S  .AND.  N2 .NE. N0) GO TO 8
        ELSE
          N3 = N1
          N1 = N4
          N2S = N2
          IF (N1 .NE. N1S  .AND.  N1 .NE. N0) GO TO 8
        ENDIF
C
C   The starting node N0 or edge N1-N2 was encountered
C     again, implying a cycle (infinite loop).  Restart
C     with N0 randomly selected.
C
        N0 = CSJRAND(N, IX,IY,IZ )
        GO TO 2
      ENDIF
C
C P is in (N1,N2,N3) unless N0, N1, N2, and P are collinear
C   or P is close to -N0.
C
      IF (B3 .GE. EPS) THEN
C
C   B3 .NE. 0.
C
        B1 = DET(X(N2),Y(N2),Z(N2),X(N3),Y(N3),Z(N3),
     .           XP,YP,ZP)
        B2 = DET(X(N3),Y(N3),Z(N3),X(N1),Y(N1),Z(N1),
     .           XP,YP,ZP)
        IF (B1 .LT. -TOL  .OR.  B2 .LT. -TOL) THEN
C
C   Restart with N0 randomly selected.
C
          N0 = CSJRAND(N, IX,IY,IZ )
          GO TO 2
        ENDIF
      ELSE
C
C   B3 = 0 and thus P lies on N1->N2. Compute
C     B1 = Det(P,N2 X N1,N2) and B2 = Det(P,N1,N2 X N1).
C
        B3 = 0.D0
        S12 = X(N1)*X(N2) + Y(N1)*Y(N2) + Z(N1)*Z(N2)
        PTN1 = XP*X(N1) + YP*Y(N1) + ZP*Z(N1)
        PTN2 = XP*X(N2) + YP*Y(N2) + ZP*Z(N2)
        B1 = PTN1 - S12*PTN2
        B2 = PTN2 - S12*PTN1
        IF (B1 .LT. -TOL  .OR.  B2 .LT. -TOL) THEN
C
C   Restart with N0 randomly selected.
C
          N0 = CSJRAND(N, IX,IY,IZ )
          GO TO 2
        ENDIF
      ENDIF
C
C P is in (N1,N2,N3).
C
      I1 = N1
      I2 = N2
      I3 = N3
      IF (B1 .LT. 0.0) B1 = 0.0
      IF (B2 .LT. 0.0) B2 = 0.0
      RETURN
C
C P Right N1->N2, where N1->N2 is a boundary edge.
C   Save N1 and N2, and set NL = 0 to indicate that
C   NL has not yet been found.
C
    9 N1S = N1
      N2S = N2
      NL = 0
C
C           Counterclockwise Boundary Traversal:
C
   10 LP = LEND(N2)
      LP = LPTR(LP)
      NEXT = LIST(LP)
      IF ( DET(X(N2),Y(N2),Z(N2),X(NEXT),Y(NEXT),Z(NEXT),
     .         XP,YP,ZP) .GE. 0.D0 ) THEN
C
C   N2 is the rightmost visible node if P Forward N2->N1
C     or NEXT Forward N2->N1.  Set Q to (N2 X N1) X N2.
C
        S12 = X(N1)*X(N2) + Y(N1)*Y(N2) + Z(N1)*Z(N2)
        Q(1) = X(N1) - S12*X(N2)
        Q(2) = Y(N1) - S12*Y(N2)
        Q(3) = Z(N1) - S12*Z(N2)
        IF (XP*Q(1) + YP*Q(2) + ZP*Q(3) .GE. 0.D0) GO TO 11
        IF (X(NEXT)*Q(1) + Y(NEXT)*Q(2) + Z(NEXT)*Q(3)
     .      .GE. 0.D0) GO TO 11
C
C   N1, N2, NEXT, and P are nearly collinear, and N2 is
C     the leftmost visible node.
C
        NL = N2
      ENDIF
C
C Bottom of counterclockwise loop:
C
      N1 = N2
      N2 = NEXT
      IF (N2 .NE. N1S) GO TO 10
C
C All boundary nodes are visible from P.
C
      I1 = N1S
      I2 = N1S
      I3 = 0
      RETURN
C
C N2 is the rightmost visible node.
C
   11 NF = N2
      IF (NL .EQ. 0) THEN
C
C Restore initial values of N1 and N2, and begin the search
C   for the leftmost visible node.
C
        N2 = N2S
        N1 = N1S
C
C           Clockwise Boundary Traversal:
C
   12   LP = LEND(N1)
        NEXT = -LIST(LP)
        IF ( DET(X(NEXT),Y(NEXT),Z(NEXT),X(N1),Y(N1),Z(N1),
     .           XP,YP,ZP) .GE. 0.D0 ) THEN
C
C   N1 is the leftmost visible node if P or NEXT is
C     forward of N1->N2.  Compute Q = N1 X (N2 X N1).
C
          S12 = X(N1)*X(N2) + Y(N1)*Y(N2) + Z(N1)*Z(N2)
          Q(1) = X(N2) - S12*X(N1)
          Q(2) = Y(N2) - S12*Y(N1)
          Q(3) = Z(N2) - S12*Z(N1)
          IF (XP*Q(1) + YP*Q(2) + ZP*Q(3) .GE. 0.D0)
     .      GO TO 13
          IF (X(NEXT)*Q(1) + Y(NEXT)*Q(2) + Z(NEXT)*Q(3)
     .        .GE. 0.D0) GO TO 13
C
C   P, NEXT, N1, and N2 are nearly collinear and N1 is the
C     rightmost visible node.
C
          NF = N1
        ENDIF
C
C Bottom of clockwise loop:
C
        N2 = N1
        N1 = NEXT
        IF (N1 .NE. N1S) GO TO 12
C
C All boundary nodes are visible from P.
C
        I1 = N1
        I2 = N1
        I3 = 0
        RETURN
C
C N1 is the leftmost visible node.
C
   13   NL = N1
      ENDIF
C
C NF and NL have been found.
C
      I1 = NF
      I2 = NL
      I3 = 0
      RETURN
C
C All points are collinear (coplanar).
C
   14 I1 = 0
      I2 = 0
      I3 = 0
      RETURN
      END
