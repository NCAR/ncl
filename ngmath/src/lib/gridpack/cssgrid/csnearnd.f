C
C	$Id: csnearnd.f,v 1.5 2008-07-27 03:10:07 haley Exp $
C                                                                      
C                Copyright (C)  2000
C        University Corporation for Atmospheric Research
C                All Rights Reserved
C
C The use of this Software is governed by a License Agreement.
C
      INTEGER FUNCTION CSNEARND (P,IST,N,X,Y,Z,LIST,LPTR,
     .                         LEND, AL)
      INTEGER IST, N, LIST(*), LPTR(*), LEND(N)
      DOUBLE PRECISION P(3), X(N), Y(N), Z(N), AL
C
C***********************************************************
C
C                                              From STRIPACK
C                                            Robert J. Renka
C                                  Dept. of Computer Science
C                                       Univ. of North Texas
C                                           renka@cs.unt.edu
C                                                   07/28/98
C
C   Given a point P on the surface of the unit sphere and a
C Delaunay triangulation created by Subroutine CSTRMESH, this
C function returns the index of the nearest triangulation
C node to P.
C
C   The algorithm consists of implicitly adding P to the
C triangulation, finding the nearest neighbor to P, and
C implicitly deleting P from the triangulation.  Thus, it
C is based on the fact that, if P is a node in a Delaunay
C triangulation, the nearest node to P is a neighbor of P.
C
C
C On input:
C
C       P = Array of length 3 containing the Cartesian coor-
C           dinates of the point P to be located relative to
C           the triangulation.  It is assumed without a test
C           that P(1)**2 + P(2)**2 + P(3)**2 = 1.
C
C       IST = Index of a node at which CSTRFIND begins the
C             search.  Search time depends on the proximity
C             of this node to P.
C
C       N = Number of nodes in the triangulation.  N .GE. 3.
C
C       X,Y,Z = Arrays of length N containing the Cartesian
C               coordinates of the nodes.
C
C       LIST,LPTR,LEND = Data structure defining the trian-
C                        gulation.  Refer to CSTRMESH.
C
C Input parameters are not altered by this function.
C
C On output:
C
C       CSNEARND = Nodal index of the nearest node to P, or 0
C                if N < 3 or the triangulation data struc-
C                ture is invalid.
C
C       AL = Arc length (angular distance in radians) be-
C            tween P and CSNEARND unless CSNEARND = 0.
C
C       Note that the number of candidates for CSNEARND
C       (neighbors of P) is limited to LMAX defined in
C       the PARAMETER statement below.
C
C Modules required by CSNEARND:  CSJRAND, CSLSTPTR, CSTRFIND, CSSTORE
C
C Intrinsic functions called by CSNEARND:  ABS, ACOS
C
C***********************************************************
C
      INTEGER   CSLSTPTR
      INTEGER   LMAX
      PARAMETER (LMAX=25)
      INTEGER   I1, I2, I3, L, LISTP(LMAX), LP, LP1, LP2,
     .          LPL, LPTRP(LMAX), N1, N2, N3, NN, NR, NST
      DOUBLE PRECISION B1, B2, B3, DS1, DSR, DX1, DX2, DX3,
     .                 DY1, DY2, DY3, DZ1, DZ2, DZ3
C
C Local parameters:
C
C B1,B2,B3 =  Unnormalized barycentric coordinates returned
C               by CSTRFIND
C DS1 =       (Negative cosine of the) distance from P to N1
C DSR =       (Negative cosine of the) distance from P to NR
C DX1,..DZ3 = Components of vectors used by the swap test
C I1,I2,I3 =  Nodal indexes of a triangle containing P, or
C               the rightmost (I1) and leftmost (I2) visible
C               boundary nodes as viewed from P
C L =         Length of LISTP/LPTRP and number of neighbors
C               of P
C LMAX =      Maximum value of L
C LISTP =     Indexes of the neighbors of P
C LPTRP =     Array of pointers in 1-1 correspondence with
C               LISTP elements
C LP =        LIST pointer to a neighbor of N1 and LISTP
C               pointer
C LP1,LP2 =   LISTP indexes (pointers)
C LPL =       Pointer to the last neighbor of N1
C N1 =        Index of a node visible from P
C N2 =        Index of an endpoint of an arc opposite P
C N3 =        Index of the node opposite N1->N2
C NN =        Local copy of N
C NR =        Index of a candidate for the nearest node to P
C NST =       Index of the node at which CSTRFIND begins the
C               search
C
C
C Store local parameters and test for N invalid.
C
      NN = N
      IF (NN .LT. 3) GO TO 6
      NST = IST
      IF (NST .LT. 1  .OR.  NST .GT. NN) NST = 1
C
C Find a triangle (I1,I2,I3) containing P, or the rightmost
C   (I1) and leftmost (I2) visible boundary nodes as viewed
C   from P.
C
      CALL CSTRFIND (NST,P,N,X,Y,Z,LIST,LPTR,LEND, B1,B2,B3,
     .             I1,I2,I3)
C
C Test for collinear nodes.
C
      IF (I1 .EQ. 0) GO TO 6
C
C Store the linked list of 'neighbors' of P in LISTP and
C   LPTRP.  I1 is the first neighbor, and 0 is stored as
C   the last neighbor if P is not contained in a triangle.
C   L is the length of LISTP and LPTRP, and is limited to
C   LMAX.
C
      IF (I3 .NE. 0) THEN
        LISTP(1) = I1
        LPTRP(1) = 2
        LISTP(2) = I2
        LPTRP(2) = 3
        LISTP(3) = I3
        LPTRP(3) = 1
        L = 3
      ELSE
        N1 = I1
        L = 1
        LP1 = 2
        LISTP(L) = N1
        LPTRP(L) = LP1
C
C   Loop on the ordered sequence of visible boundary nodes
C     N1 from I1 to I2.
C
    1   LPL = LEND(N1)
          N1 = -LIST(LPL)
          L = LP1
          LP1 = L+1
          LISTP(L) = N1
          LPTRP(L) = LP1
          IF (N1 .NE. I2  .AND.  LP1 .LT. LMAX) GO TO 1
        L = LP1
        LISTP(L) = 0
        LPTRP(L) = 1
      ENDIF
C
C Initialize variables for a loop on arcs N1-N2 opposite P
C   in which new 'neighbors' are 'swapped' in.  N1 follows
C   N2 as a neighbor of P, and LP1 and LP2 are the LISTP
C   indexes of N1 and N2.
C
      LP2 = 1
      N2 = I1
      LP1 = LPTRP(1)
      N1 = LISTP(LP1)
C
C Begin loop:  find the node N3 opposite N1->N2.
C
    2 LP = CSLSTPTR(LEND(N1),N2,LIST,LPTR)
        IF (LIST(LP) .LT. 0) GO TO 3
        LP = LPTR(LP)
        N3 = ABS(LIST(LP))
C
C Swap test:  Exit the loop if L = LMAX.
C
        IF (L .EQ. LMAX) GO TO 4
        DX1 = X(N1) - P(1)
        DY1 = Y(N1) - P(2)
        DZ1 = Z(N1) - P(3)
C
        DX2 = X(N2) - P(1)
        DY2 = Y(N2) - P(2)
        DZ2 = Z(N2) - P(3)
C
        DX3 = X(N3) - P(1)
        DY3 = Y(N3) - P(2)
        DZ3 = Z(N3) - P(3)
        IF ( DX3*(DY2*DZ1 - DY1*DZ2) -
     .       DY3*(DX2*DZ1 - DX1*DZ2) +
     .       DZ3*(DX2*DY1 - DX1*DY2) .LE. 0.D0 ) GO TO 3
C
C Swap:  Insert N3 following N2 in the adjacency list for P.
C        The two new arcs opposite P must be tested.
C
        L = L+1
        LPTRP(LP2) = L
        LISTP(L) = N3
        LPTRP(L) = LP1
        LP1 = L
        N1 = N3
        GO TO 2
C
C No swap:  Advance to the next arc and test for termination
C           on N1 = I1 (LP1 = 1) or N1 followed by 0.
C
    3   IF (LP1 .EQ. 1) GO TO 4
        LP2 = LP1
        N2 = N1
        LP1 = LPTRP(LP1)
        N1 = LISTP(LP1)
        IF (N1 .EQ. 0) GO TO 4
        GO TO 2
C
C Set NR and DSR to the index of the nearest node to P and
C   an increasing function (negative cosine) of its distance
C   from P, respectively.
C
    4 NR = I1
      DSR = -(X(NR)*P(1) + Y(NR)*P(2) + Z(NR)*P(3))
      DO 5 LP = 2,L
        N1 = LISTP(LP)
        IF (N1 .EQ. 0) GO TO 5
        DS1 = -(X(N1)*P(1) + Y(N1)*P(2) + Z(N1)*P(3))
        IF (DS1 .LT. DSR) THEN
          NR = N1
          DSR = DS1
        ENDIF
    5   CONTINUE
      DSR = -DSR
      IF (DSR .GT. 1.0D0) DSR = 1.0D0
      AL = ACOS(DSR)
      CSNEARND = NR
      RETURN
C
C Invalid input.
C
    6 CSNEARND = 0
      RETURN
      END
