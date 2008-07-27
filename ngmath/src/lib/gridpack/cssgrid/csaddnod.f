C
C	$Id: csaddnod.f,v 1.6 2008-07-27 03:10:05 haley Exp $
C                                                                      
C                Copyright (C)  2000
C        University Corporation for Atmospheric Research
C                All Rights Reserved
C
C The use of this Software is governed by a License Agreement.
C
      SUBROUTINE CSADDNOD (NST,K,X,Y,Z, LIST,LPTR,LEND,
     .                   LNEW, IER)
      INTEGER NST, K, LIST(*), LPTR(*), LEND(K), LNEW, IER
      DOUBLE PRECISION X(K), Y(K), Z(K)
C
C***********************************************************
C
C                                              From STRIPACK
C                                            Robert J. Renka
C                                  Dept. of Computer Science
C                                       Univ. of North Texas
C                                           renka@cs.unt.edu
C                                                   10/10/99
C
C   This subroutine adds node K to a triangulation of the
C convex hull of nodes 1,...,K-1, producing a triangulation
C of the convex hull of nodes 1,...,K.
C
C   The algorithm consists of the following steps:  node K
C is located relative to the triangulation (CSTRFIND), its
C index is added to the data structure (CSINTADD or CSBDYADD),
C and a sequence of swaps (CSSWPTST and CSSWAP) are applied to
C the arcs opposite K so that all arcs incident on node K
C and opposite node K are locally optimal (satisfy the cir-
C cumcircle test).  Thus, if a Delaunay triangulation is
C input, a Delaunay triangulation will result.
C
C
C On input:
C
C       NST = Index of a node at which CSTRFIND begins its
C             search.  Search time depends on the proximity
C             of this node to K.  If NST < 1, the search is
C             begun at node K-1.
C
C       K = Nodal index (index for X, Y, Z, and LEND) of the
C           new node to be added.  K .GE. 4.
C
C       X,Y,Z = Arrays of length .GE. K containing Car-
C               tesian coordinates of the nodes.
C               (X(I),Y(I),Z(I)) defines node I for
C               I = 1,...,K.
C
C The above parameters are not altered by this routine.
C
C       LIST,LPTR,LEND,LNEW = Data structure associated with
C                             the triangulation of nodes 1
C                             to K-1.  The array lengths are
C                             assumed to be large enough to
C                             add node K.  Refer to Subrou-
C                             tine CSTRMESH.
C
C On output:
C
C       LIST,LPTR,LEND,LNEW = Data structure updated with
C                             the addition of node K as the
C                             last entry unless IER .NE. 0
C                             and IER .NE. -3, in which case
C                             the arrays are not altered.
C
C       IER = Error indicator:
C             IER =  0 if no errors were encountered.
C             IER = -1 if K is outside its valid range
C                      on input.
C             IER = -2 if all nodes (including K) are col-
C                      linear (lie on a common geodesic).
C             IER =  L if nodes L and K coincide for some
C                      L < K.
C
C Modules required by CSADDNOD:  CSBDYADD, CSCOVSPH, CSINSERT,
C                                CSINTADD, CSJRAND, CSLSTPTR,
C                                CSSTORE, CSSWAP, CSSWPTST,
C                                CSTRFIND
C
C Intrinsic function called by CSADDNOD:  ABS
C
C***********************************************************
C
      INTEGER CSLSTPTR
      INTEGER I1, I2, I3, IO1, IO2, IN1, IST, KK, KM1, L,
     .        LP, LPF, LPO1, LPO1S
      LOGICAL CSSWPTST
      DOUBLE PRECISION B1, B2, B3, P(3)
C
C Local parameters:
C
C B1,B2,B3 = Unnormalized barycentric coordinates returned
C              by CSTRFIND.
C I1,I2,I3 = Vertex indexes of a triangle containing K
C IN1 =      Vertex opposite K:  first neighbor of IO2
C              that precedes IO1.  IN1,IO1,IO2 are in
C              counterclockwise order.
C IO1,IO2 =  Adjacent neighbors of K defining an arc to
C              be tested for a swap
C IST =      Index of node at which CSTRFIND begins its search
C KK =       Local copy of K
C KM1 =      K-1
C L =        Vertex index (I1, I2, or I3) returned in IER
C              if node K coincides with a vertex
C LP =       LIST pointer
C LPF =      LIST pointer to the first neighbor of K
C LPO1 =     LIST pointer to IO1
C LPO1S =    Saved value of LPO1
C P =        Cartesian coordinates of node K
C
      KK = K
      IF (KK .LT. 4) GO TO 3
C
C Initialization:
C
      KM1 = KK - 1
      IST = NST
      IF (IST .LT. 1) IST = KM1
      P(1) = X(KK)
      P(2) = Y(KK)
      P(3) = Z(KK)
C
C Find a triangle (I1,I2,I3) containing K or the rightmost
C   (I1) and leftmost (I2) visible boundary nodes as viewed
C   from node K.
C
      CALL CSTRFIND (IST,P,KM1,X,Y,Z,LIST,LPTR,LEND, B1,B2,B3,
     .             I1,I2,I3)
C
C   Test for collinear or duplicate nodes.
C
      IF (I1 .EQ. 0) GO TO 4
      IF (I3 .NE. 0) THEN
        L = I1
        IF (P(1) .EQ. X(L)  .AND.  P(2) .EQ. Y(L)  .AND.
     .      P(3) .EQ. Z(L)) GO TO 5
        L = I2
        IF (P(1) .EQ. X(L)  .AND.  P(2) .EQ. Y(L)  .AND.
     .      P(3) .EQ. Z(L)) GO TO 5
        L = I3
        IF (P(1) .EQ. X(L)  .AND.  P(2) .EQ. Y(L)  .AND.
     .      P(3) .EQ. Z(L)) GO TO 5
        CALL CSINTADD (KK,I1,I2,I3, LIST,LPTR,LEND,LNEW )
      ELSE
        IF (I1 .NE. I2) THEN
          CALL CSBDYADD (KK,I1,I2, LIST,LPTR,LEND,LNEW )
        ELSE
          CALL CSCOVSPH (KK,I1, LIST,LPTR,LEND,LNEW )
        ENDIF
      ENDIF
      IER = 0
C
C Initialize variables for optimization of the
C   triangulation.
C
      LP = LEND(KK)
      LPF = LPTR(LP)
      IO2 = LIST(LPF)
      LPO1 = LPTR(LPF)
      IO1 = ABS(LIST(LPO1))
C
C Begin loop:  find the node opposite K.
C
    1 LP = CSLSTPTR(LEND(IO1),IO2,LIST,LPTR)
        IF (LIST(LP) .LT. 0) GO TO 2
        LP = LPTR(LP)
        IN1 = ABS(LIST(LP))
C
C Swap test:  if a swap occurs, two new arcs are
C             opposite K and must be tested.
C
        LPO1S = LPO1
        IF ( .NOT. CSSWPTST(IN1,KK,IO1,IO2,X,Y,Z) ) GO TO 2
        CALL CSSWAP (IN1,KK,IO1,IO2, LIST,LPTR,LEND, LPO1)
        IF (LPO1 .EQ. 0) THEN
C
C   A swap is not possible because KK and IN1 are already
C     adjacent.  This error in CSSWPTST only occurs in the
C     neutral case and when there are nearly duplicate
C     nodes.
C
          LPO1 = LPO1S
          GO TO 2
        ENDIF
        IO1 = IN1
        GO TO 1
C
C No swap occurred.  Test for termination and reset
C   IO2 and IO1.
C
    2   IF (LPO1 .EQ. LPF  .OR.  LIST(LPO1) .LT. 0) RETURN
        IO2 = IO1
        LPO1 = LPTR(LPO1)
        IO1 = ABS(LIST(LPO1))
        GO TO 1
C
C KK < 4.
C
    3 IER = -1
      RETURN
C
C All nodes are collinear.
C
    4 IER = -2
      RETURN
C
C Nodes L and K coincide.
C
    5 IER = L
      RETURN
      END
