C
C	$Id: csdelnod.f,v 1.5 2008-07-27 03:10:06 haley Exp $
C                                                                      
C                Copyright (C)  2000
C        University Corporation for Atmospheric Research
C                All Rights Reserved
C
C The use of this Software is governed by a License Agreement.
C
      SUBROUTINE CSDELNOD (K, N,X,Y,Z,LIST,LPTR,LEND,LNEW,LWK,
     .                   IWK, IER)
      INTEGER K, N, LIST(*), LPTR(*), LEND(*), LNEW, LWK,
     .        IWK(2,*), IER
      DOUBLE PRECISION X(*), Y(*), Z(*)
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
C   This subroutine deletes node K (along with all arcs
C incident on node K) from a triangulation of N nodes on the
C unit sphere, and inserts arcs as necessary to produce a
C triangulation of the remaining N-1 nodes.  If a Delaunay
C triangulation is input, a Delaunay triangulation will
C result, and thus, CSDELNOD reverses the effect of a call to
C Subroutine CSADDNOD.
C
C
C On input:
C
C       K = Index (for X, Y, and Z) of the node to be
C           deleted.  1 .LE. K .LE. N.
C
C K is not altered by this routine.
C
C       N = Number of nodes in the triangulation on input.
C           N .GE. 4.  Note that N will be decremented
C           following the deletion.
C
C       X,Y,Z = Arrays of length N containing the Cartesian
C               coordinates of the nodes in the triangula-
C               tion.
C
C       LIST,LPTR,LEND,LNEW = Data structure defining the
C                             triangulation.  Refer to Sub-
C                             routine CSTRMESH.
C
C       LWK = Number of columns reserved for IWK.  LWK must
C             be at least NNB-3, where NNB is the number of
C             neighbors of node K, including an extra
C             pseudo-node if K is a boundary node.
C
C       IWK = Integer work array dimensioned 2 by LWK (or
C             array of length .GE. 2*LWK).
C
C On output:
C
C       N = Number of nodes in the triangulation on output.
C           The input value is decremented unless 1 .LE. IER
C           .LE. 4.
C
C       X,Y,Z = Updated arrays containing nodal coordinates
C               (with elements K+1,...,N+1 shifted up one
C               position, thus overwriting element K) unless
C               1 .LE. IER .LE. 4.
C
C       LIST,LPTR,LEND,LNEW = Updated triangulation data
C                             structure reflecting the dele-
C                             tion unless 1 .LE. IER .LE. 4.
C                             Note that the data structure
C                             may have been altered if IER >
C                             3.
C
C       LWK = Number of IWK columns required unless IER = 1
C             or IER = 3.
C
C       IWK = Indexes of the endpoints of the new arcs added
C             unless LWK = 0 or 1 .LE. IER .LE. 4.  (Arcs
C             are associated with columns, or pairs of
C             adjacent elements if IWK is declared as a
C             singly-subscripted array.)
C
C       IER = Error indicator:
C             IER = 0 if no errors were encountered.
C             IER = 1 if K or N is outside its valid range
C                     or LWK < 0 on input.
C             IER = 2 if more space is required in IWK.
C                     Refer to LWK.
C             IER = 3 if the triangulation data structure is
C                     invalid on input.
C             IER = 4 if K indexes an interior node with
C                     four or more neighbors, none of which
C                     can be swapped out due to collineari-
C                     ty, and K cannot therefore be deleted.
C             IER = 5 if an error flag (other than IER = 1)
C                     was returned by CSOPTIM.  An error
C                     message is written to the standard
C                     output unit in this case.
C             IER = 6 if error flag 1 was returned by CSOPTIM.
C                     This is not necessarily an error, but
C                     the arcs may not be optimal.
C
C   Note that the deletion may result in all remaining nodes
C being collinear.  This situation is not flagged.
C
C Modules required by CSDELNOD:  CSDELNB, CSLEFT, CSLSTPTR, CSNBCNT,
C                                CSOPTIM, CSSWAP, CSSWPTST
C
C Intrinsic function called by CSDELNOD:  ABS
C
C***********************************************************
C
      INTEGER CSLSTPTR, CSNBCNT
      INTEGER I, IERR, IWL, J, LNW, LP, LP21, LPF, LPH, LPL,
     .        LPL2, LPN, LWKL, N1, N2, NFRST, NIT, NL, NN,
     .        NNB, NR
      LOGICAL CSLEFT
      LOGICAL BDRY
      DOUBLE PRECISION X1, X2, XL, XR, Y1, Y2, YL, YR, Z1,
     .                 Z2, ZL, ZR
C
C Local parameters:
C
C BDRY =    Logical variable with value TRUE iff N1 is a
C             boundary node
C I,J =     DO-loop indexes
C IERR =    Error flag returned by CSOPTIM
C IWL =     Number of IWK columns containing arcs
C LNW =     Local copy of LNEW
C LP =      LIST pointer
C LP21 =    LIST pointer returned by CSSWAP
C LPF,LPL = Pointers to the first and last neighbors of N1
C LPH =     Pointer (or flag) returned by CSDELNB
C LPL2 =    Pointer to the last neighbor of N2
C LPN =     Pointer to a neighbor of N1
C LWKL =    Input value of LWK
C N1 =      Local copy of K
C N2 =      Neighbor of N1
C NFRST =   First neighbor of N1:  LIST(LPF)
C NIT =     Number of iterations in CSOPTIM
C NR,NL =   Neighbors of N1 preceding (to the right of) and
C             following (to the left of) N2, respectively
C NN =      Number of nodes in the triangulation
C NNB =     Number of neighbors of N1 (including a pseudo-
C             node representing the boundary if N1 is a
C             boundary node)
C X1,Y1,Z1 = Coordinates of N1
C X2,Y2,Z2 = Coordinates of N2
C XL,YL,ZL = Coordinates of NL
C XR,YR,ZR = Coordinates of NR
C
C
C Set N1 to K and NNB to the number of neighbors of N1 (plus
C   one if N1 is a boundary node), and test for errors.  LPF
C   and LPL are LIST indexes of the first and last neighbors
C   of N1, IWL is the number of IWK columns containing arcs,
C   and BDRY is TRUE iff N1 is a boundary node.
C
      N1 = K
      NN = N
      IF (N1 .LT. 1  .OR.  N1 .GT. NN  .OR.  NN .LT. 4  .OR.
     .    LWK .LT. 0) GO TO 21
      LPL = LEND(N1)
      LPF = LPTR(LPL)
      NNB = CSNBCNT(LPL,LPTR)
      BDRY = LIST(LPL) .LT. 0
      IF (BDRY) NNB = NNB + 1
      IF (NNB .LT. 3) GO TO 23
      LWKL = LWK
      LWK = NNB - 3
      IF (LWKL .LT. LWK) GO TO 22
      IWL = 0
      IF (NNB .EQ. 3) GO TO 3
C
C Initialize for loop on arcs N1-N2 for neighbors N2 of N1,
C   beginning with the second neighbor.  NR and NL are the
C   neighbors preceding and following N2, respectively, and
C   LP indexes NL.  The loop is exited when all possible
C   swaps have been applied to arcs incident on N1.
C
      X1 = X(N1)
      Y1 = Y(N1)
      Z1 = Z(N1)
      NFRST = LIST(LPF)
      NR = NFRST
      XR = X(NR)
      YR = Y(NR)
      ZR = Z(NR)
      LP = LPTR(LPF)
      N2 = LIST(LP)
      X2 = X(N2)
      Y2 = Y(N2)
      Z2 = Z(N2)
      LP = LPTR(LP)
C
C Top of loop:  set NL to the neighbor following N2.
C
    1 NL = ABS(LIST(LP))
      IF (NL .EQ. NFRST  .AND.  BDRY) GO TO 3
      XL = X(NL)
      YL = Y(NL)
      ZL = Z(NL)
C
C   Test for a convex quadrilateral.  To avoid an incorrect
C     test caused by collinearity, use the fact that if N1
C     is a boundary node, then N1 CSLEFT NR->NL and if N2 is
C     a boundary node, then N2 CSLEFT NL->NR.
C
      LPL2 = LEND(N2)
      IF ( .NOT. ((BDRY  .OR.  CSLEFT(XR,YR,ZR,XL,YL,ZL,X1,Y1,
     .      Z1))  .AND.  (LIST(LPL2) .LT. 0  .OR.
     .      CSLEFT(XL,YL,ZL,XR,YR,ZR,X2,Y2,Z2))) ) THEN
C
C   Nonconvex quadrilateral -- no swap is possible.
C
        NR = N2
        XR = X2
        YR = Y2
        ZR = Z2
        GO TO 2
      ENDIF
C
C   The quadrilateral defined by adjacent triangles
C     (N1,N2,NL) and (N2,N1,NR) is convex.  Swap in
C     NL-NR and store it in IWK unless NL and NR are
C     already adjacent, in which case the swap is not
C     possible.  Indexes larger than N1 must be decremented
C     since N1 will be deleted from X, Y, and Z.
C
      CALL CSSWAP (NL,NR,N1,N2, LIST,LPTR,LEND, LP21)
      IF (LP21 .EQ. 0) THEN
        NR = N2
        XR = X2
        YR = Y2
        ZR = Z2
        GO TO 2
      ENDIF
      IWL = IWL + 1
      IF (NL .LE. N1) THEN
        IWK(1,IWL) = NL
      ELSE
        IWK(1,IWL) = NL - 1
      ENDIF
      IF (NR .LE. N1) THEN
        IWK(2,IWL) = NR
      ELSE
        IWK(2,IWL) = NR - 1
      ENDIF
C
C   Recompute the LIST indexes and NFRST, and decrement NNB.
C
      LPL = LEND(N1)
      NNB = NNB - 1
      IF (NNB .EQ. 3) GO TO 3
      LPF = LPTR(LPL)
      NFRST = LIST(LPF)
      LP = CSLSTPTR(LPL,NL,LIST,LPTR)
      IF (NR .EQ. NFRST) GO TO 2
C
C   NR is not the first neighbor of N1.
C     Back up and test N1-NR for a swap again:  Set N2 to
C     NR and NR to the previous neighbor of N1 -- the
C     neighbor of NR which follows N1.  LP21 points to NL
C     as a neighbor of NR.
C
      N2 = NR
      X2 = XR
      Y2 = YR
      Z2 = ZR
      LP21 = LPTR(LP21)
      LP21 = LPTR(LP21)
      NR = ABS(LIST(LP21))
      XR = X(NR)
      YR = Y(NR)
      ZR = Z(NR)
      GO TO 1
C
C   Bottom of loop -- test for termination of loop.
C
    2 IF (N2 .EQ. NFRST) GO TO 3
      N2 = NL
      X2 = XL
      Y2 = YL
      Z2 = ZL
      LP = LPTR(LP)
      GO TO 1
C
C Delete N1 and all its incident arcs.  If N1 is an interior
C   node and either NNB > 3 or NNB = 3 and N2 CSLEFT NR->NL,
C   then N1 must be separated from its neighbors by a plane
C   containing the origin -- its removal reverses the effect
C   of a call to CSCOVSPH, and all its neighbors become
C   boundary nodes.  This is achieved by treating it as if
C   it were a boundary node (setting BDRY to TRUE, changing
C   a sign in LIST, and incrementing NNB).
C
    3 IF (.NOT. BDRY) THEN
        IF (NNB .GT. 3) THEN
          BDRY = .TRUE.
        ELSE
          LPF = LPTR(LPL)
          NR = LIST(LPF)
          LP = LPTR(LPF)
          N2 = LIST(LP)
          NL = LIST(LPL)
          BDRY = CSLEFT(X(NR),Y(NR),Z(NR),X(NL),Y(NL),Z(NL),
     .                X(N2),Y(N2),Z(N2))
        ENDIF
        IF (BDRY) THEN
C
C   IF a boundary node already exists, then N1 and its
C     neighbors cannot be converted to boundary nodes.
C     (They must be collinear.)  This is a problem if
C     NNB > 3.
C
          DO 4 I = 1,NN
            IF (LIST(LEND(I)) .LT. 0) THEN
              BDRY = .FALSE.
              GO TO 5
            ENDIF
    4       CONTINUE
          LIST(LPL) = -LIST(LPL)
          NNB = NNB + 1
        ENDIF
      ENDIF
    5 IF (.NOT. BDRY  .AND.  NNB .GT. 3) GO TO 24
C
C Initialize for loop on neighbors.  LPL points to the last
C   neighbor of N1.  LNEW is stored in local variable LNW.
C
      LP = LPL
      LNW = LNEW
C
C Loop on neighbors N2 of N1, beginning with the first.
C
    6 LP = LPTR(LP)
        N2 = ABS(LIST(LP))
        CALL CSDELNB (N2,N1,N, LIST,LPTR,LEND,LNW, LPH)
        IF (LPH .LT. 0) GO TO 23
C
C   LP and LPL may require alteration.
C
        IF (LPL .EQ. LNW) LPL = LPH
        IF (LP .EQ. LNW) LP = LPH
        IF (LP .NE. LPL) GO TO 6
C
C Delete N1 from X, Y, Z, and LEND, and remove its adjacency
C   list from LIST and LPTR.  LIST entries (nodal indexes)
C   which are larger than N1 must be decremented.
C
      NN = NN - 1
      IF (N1 .GT. NN) GO TO 9
      DO 7 I = N1,NN
        X(I) = X(I+1)
        Y(I) = Y(I+1)
        Z(I) = Z(I+1)
        LEND(I) = LEND(I+1)
    7   CONTINUE
C
      DO 8 I = 1,LNW-1
        IF (LIST(I) .GT. N1) LIST(I) = LIST(I) - 1
        IF (LIST(I) .LT. -N1) LIST(I) = LIST(I) + 1
    8   CONTINUE
C
C   For LPN = first to last neighbors of N1, delete the
C     preceding neighbor (indexed by LP).
C
C   Each empty LIST,LPTR location LP is filled in with the
C     values at LNW-1, and LNW is decremented.  All pointers
C     (including those in LPTR and LEND) with value LNW-1
C     must be changed to LP.
C
C  LPL points to the last neighbor of N1.
C
    9 IF (BDRY) NNB = NNB - 1
      LPN = LPL
      DO 13 J = 1,NNB
        LNW = LNW - 1
        LP = LPN
        LPN = LPTR(LP)
        LIST(LP) = LIST(LNW)
        LPTR(LP) = LPTR(LNW)
        IF (LPTR(LPN) .EQ. LNW) LPTR(LPN) = LP
        IF (LPN .EQ. LNW) LPN = LP
        DO 10 I = NN,1,-1
          IF (LEND(I) .EQ. LNW) THEN
            LEND(I) = LP
            GO TO 11
          ENDIF
   10     CONTINUE
C
   11   DO 12 I = LNW-1,1,-1
          IF (LPTR(I) .EQ. LNW) LPTR(I) = LP
   12     CONTINUE
   13   CONTINUE
C
C Update N and LNEW, and optimize the patch of triangles
C   containing K (on input) by applying swaps to the arcs
C   in IWK.
C
      N = NN
      LNEW = LNW
      IF (IWL .GT. 0) THEN
        NIT = 4*IWL
        CALL CSOPTIM (X,Y,Z,IWL, LIST,LPTR,LEND,NIT,IWK, IERR)
        IF (IERR .NE. 0  .AND.  IERR .NE. 1) GO TO 25
        IF (IERR .EQ. 1) GO TO 26
      ENDIF
C
C Successful termination.
C
      IER = 0
      RETURN
C
C Invalid input parameter.
C
   21 IER = 1
      RETURN
C
C Insufficient space reserved for IWK.
C
   22 IER = 2
      RETURN
C
C Invalid triangulation data structure.  NNB < 3 on input or
C   N2 is a neighbor of N1 but N1 is not a neighbor of N2.
C
   23 IER = 3
      RETURN
C
C N1 is interior but NNB could not be reduced to 3.
C
   24 IER = 4
      RETURN
C
C Error flag (other than 1) returned by CSOPTIM.
C
   25 IER = 5
      WRITE (*,100) NIT, IERR
  100 FORMAT (//5X,'*** Error in CSOPTIM (called from ',
     .        'CSDELNOD):  NIT = ',I4,', IER = ',I1,' ***'/)
      RETURN
C
C Error flag 1 returned by CSOPTIM.
C
   26 IER = 6
      RETURN
      END
