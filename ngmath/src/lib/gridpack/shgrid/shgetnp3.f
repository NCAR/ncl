C
C       $Id: shgetnp3.f,v 1.4 2008-07-27 03:10:13 haley Exp $
C                                                                      
C                Copyright (C)  2000
C        University Corporation for Atmospheric Research
C                All Rights Reserved
C
C The use of this Software is governed by a License Agreement.
C
      SUBROUTINE SHGETNP3 (PX,PY,PZ,X,Y,Z,NR,LCELL,LNEXT,
     +                    XYZMIN,XYZDEL, NP,DSQ)
      INTEGER NR, LCELL(NR,NR,NR), LNEXT(1), NP
      REAL    PX, PY, PZ, X(1), Y(1), Z(1), XYZMIN(3),
     +        XYZDEL(3), DSQ
C
C***********************************************************
C
C                                               ROBERT RENKA
C                                       Univ. of North Texas
C                                             (817) 565-2767
C
C  Given a set of N nodes and the data structure defined in
C  subroutine SHSTORE3, this subroutine uses the cell method to
C  find the closest unmarked node NP to a specified point P.
C  NP is then marked by setting LNEXT(NP) to -LNEXT(NP).  (A
C  node is marked if and only if the corresponding LNEXT element
C  is negative.  The absolute values of LNEXT elements,
C  however, must be preserved.)  Thus, the closest M nodes to
C  P may be determined by a sequence of M calls to this routine
C  Note that if the nearest neighbor to node K is to be
C  determined (PX = X(K), PY = Y(K), and PZ = Z(K)), then
C  K should be marked before the call to this routine.
C
C  The search is begun in the cell containing (or closest
C  to) P and proceeds outward in box-shaped layers until all
C  cells which contain points within distance R of P have
C  been searched, where R is the distance from P to the first
C  unmarked node encountered (infinite if no unmarked nodes
C  are present).
C
C  On input --
C
C    PX,PY,PZ = Cartesian coordinates of the point P whose
C               nearest unmarked neighbor is to be found.
C
C    X,Y,Z = Arrays of length N, for N .GE. 2, containing
C            the Cartesian coordinates of the nodes.
C
C    NR = Number of rows, columns, and planes in the cell
C         grid.  NR .GE. 1.
C
C    LCELL = NR by NR by NR array of nodal indices associated
C            with cells.
C
C    LNEXT = Array of length N containing next-node indices
C            (or their negatives).
C
C    XYZMIN,XYZDEL = Arrays of length 3 containing minimum
C                    nodal coordinates and cell dimensions,
C                    respectively.  XYZDEL elements must 
C                    be positive.
C
C  Input parameters other than LNEXT are not altered by
C  this routine.  With the exception of (PX,PY,PZ) and the
C  signs of LNEXT elements, these parameters should be
C  unaltered from their values on output from subroutine SHSTORE3.
C
C  On output --
C
C    NP = index (for X, Y, and Z) of the nearest unmarked
C         node to P, or 0 if all nodes are marked or NR
C         .LT. 1 or an element of XYZDEL is not positive.
C         LNEXT(NP) .LT. 0 if NP .NE. 0.
C
C    DSQ = Squared Euclidean distance between P and node
C          NP, or 0 if NP = 0.
C
C  Modules required by SHGETNP3 -- none
C
C  Intrinsic functions called by SHGETNP3 -- IABS, IFIX, SQRT
C
C***********************************************************
C
      LOGICAL FIRST
      XP = PX
      YP = PY
      ZP = PZ
      DX = XYZDEL(1)
      DY = XYZDEL(2)
      DZ = XYZDEL(3)
C
C  Test for invalid input parameters.
C
      IF (NR .LT. 1  .OR.  DX .LE. 0.  .OR.  DY .LE. 0.
     +    .OR.  DZ .LE. 0.) GO TO 10
C
C Initialize parameters --
C
C   FIRST = TRUE iff the first unmarked node has yet to be
C           encountered,
C   IMIN,...,KMAX = cell indices defining the range of the
C                   search,
C   DELX,DELY,DELZ = PX-XYZMIN(1), PY-XYZMIN(2), and
C                    PZ-XYZMIN(3),
C   I0,J0,K0 = cell containing or closest to P,
C   I1,...,K2 = cell indices of the layer whose intersection
C               with the range defined by IMIN,...,KMAX is
C               currently being searched.
C
      FIRST = .TRUE.
      IMIN = 1
      IMAX = NR
      JMIN = 1
      JMAX = NR
      KMIN = 1
      KMAX = NR
      DELX = XP - XYZMIN(1)
      DELY = YP - XYZMIN(2)
      DELZ = ZP - XYZMIN(3)
      I0 = IFIX(DELX/DX) + 1
      IF (I0 .LT. 1) I0 = 1
      IF (I0 .GT. NR) I0 = NR
      J0 = IFIX(DELY/DY) + 1
      IF (J0 .LT. 1) J0 = 1
      IF (J0 .GT. NR) J0 = NR
      K0 = IFIX(DELZ/DZ) + 1
      IF (K0 .LT. 1) K0 = 1
      IF (K0 .GT. NR) K0 = NR
      I1 = I0
      I2 = I0
      J1 = J0
      J2 = J0
      K1 = K0
      K2 = K0
C
C  Outer loop on layers, inner loop on layer cells, excluding
C  those outside the range (IMIN,IMAX) x (JMIN,JMAX) x (KMIN,KMAX).
C
    1 DO 7 K = K1,K2
        IF (K .GT. KMAX) GO TO 8
        IF (K .LT. KMIN) GO TO 7
        DO 6 J = J1,J2
          IF (J .GT. JMAX) GO TO 7
          IF (J .LT. JMIN) GO TO 6
          DO 5 I = I1,I2
            IF (I .GT. IMAX) GO TO 6
            IF (I .LT. IMIN) GO TO 5
            IF (K .NE. K1  .AND.  K .NE. K2  .AND.
     +          J .NE. J1  .AND.  J .NE. J2  .AND.
     +          I .NE. I1  .AND.  I .NE. I2) GO TO 5
C
C  Search cell (I,J,K) for unmarked nodes L.
C
            L = LCELL(I,J,K)
            IF (L .EQ. 0) GO TO 5
C
C  Loop on nodes in cell (I,J,K).
C
    2       LN = LNEXT(L)
            IF (LN .LT. 0) GO TO 4
C
C  Node L is not marked.
C
            RSQ = (X(L)-XP)**2 + (Y(L)-YP)**2 + (Z(L)-ZP)**2
            IF (.NOT. FIRST) GO TO 3
C
C  Node L is the first unmarked neighbor of P encountered.
C  Initialize LMIN to the current candidate for NP, and
C  RSMIN to the squared distance from P to LMIN.  IMIN,
C  IMAX, JMIN, JMAX, KMIN, and KMAX are updated to define
C  the smallest rectangle containing a sphere of radius
C  R = SQRT(RSMIN) centered at P, and contained in (1,NR)
C  x (1,NR) x (1,NR) (except that, if P is outside the
C  box defined by the nodes, it is possible that IMIN
C  .GT. NR or IMAX .LT. 1, etc.).  FIRST is reset to FALSE.
C
            LMIN = L
            RSMIN = RSQ
            R = SQRT(RSMIN)
            IMIN = IFIX((DELX-R)/DX) + 1
            IF (IMIN .LT. 1) IMIN = 1
            IMAX = IFIX((DELX+R)/DX) + 1
            IF (IMAX .GT. NR) IMAX = NR
            JMIN = IFIX((DELY-R)/DY) + 1
            IF (JMIN .LT. 1) JMIN = 1
            JMAX = IFIX((DELY+R)/DY) + 1
            IF (JMAX .GT. NR) JMAX = NR
            KMIN = IFIX((DELZ-R)/DZ) + 1
            IF (KMIN .LT. 1) KMIN = 1
            KMAX = IFIX((DELZ+R)/DZ) + 1
            IF (KMAX .GT. NR) KMAX = NR
            FIRST = .FALSE.
            GO TO 4
C
C  Test for node L closer than LMIN to P.
C
    3       IF (RSQ .GE. RSMIN) GO TO 4
C
C  Update LMIN and RSMIN.
C
            LMIN = L
            RSMIN = RSQ
C
C  Test for termination of loop on nodes in cell (I,J,K).
C
    4       IF (IABS(LN) .EQ. L) GO TO 5
            L = IABS(LN)
            GO TO 2
    5       CONTINUE
    6     CONTINUE
    7   CONTINUE
C
C  Test for termination of loop on cell layers.
C
    8 IF (I1 .LE. IMIN  .AND.  I2 .GE. IMAX  .AND.
     .    J1 .LE. JMIN  .AND.  J2 .GE. JMAX  .AND.
     .    K1 .LE. KMIN  .AND.  K2 .GE. KMAX) GO TO 9
      I1 = I1 - 1
      I2 = I2 + 1
      J1 = J1 - 1
      J2 = J2 + 1
      K1 = K1 - 1
      K2 = K2 + 1
      GO TO 1
C
C  Unless no unmarked nodes were encountered, LMIN is the
C  closest unmarked node to P.
C
    9 IF (FIRST) GO TO 10
      NP = LMIN
      DSQ = RSMIN
      LNEXT(LMIN) = -LNEXT(LMIN)
      RETURN
C
C  Error -- NR or XYZDEL is invalid or all nodes are marked.
C
   10 NP = 0
      DSQ = 0.
C
      RETURN
      END
