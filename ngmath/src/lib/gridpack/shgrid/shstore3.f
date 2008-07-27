C
C       $Id: shstore3.f,v 1.6 2008-07-27 03:10:14 haley Exp $
C                                                                      
C                Copyright (C)  2000
C        University Corporation for Atmospheric Research
C                All Rights Reserved
C
C The use of this Software is governed by a License Agreement.
C
      SUBROUTINE SHSTORE3 (N,X,Y,Z,NR, LCELL,LNEXT,XYZMIN,
     +                    XYZDEL,IER)
      INTEGER N, NR, LCELL(NR,NR,NR), LNEXT(N), IER
      REAL    X(N), Y(N), Z(N), XYZMIN(3), XYZDEL(3)
C
C***********************************************************
C
C                                               ROBERT RENKA
C                                       UNIV. OF NORTH TEXAS
C                                             (817) 565-2767
C
C  Given a set of N arbitrarily distributed nodes in three-
C  space, this subroutine creates a data structure for a
C  cell-based method of solving closest-point problems.  The
C  smallest box containing the nodes is partitioned into an
C  NR by NR by NR uniform grid of cells, and nodes are as-
C  sociated with cells.  In particular, the data structure
C  stores the indices of the nodes contained in each cell.
C  For a uniform random distribution of nodes, the nearest
C  node to an arbitrary point can be determined in constant
C  expected time.
C
C  ON INPUT --
C
C    N = number of nodes.  N .GE. 2.
C
C    X,Y,Z = arrays of length N containing the Cartesian
C            coordinates of the nodes.
C
C    NR = number of rows, columns, and planes in the
C         grid.  The cell density (average number of
C         nodes per cell) is D = N/(NR**3).  A recommended
C         value, based on empirical evidence, is D = 3
C         -- NR = (N/3)**(1/3).  NR .GE. 1.
C
C  The above parameters are not altered by this routine.
C
C    LCELL = array of length .GE. NR**3.
C
C    LNEXT = array of length .GE. N.
C
C    XYZMIN,XYZDEL = arrays of length .GE. 3.
C
C  ON OUTPUT --
C
C    LCELL = NR by NR by NR cell array such that
C            LCELL(I,J,K) contains the index (for X, Y,
C            and Z) of the first node (node with smallest
C            index) in cell (I,J,K), or LCELL(I,J,K) = 0
C            If no nodes are contained in the cell.  The
C            corner of cell (I,J,K) which is farthest
C            from the box corner defined by XYZMIN has
C            coordinates (XMIN+I*DX,YMIN+J*DY,ZMIN+K*DZ),
C            where (XMIN,YMIN,ZMIN) are the elements of
C            XYZMIN.  LCELL is not defined if IER .NE. 0.
C
C    LNEXT = Array of next-node indices such that
C            LNEXT(L) contains the index of the next node
C            in the cell which contains node L, or
C            LNEXT(L) = L if L is the last node in the
C            cell for L = 1,...,N.  (The nodes contained
C            in a cell are ordered by their indices.)
C            If, for example, cell (I,J,K) contains nodes
C            2, 3, and 5 (and no others), then
C            LCELL(I,J,K) = 2, LNEXT(2) = 3, LNEXT(3) =
C            5, and LNEXT(5) = 5.  LNEXT is not defined
C            if IER .NE. 0.
C
C    XYZMIN = Array of length 3 containing the minimum
C             nodal coordinates XMIN, YMIN, and ZMIN (in
C             that order) unless IER = 1.  The opposite
C             corner of the box defined by the nodes is
C             (XMIN+NR*DX,YMIN+NR*DY,ZMIN+NR*DZ).
C
C    XYZDEL = Array of length 3 containing the dimensions
C             of the cells unless IER = 1.  XYZDEL(1) =
C             (XMAX-XMIN)/NR, XYZDEL(2) = (YMAX-YMIN)/NR,
C             AND XYZDEL(3) = (ZMAX-ZMIN)/NR, where XMIN,
C             XMAX, YMIN, YMAX, ZMIN, and ZMAX are the
C             extrema of X, Y, and Z.
C
C    IER = Error indicator --
C          IER = 0 if no errors were encountered.
C          IER = 1 if N .LT. 2 or NR .LT. 1.
C          IER = 2 if a component of XYZDEL is not positive.
C
C  MODULES REQUIRED BY SHSTORE3 -- NONE
C
C  INTRINSIC FUNCTIONS CALLED BY SHSTORE3 -- FLOAT, IFIX
C
C***********************************************************
C
      NN = N
      NNR = NR
      IF (NN .LT. 2  .OR.  NNR .LT. 1) GO TO 4
C
C  Compute the dimensions of the rectangle containing the nodes.
C
      XMN = X(1)
      XMX = XMN
      YMN = Y(1)
      YMX = YMN
      ZMN = Z(1)
      ZMX = ZMN
      DO 1 L = 2,NN
        IF (X(L) .LT. XMN) XMN = X(L)
        IF (X(L) .GT. XMX) XMX = X(L)
        IF (Y(L) .LT. YMN) YMN = Y(L)
        IF (Y(L) .GT. YMX) YMX = Y(L)
        IF (Z(L) .LT. ZMN) ZMN = Z(L)
        IF (Z(L) .GT. ZMX) ZMX = Z(L)
    1 CONTINUE
      XYZMIN(1) = XMN
      XYZMIN(2) = YMN
      XYZMIN(3) = ZMN
C
C  Compute cell dimensions and test for zero area.
C
      DELX = (XMX-XMN)/FLOAT(NNR)
      DELY = (YMX-YMN)/FLOAT(NNR)
      DELZ = (ZMX-ZMN)/FLOAT(NNR)
      XYZDEL(1) = DELX
      XYZDEL(2) = DELY
      XYZDEL(3) = DELZ
      IF (DELX .EQ. 0.  .OR.  DELY .EQ. 0.  .OR.
     +    DELZ .EQ. 0.) GO TO 5
C
C INITIALIZE LCELL.
C
      DO 7 K = 1,NNR
        DO 6 J = 1,NNR
          DO 2 I = 1,NNR
            LCELL(I,J,K) = 0
    2     CONTINUE
    6   CONTINUE
    7 CONTINUE
C
C  Loop on nodes, storing indices in LCELL and LNEXT.
C
      NP1 = NN + 1
      DO 3 LL = 1,NN
        LB = NP1 - LL
        I = IFIX((X(LB)-XMN)/DELX) + 1
        IF (I .GT. NNR) I = NNR
        J = IFIX((Y(LB)-YMN)/DELY) + 1
        IF (J .GT. NNR) J = NNR
        K = IFIX((Z(LB)-ZMN)/DELZ) + 1
        IF (K .GT. NNR) K = NNR
        L = LCELL(I,J,K)
        LNEXT(LB) = L
        IF (L .EQ. 0) LNEXT(LB) = LB
        LCELL(I,J,K) = LB
    3 CONTINUE
C
C  No errors encountered.
C
      IER = 0
      RETURN
C
C  Invalid input parameter.
C
    4 CONTINUE
      CALL SHERR (1,'SHSTORE3 - number of input data must be > 9',41)
      IER = 1
      RETURN
C
C  Nonpositive XYZDEL component.
C
    5 CONTINUE
      CALL SHERR (7,
     +            'SHSTORE3 - cell grid dimensions must be positive',
     +             48)
      IER = 7
      RETURN
      END
