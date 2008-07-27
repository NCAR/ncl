C
C       $Id: sh3val.f,v 1.5 2008-07-27 03:10:13 haley Exp $
C                                                                      
C                Copyright (C)  2000
C        University Corporation for Atmospheric Research
C                All Rights Reserved
C
C The use of this Software is governed by a License Agreement.
C
      FUNCTION SH3VAL (PX,PY,PZ,N,X,Y,Z,F,NR,LCELL,LNEXT,
     +                 XYZMIN,XYZDEL,RMAX,RSQ,A)
      INTEGER N, NR, LCELL(NR,NR,NR), LNEXT(N)
      REAL    PX, PY, PZ, X(N), Y(N), Z(N), F(N), XYZMIN(3),
     +        XYZDEL(3), RMAX, RSQ(N), A(9,N)
C
C***********************************************************
C
C                                               ROBERT RENKA
C                                       UNIV. OF NORTH TEXAS
C                                             (817) 565-2767
C                                                   10/28/87
C
C  This function returns the value Q(PX,PY,PZ) where Q is
C  the weighted sum of quadratic nodal functions defined in
C  subroutine SHQSHEP.  SH3GRD may be called to compute a
C  gradient of Q along with the value, and/or to test for
C  errors.
C
C  ON INPUT --
C
C    PX,PY,PZ = Cartesian coordinates of the point P at
C               which Q is to be evaluated.
C
C    N = Number of nodes and data values defining Q.
C        N .GE. 10.
C
C    X,Y,Z,F = Arrays of length N containing the nodes
C              and data values interpolated by Q.
C
C    NR = Number of rows, columns, and planes in the cell
C         grid.  Refer to SHSTORE3.  NR .GE. 1.
C
C    LCELL = NR by NR by NR array of nodal indices associated
C            with cells.  Refer to SHSTORE3.
C
C    LNEXT = Array of length N containing next-node indices
C            Refer to SHSTORE3.
C
C    XYZMIN,XYZDEL = Arrays of length 3 containing minimum
C                    nodal coordinates and cell dimensions
C                    respectively.  XYZDEL elements must be 
C                    positive.  Refer to SHSTORE3.
C
C    RMAX = Square root of the largest element in RSQ --
C           maximum radius.
C
C    RSQ = Array of length N containing the squared radii
C          which enter into the weights defining Q.
C
C    A = 9 by N array containing the coefficients for the
C        nodal functions defining Q.
C
C  Input parameters are not altered by this function.  The
C  parameters other than PX, PY and PZ should be input unaltered
C  from their values on output from SHQSHEP.  This function
C  should not be called if a nonzero error flag was returned by 
C  SHQSHEP.
C
C  ON OUTPUT --
C
C    SH3VAL = function value Q(PX,PY,PZ) unless N, NR,
C             XYZDEL, or RMAX is invalid, in which case
C             no value is returned.
C
C  MODULES REQUIRED BY SH3VAL -- NONE
C
C  INTRINSIC FUNCTIONS CALLED BY SH3VAL -- IFIX, SQRT
C
C***********************************************************
C
      XP = PX
      YP = PY
      ZP = PZ
      XMIN = XYZMIN(1)
      YMIN = XYZMIN(2)
      ZMIN = XYZMIN(3)
      DX = XYZDEL(1)
      DY = XYZDEL(2)
      DZ = XYZDEL(3)
      IF (N .LT. 10) THEN
        GO TO 30
      ELSEIF (NR .LT. 1) THEN
        GO TO 31
      ELSEIF (DX .LE. 0.) THEN
        GO TO 32
      ELSEIF (DY .LE. 0.) THEN
        GO TO 33
      ELSEIF (DZ .LE. 0.) THEN
        GO TO 34
      ELSEIF (RMAX .LT. 0.) THEN
        GO TO 35
      ENDIF
C
C  Set IMIN, IMAX, JMIN, JMAX, KMIN, and KMAX to cell indices
C  defining the range of the search for nodes whose radii
C  include P.  The cells which must be searched are those
C  intersected by (or contained in) a sphere of radius RMAX
C  centered at P.
C
      IMIN = IFIX((XP-XMIN-RMAX)/DX) + 1
      IMAX = IFIX((XP-XMIN+RMAX)/DX) + 1
      IF (IMIN .LT. 1) IMIN = 1
      IF (IMAX .GT. NR) IMAX = NR
      JMIN = IFIX((YP-YMIN-RMAX)/DY) + 1
      JMAX = IFIX((YP-YMIN+RMAX)/DY) + 1
      IF (JMIN .LT. 1) JMIN = 1
      IF (JMAX .GT. NR) JMAX = NR
      KMIN = IFIX((ZP-ZMIN-RMAX)/DZ) + 1
      KMAX = IFIX((ZP-ZMIN+RMAX)/DZ) + 1
      IF (KMIN .LT. 1) KMIN = 1
      IF (KMAX .GT. NR) KMAX = NR
C
C  The following is a test for no cells within the sphere
C  of radius RMAX.
C
      IF (IMIN .GT. IMAX  .OR.  JMIN .GT. JMAX  .OR.
     .    KMIN .GT. KMAX) GO TO 5
C
C  Accumulate weight values in SW and weighted nodal function
C  values in SWQ.  The weights are W(L) = ((R-D)+/(R*D))**2
C  for R**2 = RSQ(L) and D = distance between P and node L.
C
      SW = 0.
      SWQ = 0.
C
C  Outer loop on cells (I,J,K).
C
      DO 3 K = KMIN,KMAX
        DO 3 J = JMIN,JMAX
          DO 3 I = IMIN,IMAX
            L = LCELL(I,J,K)
            IF (L .EQ. 0) GO TO 3
C
C  Inner loop on nodes L.
C
    1       DELX = XP - X(L)
            DELY = YP - Y(L)
            DELZ = ZP - Z(L)
            DXSQ = DELX*DELX
            DYSQ = DELY*DELY
            DZSQ = DELZ*DELZ
            DS = DXSQ + DYSQ + DZSQ
            RS = RSQ(L)
            IF (DS .GE. RS) GO TO 2
            IF (DS .EQ. 0.) GO TO 4
            RDS = RS*DS
            RD = SQRT(RDS)
            W = (RS+DS-RD-RD)/RDS
            SW = SW + W
            SWQ = SWQ + W*( A(1,L)*DXSQ + A(2,L)*DELX*DELY +
     +                      A(3,L)*DYSQ + A(4,L)*DELX*DELZ +
     +                      A(5,L)*DELY*DELZ + A(6,L)*DZSQ +
     +                      A(7,L)*DELX + A(8,L)*DELY +
     +                      A(9,L)*DELZ + F(L) )
C
C  Bottom of loop on nodes in cell (I,J,K).
C
    2       LP = L
            L = LNEXT(LP)
            IF (L .NE. LP) GO TO 1
    3       CONTINUE
C
C  SW = 0 iff P is not within the radius R(L) for any node L.
C
      IF (SW .EQ. 0.) GO TO 5
      SH3VAL = SWQ/SW
      RETURN
C
C  (PX,PY,PZ) = (X(L),Y(L),Z(L))
C
    4 SH3VAL = F(L)
      RETURN
C
C  All weights are 0 at P.
C
    5 SH3VAL = 0.
      RETURN
C
C  Invalid input parameter.
C
   30 CONTINUE
      CALL SHERR (1,'SH3VAL - number of input data must be > 9',41)
      SH3VAL = 0.
      RETURN
   31 CONTINUE
      CALL SHERR (7,'SH3VAL - cell grid dimensions must be positive',
     +            46)
      SH3VAL = 0.
      RETURN
   32 CONTINUE
      CALL SHERR (10,'SH3VAL - at least two points must have different X
     + coordinates',62)
      SH3VAL = 0.
      RETURN
   33 CONTINUE
      CALL SHERR (11,'SH3VAL - at least two points must have different Y
     + coordinates',62)
      SH3VAL = 0.
      RETURN
   34 CONTINUE
      CALL SHERR (12,'SH3VAL - at least two points must have different Z
     + coordinates',62)
      SH3VAL = 0.
      RETURN
   35 CONTINUE
      CALL SHERR (14,
     +'SH3GRD - negative search radius in calculating least squares fit'
     +           ,64)
      SH3VAL = 0.
      RETURN
      END
