C
C       $Id: sh3grd.f,v 1.6 2008-07-27 03:10:13 haley Exp $
C                                                                      
C                Copyright (C)  2000
C        University Corporation for Atmospheric Research
C                All Rights Reserved
C
C The use of this Software is governed by a License Agreement.
C
      SUBROUTINE SH3GRD (PX,PY,PZ,N,X,Y,Z,F,NR,LCELL,LNEXT,
     +                   XYZMIN,XYZDEL,RMAX,RSQ,A, Q,QX,QY,
     +                   QZ,IER)
      INTEGER N, NR, LCELL(NR,NR,NR), LNEXT(N), IER
      REAL    PX, PY, PZ, X(N), Y(N), Z(N), F(N), XYZMIN(3),
     +        XYZDEL(3), RMAX, RSQ(N), A(9,N), Q, QX, QY, QZ
C
C***********************************************************
C
C                                               ROBERT RENKA
C                                       UNIV. OF NORTH TEXAS
C                                             (817) 565-2767
C                                                   10/28/87
C
C  This subroutine computes the value and gradient at
C  (PX,PY,PZ) of the interpolatory function Q defined in
C  subroutine SHQSHEP.  Q(X,Y,Z) is a weighted sum of quadratic
C  nodal functions.
C
C  ON INPUT --
C
C    PX,PY,PZ = Cartesian coordinates of the point P at
C               which Q and its partials are to be evaluated.
C
C    N = Number of nodes and data values defining Q.
C        N .GE. 10.
C
C    X,Y,Z,F = Arrays of length N containing the nodes
C              and data values interpolated by Q.
C
C    NR = Number of rows, columns and planes in the cell
C         grid.  Refer to SHSTORE3.  NR .GE. 1.
C
C    LCELL = NR by NR by NR array of nodal indices assoiated
C            with cells.  Refer to SHSTORE3.
C
C    LNEXT = Array of length N containing next-node indices
C            Refer to SHSTORE3.
C
C    XYZMIN,XYZDEL = Arrays of length 3 containing minimum
C                    nodal coordinates and cell dimensions,
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
C  Input parameters are not altered by this subroutine.
C  The parameters other than PX, PY, and PZ should be input
C  unaltered from their values on output from SHQSHEP.  This
C  subroutine should not be called if a nonzero error flag
C  was returned by SHQSHEP.
C
C  ON OUTPUT --
C
C    Q = value of Q at (PX,PY,PZ) unless IER .EQ. 1, in
C        which case no values are returned.
C
C    QX,QY,QZ = first partial derivatives of Q at
C               (PX,PY,PZ) unless IER .EQ. 1.
C
C    IER = error indicator
C          IER = 0 if no errors were encountered.
C          IER = 1 if N, NR, XYZDEL, or RMAX is invalid.
C          IER = 2 if no errors were encountered but
C                  (PX,PY,PZ) is not within the radius
C                  R(K) for any node K (and thus Q = QX =
C                  QY = QZ = 0).
C
C  MODULES REQUIRED BY SH3GRD -- NONE
C
C  INTRINSIC FUNCTIONS CALLED BY SH3GRD -- IFIX, SQRT
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
     +    KMIN .GT. KMAX) GO TO 6
C
C  Q = SWQ/SW = SUM(W(L)*Q(L))/SUM(W(L)) where the sum is
C  from L = 1 to N, Q(L) is the quadratic nodal function,
C  and W(L) = ((R-D)+/(R*D))**2 for radius R(L) and distance
C  D(L).  Thus
C
C        QX = (SWQX*SW - SWQ*SWX)/SW**2
C        QY = (SWQY*SW - SWQ*SWY)/SW**2
C        QZ = (SWQZ*SW - SWQ*SWZ)/SW**2
C
C  Where SWQX and SWX are partial derivatives with respect
C  to X of SWQ and SW, respectively.  SWQY, SWY, SWQZ, and
C  SWZ are defined similarly.
C
      SW = 0.
      SWX = 0.
      SWY = 0.
      SWZ = 0.
      SWQ = 0.
      SWQX = 0.
      SWQY = 0.
      SWQZ = 0.
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
            T = 2.*(RD-RS)/(DS*RDS)
            WX = DELX*T
            WY = DELY*T
            WZ = DELZ*T
            QLX = 2.*A(1,L)*DELX + A(2,L)*DELY + A(4,L)*DELZ
            QLY = A(2,L)*DELX + 2.*A(3,L)*DELY + A(5,L)*DELZ
            QLZ = A(4,L)*DELX + A(5,L)*DELY + 2.*A(6,L)*DELZ
            QL = (QLX*DELX + QLY*DELY + QLZ*DELZ)/2. +
     +           A(7,L)*DELX + A(8,L)*DELY + A(9,L)*DELZ +
     +           F(L)
            QLX = QLX + A(7,L)
            QLY = QLY + A(8,L)
            QLZ = QLZ + A(9,L)
            SW = SW + W
            SWX = SWX + WX
            SWY = SWY + WY
            SWZ = SWZ + WZ
            SWQ = SWQ + W*QL
            SWQX = SWQX + WX*QL + W*QLX
            SWQY = SWQY + WY*QL + W*QLY
            SWQZ = SWQZ + WZ*QL + W*QLZ
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
      IF (SW .EQ. 0.) GO TO 6
      Q = SWQ/SW
      SWS = SW*SW
      QX = (SWQX*SW - SWQ*SWX)/SWS
      QY = (SWQY*SW - SWQ*SWY)/SWS
      QZ = (SWQZ*SW - SWQ*SWZ)/SWS
      IER = 0
      RETURN
C
C  (PX,PY,PZ) = (X(L),Y(L),Z(L))
C
    4 Q = F(L)
      QX = A(7,L)
      QY = A(8,L)
      QZ = A(9,L)
      IER = 0
      RETURN
C
C  Invalid input parameter.
C
   30 CONTINUE
      CALL SHERR (1,'SH3GRD - number of input data must be > 9',41)
      IER = 1
      RETURN
   31 CONTINUE
      CALL SHERR (7,'SH3GRD - cell grid dimensions must be positive',
     +            46)
      IER = 7
      RETURN
   32 CONTINUE
      CALL SHERR (10,'SH3GRD - at least two points must have different X
     + coordinates',62)
      IER = 10
      RETURN
   33 CONTINUE
      CALL SHERR (11,'SH3GRD - at least two points must have different Y
     + coordinates',62)
      IER = 11
      RETURN
   34 CONTINUE
      CALL SHERR (12,'SH3GRD - at least two points must have different Z
     + coordinates',62)
      IER = 12
      RETURN
   35 CONTINUE
      CALL SHERR (14,
     +'SH3GRD - negative search radius in calculating least squares fit'      
     +            ,64)
      IER = 14
      RETURN
C
C  No cells contain a point within RMAX of P, or
C  SW = 0 and thus DS .GE. RSQ(L) for all L.
C
    6 CONTINUE
      CALL SHERR (13,
     +            'SH3GRD - no cells contain a point within the radius o
     +f influence of the input point',83)
      Q = 0.
      QX = 0.
      QY = 0.
      QZ = 0.
      IER = 13
      RETURN
      END
