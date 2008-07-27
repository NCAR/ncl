C
C       $Id: shsetup.f,v 1.4 2008-07-27 03:10:14 haley Exp $
C                                                                      
C                Copyright (C)  2000
C        University Corporation for Atmospheric Research
C                All Rights Reserved
C
C The use of this Software is governed by a License Agreement.
C
      SUBROUTINE SHSETUP (XK,YK,ZK,FK,XI,YI,ZI,FI,S1,S2,
     +                   R, ROW)
      REAL XK, YK, ZK, FK, XI, YI, ZI, FI, S1, S2,
     +     R, ROW(10)
C
C***********************************************************
C
C                                               ROBERT RENKA
C                                       UNIV. OF NORTH TEXAS
C                                             (817) 565-2767
C
C  This routine sets up the I-th row of an augmented re-
C  gression matrix for a weighted least-squares fit of a
C  quadratic function Q(X,Y,Z) to a set of data values F,
C  where Q(XK,YK,ZK) = FK.  The first 6 columns (quadratic
C  terms) are scaled by 1/S2, and columns 7, 8, and 9 (linear
C  terms) are scaled by 1/S1.  The weight is (R-D)/(R*D) if
C  R .GT. D, and 0 if R .LE. D, where D is the distance between
C  nodes I and K.
C
C  ON INPUT --
C
C    XK,YK,ZK,FK = Coordinates and data value at node K
C                  (interpolated by Q).
C
C    XI,YI,ZI,FI = Coordinates and data value at node I.
C
C    S1,S2 = Reciprocals of the scale factors.
C
C    R = Radius of influence about node K defining the weight.
C
C    ROW = Array of length 10.
C
C  Input parameters are not altered by this routine.
C
C  ON OUTPUT --
C
C    ROW = Array containing a row of the augmented regression matrix.
C
C  MODULES REQUIRED BY SHSETUP -- NONE
C
C  INTRINSIC FUNCTION CALLED BY SHSETUP -- SQRT
C
C***********************************************************
C
      INTEGER I
      REAL    DX, DY, DZ, DXSQ, DYSQ, DZSQ, D, W, W1, W2
C
C  Local parameters -
C
C  I =    DO-LOOP INDEX
C  DX =   XI - XK
C  DY =   YI - YK
C  DZ =   ZI - ZK
C  DXSQ = DX*DX
C  DYSQ = DY*DY
C  DZSQ = DZ*DZ
C  D =    distance between nodes K and I
C  W =    weight associated with the row
C  W1 =   W/S1
C  W2 =   W/S2
C
      DX = XI - XK
      DY = YI - YK
      DZ = ZI - ZK
      DXSQ = DX*DX
      DYSQ = DY*DY
      DZSQ = DZ*DZ
      D = SQRT(DXSQ + DYSQ + DZSQ)
      IF (D .LE. 0.  .OR.  D .GE. R) GO TO 1
      W = (R-D)/R/D
      W1 = W/S1
      W2 = W/S2
      ROW(1) = DXSQ*W2
      ROW(2) = DX*DY*W2
      ROW(3) = DYSQ*W2
      ROW(4) = DX*DZ*W2
      ROW(5) = DY*DZ*W2
      ROW(6) = DZSQ*W2
      ROW(7) = DX*W1
      ROW(8) = DY*W1
      ROW(9) = DZ*W1
      ROW(10) = (FI - FK)*W
      RETURN
C
C  Nodes K and I coincide or node I is outside of the radiuS
C  of influence.  Set ROW to the zero vector.
C
    1 DO 2 I = 1,10
    2   ROW(I) = 0.
      RETURN
      END
