C
C $Id: idxchg.f,v 1.6 2008-07-27 00:17:30 haley Exp $
C
C                Copyright (C)  2000
C        University Corporation for Atmospheric Research
C                All Rights Reserved
C
C The use of this Software is governed by a License Agreement.
C
      FUNCTION IDXCHG (X,Y,I1,I2,I3,I4)
C
C This function determines whether or not two adjacent triangles in the
C triangulation should be "exchanged".  By default, it uses a criterion,
C due to C. L. Lawson, that maximizes the minimum angle occurring in the
C triangles, but if ITTY is set non-zero, the function IDXCHN is called
C instead.
C
C The input arguments are as follows:
C
C   X and Y are arrays containing the coordinates of the data points.
C
C   I1, I2, I3, and I4 are point numbers of four points P1, P2, P3,
C   and P4 forming a quadrilateral with P3 and P4 connected diagonally
C   to form two triangles.
C
C The function returns an integer 1 if P1 and P2 should be connected
C instead of P3 and P4 (an "exchange"); otherwise, it returns 0.
C
C Declaration statements.
C
      COMMON /IDCOMN/ INTY,ITTY,ALSP,BLSP,CLSP,XAVG,YAVG
      SAVE   /IDCOMN/
C
      DIMENSION X(*), Y(*)
C
      EQUIVALENCE (C2SQ,C1SQ),(A3SQ,B2SQ),(B3SQ,A1SQ),
     1            (A4SQ,B1SQ),(B4SQ,A2SQ),(C4SQ,C3SQ)
C
      DATA  EPSLN / 1.E-6 /
C
C See if the new triangulation is to be produced and, if so, do it.
C
      IF (ITTY.NE.0) THEN
        IDXCHG=IDXCHN(X,Y,I1,I2,I3,I4)
        RETURN
      END IF
C
C Preliminary processing.
C
      X1=X(I1)
      Y1=Y(I1)
      X2=X(I2)
      Y2=Y(I2)
      X3=X(I3)
      Y3=Y(I3)
      X4=X(I4)
      Y4=Y(I4)
C
C Calculation.
C
      IDX=0
C
      U3=(Y2-Y3)*(X1-X3)-(X2-X3)*(Y1-Y3)
      U4=(Y1-Y4)*(X2-X4)-(X1-X4)*(Y2-Y4)
C
      IF (U3*U4.GT.0.) THEN
        U1=(Y3-Y1)*(X4-X1)-(X3-X1)*(Y4-Y1)
        U2=(Y4-Y2)*(X3-X2)-(X4-X2)*(Y3-Y2)
        A1SQ=(X1-X3)**2+(Y1-Y3)**2
        B1SQ=(X4-X1)**2+(Y4-Y1)**2
        C1SQ=(X3-X4)**2+(Y3-Y4)**2
        A2SQ=(X2-X4)**2+(Y2-Y4)**2
        B2SQ=(X3-X2)**2+(Y3-Y2)**2
        C3SQ=(X2-X1)**2+(Y2-Y1)**2
        S1SQ=U1*U1/(C1SQ*MAX(A1SQ,B1SQ))
        S2SQ=U2*U2/(C2SQ*MAX(A2SQ,B2SQ))
        S3SQ=U3*U3/(C3SQ*MAX(A3SQ,B3SQ))
        S4SQ=U4*U4/(C4SQ*MAX(A4SQ,B4SQ))
        IF ((MIN(S3SQ,S4SQ)-MIN(S1SQ,S2SQ)).GT.EPSLN) IDX=1
      END IF
C
      IDXCHG=IDX
C
C Done.
C
      RETURN
C
      END
