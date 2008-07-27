C     
C                Copyright (C)  2000
C        University Corporation for Atmospheric Research
C                All Rights Reserved
C
C The use of this Software is governed by a License Agreement.
C
      FUNCTION SPLFED(NDIM,X,COEF,XMIN,XMAX,NODES,IERROR)
      DOUBLE PRECISION SPLFED
      DOUBLE PRECISION X
      DOUBLE PRECISION COEF
      DOUBLE PRECISION XMIN
      DOUBLE PRECISION XMAX
      DOUBLE PRECISION SPLDED
      DIMENSION X(NDIM),COEF(*),XMIN(NDIM),XMAX(NDIM),NODES(NDIM)
      DIMENSION NDERIV(4)
      SAVE
C
      DATA NDERIV(1),NDERIV(2),NDERIV(3),NDERIV(4)/0,0,0,0/
C
C  The restriction for NDIM to be .LE. 4 can be eliminated by
C  increasing the above dimension and those in SPLDED.
C
      SPLFED = SPLDED(NDIM,X,NDERIV,COEF,XMIN,XMAX,NODES,IERROR)
C
      RETURN
      END
