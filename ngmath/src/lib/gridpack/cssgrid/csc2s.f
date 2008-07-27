C
C	$Id: csc2s.f,v 1.5 2008-07-27 03:10:06 haley Exp $
C                                                                      
C                Copyright (C)  2000
C        University Corporation for Atmospheric Research
C                All Rights Reserved
C
C The use of this Software is governed by a License Agreement.
C
      SUBROUTINE CSC2S (N,X,Y,Z,RLAT,RLON)
      REAL X(N),Y(N),Z(N),RLAT(N),RLON(N)
C
C  This subroutine converts a point P from Cartesian coor-
C  dinates on a unit sphere to spherical coordinates.
C
C
C On input:
C
C       X,Y,Z = Cartesian coordinates of P.
C
C On output:
C
C       RLAT = Latitude of P, in degrees. - 90. .LE. RLAT .LE. 90.
C
C       PLON = Longitude of P, in degrees. -180. .LE. RLON .LE. 180.
C
C Modules required by CSC2S:  CSSCOORD
C
C Intrinsic functions called by CSC2S:  None
C
C***********************************************************
C
      PARAMETER (D2R=0.017453293, R2D=57.295778)
C
      DO 10 I=1,N
        CALL CSSCOORD(X(I),Y(I),Z(I),RLATD,RLOND,PRNM)
        RLAT(I) = R2D*RLATD
        RLON(I) = R2D*RLOND
   10 CONTINUE
C
      RETURN
      END
