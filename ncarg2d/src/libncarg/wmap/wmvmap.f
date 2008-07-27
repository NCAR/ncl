C
C	$Id: wmvmap.f,v 1.3 2008-07-27 00:17:37 haley Exp $
C                                                                      
C                Copyright (C)  2000
C        University Corporation for Atmospheric Research
C                All Rights Reserved
C
C The use of this Software is governed by a License Agreement.
C
      SUBROUTINE WMVECTMAP(X,Y,U,V)
C
C  This subroutine plots wind vectors over a map with the origin 
C  of the vector at lat/lon coordinate (X,Y).  U and V are the 
C  components of the wind vector.
C
      include 'wmcomn.h'
C
      PARAMETER (R2D=57.2957795131,D2R=0.0174532925)
C
C  Do a call forcing a BLOCKDATA to be loaded from a binary library.
C
      CALL WMBLDA
C
C  Adjust the vector direction depending on its location
C  on a map.
C
      CALL MAPTRN(X,Y,X0,Y0)
      ANGR = ATAN2(U,V)
      CALL MAPTRN(X+0.1*COS(ANGR),Y+0.1/COS(D2R*X)*SIN(ANGR),XA,YA)
      ANGTMP = ATAN2(YA-Y0,XA-X0)
      VLEN = SQRT(U*U+V*V)
      UTMP = VLEN*COS(ANGTMP)
      VTMP = VLEN*SIN(ANGTMP)
      CALL WMVECT(X0,Y0,UTMP,VTMP)
C
      RETURN
      END
