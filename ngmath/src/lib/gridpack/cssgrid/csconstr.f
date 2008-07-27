C
C	$Id: csconstr.f,v 1.5 2008-07-27 03:10:06 haley Exp $
C                                                                      
C                Copyright (C)  2000
C        University Corporation for Atmospheric Research
C                All Rights Reserved
C
C The use of this Software is governed by a License Agreement.
C
      SUBROUTINE CSCONSTR (XK,YK,ZK, CX,SX,CY,SY)
      DOUBLE PRECISION XK, YK, ZK, CX, SX, CY, SY
C
C***********************************************************
C
C                                              From SSRFPACK
C                                            Robert J. Renka
C                                  Dept. of Computer Science
C                                       Univ. of North Texas
C                                           renka@cs.unt.edu
C                                                   05/09/92
C
C   This subroutine constructs the elements of a 3 by 3
C orthogonal matrix R which rotates a point (XK,YK,ZK) on
C the unit sphere to the north pole, i.e.,
C
C      (XK)     (CY  0 -SY)   (1   0   0)   (XK)     (0)
C  R * (YK)  =  ( 0  1   0) * (0  CX -SX) * (YK)  =  (0)
C      (ZK)     (SY  0  CY)   (0  SX  CX)   (ZK)     (1)
C
C On input:
C
C       XK,YK,ZK = Components of a unit vector to be
C                  rotated to (0,0,1).
C
C Input parameters are not altered by this routine.
C
C On output:
C
C       CX,SX,CY,SY = Elements of R:  CX,SX define a rota-
C                     tion about the X-axis and CY,SY define
C                     a rotation about the Y-axis.
C
C Modules required by CSCONSTR:  None
C
C Intrinsic function called by CSCONSTR:  SQRT
C
C***********************************************************
C
      CY = SQRT(YK*YK + ZK*ZK)
      SY = XK
      IF (CY .NE. 0.) THEN
        CX = ZK/CY
        SX = YK/CY
      ELSE
C
C (XK,YK,ZK) lies on the X-axis.
C
        CX = 1.
        SX = 0.
      ENDIF
      RETURN
      END
