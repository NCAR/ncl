C
C	$Id: csconstr.f,v 1.4 2000-08-22 15:19:08 haley Exp $
C                                                                      
C                Copyright (C)  2000
C        University Corporation for Atmospheric Research
C                All Rights Reserved
C
C This file is free software; you can redistribute it and/or modify
C it under the terms of the GNU General Public License as published
C by the Free Software Foundation; either version 2 of the License, or
C (at your option) any later version.
C
C This software is distributed in the hope that it will be useful, but
C WITHOUT ANY WARRANTY; without even the implied warranty of
C MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
C General Public License for more details.
C
C You should have received a copy of the GNU General Public License
C along with this software; if not, write to the Free Software
C Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA 02111-1307
C USA.
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
