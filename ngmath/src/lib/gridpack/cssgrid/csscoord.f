C
C	$Id: csscoord.f,v 1.3 2000-07-13 02:49:12 haley Exp $
C                                                                      
C                Copyright (C)  2000
C        University Corporation for Atmospheric Research
C                All Rights Reserved
C
C This file is free software; you can redistribute it and/or modify
C it under the terms of the GNU Lesser General Public License as
C published by the Free Software Foundation; either version 2.1 of the
C License, or (at your option) any later version.
C
C This software is distributed in the hope that it will be useful, but
C WITHOUT ANY WARRANTY; without even the implied warranty of
C MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
C Lesser General Public License for more details.
C
C You should have received a copy of the GNU Lesser General Public
C License along with this software; if not, write to the Free Software
C Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA 02111-1307
C USA.
C
      SUBROUTINE CSSCOORD (PX,PY,PZ, PLAT,PLON,PNRM)
      REAL PX, PY, PZ, PLAT, PLON, PNRM
C
C***********************************************************
C
C                                              From STRIPACK
C                                            Robert J. Renka
C                                  Dept. of Computer Science
C                                       Univ. of North Texas
C                                           renka@cs.unt.edu
C                                                   08/27/90
C
C   This subroutine converts a point P from Cartesian coor-
C dinates to spherical coordinates.
C
C
C On input:
C
C       PX,PY,PZ = Cartesian coordinates of P.
C
C Input parameters are not altered by this routine.
C
C On output:
C
C       PLAT = Latitude of P in the range -PI/2 to PI/2, or
C              0 if PNRM = 0.  PLAT should be scaled by
C              180/PI to obtain the value in degrees.
C
C       PLON = Longitude of P in the range -PI to PI, or 0
C              if P lies on the Z-axis.  PLON should be
C              scaled by 180/PI to obtain the value in
C              degrees.
C
C       PNRM = Magnitude (Euclidean norm) of P.
C
C Modules required by CSSCOORD:  None
C
C Intrinsic functions called by CSSCOORD:  ASIN, ATAN2, SQRT
C
C***********************************************************
C
      PNRM = SQRT(PX*PX + PY*PY + PZ*PZ)
      IF (PX .NE. 0.D0  .OR.  PY .NE. 0.D0) THEN
        PLON = ATAN2(PY,PX)
      ELSE
        PLON = 0.D0
      ENDIF
      IF (PNRM .NE. 0.D0) THEN
        PLAT = ASIN(PZ/PNRM)
      ELSE
        PLAT = 0.D0
      ENDIF
      RETURN
      END
