C
C $Id: bcdp2l.f,v 1.2 2000-07-12 16:26:15 haley Exp $
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
      REAL FUNCTION BCDP2L(PX,PY,X1,Y1,X2,Y2)
C
C  Calculate the undirected distance from a point (PX,PY) to a line
C  passing through the two points (X1,Y1) and (X2,Y2).  Use the formula
C  for the distance from a point (PX,PY) to a line  A*X+B*Y+C = 0
C  given by:
C
C     ABS(A*PX+B*PY+C)/SQRT(A**2+B**2)
C
      IF (X1 .EQ. X2) THEN
        IF (Y1 .EQ. Y2) THEN
C
C  Return the distance from (PX,PY) to the point (X1,Y1) which is greater
C  than or equal to the distance from (PX,PY) to any line passing through
C  (X1,Y1) = (X2,Y2).
C
          BCDP2L = SQRT((PY-Y1)**2+(PX-X1)**2)
          GO TO 10
        ENDIF
        BCDP2L = ABS(PX-X1)
        GO TO 10
      ELSE
        CONST = (Y1-Y2)/(X2-X1)
        A = CONST
        B = 1.
        C = -(CONST*X1+Y1)
        BCDP2L = ABS(A*PX+B*PY+C)/SQRT(A*A+B*B)
        GO TO 10
      ENDIF
C
   10 CONTINUE
      RETURN
      END
