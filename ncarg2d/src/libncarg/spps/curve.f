C
C $Id: curve.f,v 1.5 2000-08-22 15:06:09 haley Exp $
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
      SUBROUTINE CURVE (PX,PY,NP)
C
      DIMENSION PX(NP),PY(NP)
C
C CURVE draws the curve defined by the points (PX(I),PY(I)), for I = 1
C to NP.  All coordinates are stated in the user coordinate system.
C
C Define arrays to hold converted point coordinates when it becomes
C necessary to draw the curve piecewise.
C
      DIMENSION QX(10),QY(10)
C
C Check for an uncleared prior error.
C
      IF (ICFELL('CURVE - UNCLEARED PRIOR ERROR',1).NE.0) RETURN
C
C If NP is less than or equal to zero, there's nothing to do.
C
      IF (NP.LE.0) RETURN
C
C If NP is exactly equal to 1, just draw a point.
C
      IF (NP.EQ.1) THEN
        CALL POINT (PX(1),PY(1))
        IF (ICFELL('CURVE',2).NE.0) RETURN
C
C Otherwise, draw the curve.
C
      ELSE
C
C Flush the pen-move buffer.
C
        CALL PLOTIF (0.,0.,2)
        IF (ICFELL('CURVE',3).NE.0) RETURN
C
C Save the current SET parameters.
C
        CALL GETSET (F1,F2,F3,F4,F5,F6,F7,F8,LL)
        IF (ICFELL('CURVE',4).NE.0) RETURN
C
C If the mapping defined by the last SET call was non-reversed and
C linear in both x and y, a single polyline will suffice.
C
        IF (F5.LT.F6.AND.F7.LT.F8.AND.LL.EQ.1) THEN
          CALL GPL (NP,PX,PY)
C
C Otherwise, piece the line together out of smaller chunks, converting
C the coordinates for each chunk as directed by the last SET call.
C
        ELSE
          DO 102 IP=1,NP,9
            NQ=MIN0(10,NP-IP+1)
            IF (NQ.GE.2) THEN
              DO 101 IQ=1,NQ
                QX(IQ)=CUFX(PX(IP+IQ-1))
                QY(IQ)=CUFY(PY(IP+IQ-1))
  101         CONTINUE
              CALL SET (F1,F2,F3,F4,F1,F2,F3,F4,1)
              IF (ICFELL('CURVE',5).NE.0) RETURN
              CALL GPL (NQ,QX,QY)
              CALL SET (F1,F2,F3,F4,F5,F6,F7,F8,LL)
              IF (ICFELL('CURVE',6).NE.0) RETURN
            END IF
  102     CONTINUE
        END IF
C
C Update the pen position.
C
        CALL FRSTPT (PX(NP),PY(NP))
        IF (ICFELL('CURVE',7).NE.0) RETURN
C
      END IF
C
C Done.
C
      RETURN
C
      END
