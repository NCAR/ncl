C
C $Id: curve.f,v 1.7 2008-07-27 00:17:24 haley Exp $
C
C                Copyright (C)  2000
C        University Corporation for Atmospheric Research
C                All Rights Reserved
C
C The use of this Software is governed by a License Agreement.
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
            NQ=MIN(10,NP-IP+1)
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
