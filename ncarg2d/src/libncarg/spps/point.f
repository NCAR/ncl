C
C $Id: point.f,v 1.8 2008-07-27 00:17:25 haley Exp $
C
C                Copyright (C)  2000
C        University Corporation for Atmospheric Research
C                All Rights Reserved
C
C The use of this Software is governed by a License Agreement.
C
      SUBROUTINE POINT (PX,PY)
C
C Draws a point at (PX,PY), defined in the user coordinate system.
C What is actually drawn, for various reasons, is a little X.
C
      IF (ICFELL('POINT - UNCLEARED PRIOR ERROR',1).NE.0) RETURN
      QX=CUFX(PX)
      IF (ICFELL('POINT',2).NE.0) RETURN
      QY=CUFY(PY)
      IF (ICFELL('POINT',3).NE.0) RETURN
      CALL PLOTIF (QX-.0005,QY-.0005,0)
      IF (ICFELL('POINT',4).NE.0) RETURN
      CALL PLOTIF (QX+.0005,QY+.0005,1)
      IF (ICFELL('POINT',5).NE.0) RETURN
      CALL PLOTIF (QX-.0005,QY+.0005,0)
      IF (ICFELL('POINT',6).NE.0) RETURN
      CALL PLOTIF (QX+.0005,QY-.0005,1)
      IF (ICFELL('POINT',7).NE.0) RETURN
      CALL PLOTIF (0.,0.,2)
      IF (ICFELL('POINT',8).NE.0) RETURN
      RETURN
      END
