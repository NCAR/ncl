C
C $Id: point.f,v 1.4 1994-03-17 01:44:09 kennison Exp $
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
