C
C $Id: point.f,v 1.3 1993-12-12 20:56:09 kennison Exp $
C
      SUBROUTINE POINT (PX,PY)
C
C Draws a point at (PX,PY), defined in the user coordinate system.
C What is actually drawn, for various reasons, is a little X.
C
      QX=CUFX(PX)
      QY=CUFY(PY)
      CALL PLOTIF (QX-.0005,QY-.0005,0)
      IF (ICFELL('POINT',1).NE.0) RETURN
      CALL PLOTIF (QX+.0005,QY+.0005,1)
      IF (ICFELL('POINT',2).NE.0) RETURN
      CALL PLOTIF (QX-.0005,QY+.0005,0)
      IF (ICFELL('POINT',3).NE.0) RETURN
      CALL PLOTIF (QX+.0005,QY-.0005,1)
      IF (ICFELL('POINT',4).NE.0) RETURN
      CALL PLOTIF (0.,0.,2)
      IF (ICFELL('POINT',5).NE.0) RETURN
      RETURN
      END
