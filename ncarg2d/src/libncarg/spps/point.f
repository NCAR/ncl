C
C $Id: point.f,v 1.2 1993-03-02 17:50:18 kennison Exp $
C
      SUBROUTINE POINT (PX,PY)
C
C Draws a point at (PX,PY), defined in the user coordinate system.
C What is actually drawn, for various reasons, is a little X.
C
      QX=CUFX(PX)
      QY=CUFY(PY)
      CALL PLOTIF (QX-.0005,QY-.0005,0)
      CALL PLOTIF (QX+.0005,QY+.0005,1)
      CALL PLOTIF (QX-.0005,QY+.0005,0)
      CALL PLOTIF (QX+.0005,QY-.0005,1)
      CALL PLOTIF (0.,0.,2)
      RETURN
      END
