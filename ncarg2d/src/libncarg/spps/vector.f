C
C $Id: vector.f,v 1.2 1993-12-12 20:56:28 kennison Exp $
C
      SUBROUTINE VECTOR (PX,PY)
C
C Draw a vector (line segment) from the current pen position to the new
C pen position (PX,PY), in the user coordinate system, and then make
C (PX,PY) the current pen position.
C
      CALL PLOTIF (CUFX(PX),CUFY(PY),1)
      IF (ICFELL('VECTOR',1).NE.0) RETURN
      RETURN
      END
