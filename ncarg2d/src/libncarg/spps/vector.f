C
C $Id: vector.f,v 1.3 1994-03-17 01:44:24 kennison Exp $
C
      SUBROUTINE VECTOR (PX,PY)
C
C Draw a vector (line segment) from the current pen position to the new
C pen position (PX,PY), in the user coordinate system, and then make
C (PX,PY) the current pen position.
C
      IF (ICFELL('VECTOR - UNCLEARED PRIOR ERROR',1).NE.0) RETURN
      CALL PLOTIF (CUFX(PX),CUFY(PY),1)
      IF (ICFELL('VECTOR',2).NE.0) RETURN
      RETURN
      END
