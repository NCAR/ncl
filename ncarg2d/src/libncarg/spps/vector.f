C
C	$Id: vector.f,v 1.1.1.1 1992-04-17 22:32:35 ncargd Exp $
C
      SUBROUTINE VECTOR (PX,PY)
C
C Draw a vector (line segment) from the current pen position to the new
C pen position (PX,PY), in the user coordinate system, and then make
C (PX,PY) the current pen position.
C
      CALL PLOTIF (CUFX(PX),CUFY(PY),1)
      RETURN
      END
