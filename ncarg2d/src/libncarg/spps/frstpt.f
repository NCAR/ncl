C
C	$Id: frstpt.f,v 1.1.1.1 1992-04-17 22:32:29 ncargd Exp $
C
      SUBROUTINE FRSTPT (PX,PY)
C
C Given the user coordinates PX and PY of a point, FRSTPT generates a
C pen-up move to that point.
C
      CALL PLOTIF (CUFX(PX),CUFY(PY),0)
C
C Done.
C
      RETURN
C
      END
