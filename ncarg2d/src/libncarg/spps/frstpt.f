C
C $Id: frstpt.f,v 1.2 1993-12-12 20:55:22 kennison Exp $
C
      SUBROUTINE FRSTPT (PX,PY)
C
C Given the user coordinates PX and PY of a point, FRSTPT generates a
C pen-up move to that point.
C
      CALL PLOTIF (CUFX(PX),CUFY(PY),0)
      IF (ICFELL('FRSTPT',1).NE.0) RETURN
C
C Done.
C
      RETURN
C
      END
