C
C $Id: frstpt.f,v 1.3 1994-03-17 01:43:30 kennison Exp $
C
      SUBROUTINE FRSTPT (PX,PY)
C
C Given the user coordinates PX and PY of a point, FRSTPT generates a
C pen-up move to that point.
C
      IF (ICFELL('FRSTPT - UNCLEARED PRIOR ERROR',1).NE.0) RETURN
      CALL PLOTIF (CUFX(PX),CUFY(PY),0)
      IF (ICFELL('FRSTPT',2).NE.0) RETURN
C
C Done.
C
      RETURN
C
      END
