C
C $Id: line.f,v 1.5 1994-03-17 01:44:00 kennison Exp $
C
      SUBROUTINE LINE (X1,Y1,X2,Y2)
C
C Draw a line connecting the point (X1,Y1) to the point (X2,Y2), in the
C user coordinate system.
C
      IF (ICFELL('LINE - UNCLEARED PRIOR ERROR',1).NE.0) RETURN
      CALL PLOTIF (CUFX(X1),CUFY(Y1),0)
      IF (ICFELL('LINE',2).NE.0) RETURN
      CALL PLOTIF (CUFX(X2),CUFY(Y2),1)
      IF (ICFELL('LINE',3).NE.0) RETURN
      CALL PLOTIF (0.,0.,2)
      IF (ICFELL('LINE',4).NE.0) RETURN
      RETURN
      END
