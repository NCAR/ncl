C
C $Id: line.f,v 1.3 1993-03-02 17:50:16 kennison Exp $
C
      SUBROUTINE LINE (X1,Y1,X2,Y2)
C
C Draw a line connecting the point (X1,Y1) to the point (X2,Y2), in the
C user coordinate system.
C
      CALL PLOTIF (CUFX(X1),CUFY(Y1),0)
      CALL PLOTIF (CUFX(X2),CUFY(Y2),1)
      CALL PLOTIF (0.,0.,2)
      RETURN
      END
