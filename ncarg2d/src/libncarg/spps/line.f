C
C	$Id: line.f,v 1.1.1.1 1992-04-17 22:32:30 ncargd Exp $
C
      SUBROUTINE LINE (X1,Y1,X2,Y2)
C
C Draw a line connecting the point (X1,Y1) to the point (X2,Y2), in the
C user coordinate system.
C
      CALL PLOTIF (CUFX(X1),CUFY(Y1),0)
      CALL PLOTIF (CUFX(X2),CUFY(Y2),1)
      RETURN
      END
