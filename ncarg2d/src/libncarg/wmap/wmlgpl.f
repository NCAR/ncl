C
C	$Id: wmlgpl.f,v 1.1 1994-09-09 23:55:24 fred Exp $
C
      SUBROUTINE WMLGPL(N,X,Y)
C
C  Draws polylines either using GPL (IWDTYP=0) or WMDRFL
C  (IWDTYP=1).
C
      include 'wmcomn.h'
C
C  Save the current line attributes and reset.
C
      CALL GQPLCI(IER,ICOLD)
      CALL GSPLCI(ICOLOR)
C
      IF (IWDTYP .EQ. 1) THEN
        CALL WMDRFL(N,X,Y)
      ELSE
        CALL GPL(N,X,Y)
      ENDIF
C
C  Restore line attributes.
C
      CALL GSPLCI(ICOLD)
C
      RETURN
      END
