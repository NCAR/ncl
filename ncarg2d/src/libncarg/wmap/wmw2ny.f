C
C	$Id: wmw2ny.f,v 1.1 1994-09-09 23:55:44 fred Exp $
C
      SUBROUTINE WMW2NY(NPT,P,Q)
C
C  This subroutine takes the NPT points in the real array P,
C  which are assumed to be in world coordinates, and, using
C  the current normalization transformation, converts them
C  into NDC coordinates and stores them in Q.  This subroutine
C  operates on Y-coordinates.
C
      INTEGER NPT
      REAL P(NPT),Q(NPT)
      REAL WINDOW(4),VIEWPT(4)
C
      CALL GQCNTN(IER,NTR)
      CALL GQNT(NTR,IER,WINDOW,VIEWPT)
      SCALE = (VIEWPT(4)-VIEWPT(3))/(WINDOW(4)-WINDOW(3))
      DO 20 I=1,NPT
        Q(I) = VIEWPT(3)+(P(I)-WINDOW(3))*SCALE
   20 CONTINUE
C
      RETURN
      END
