C
C       $Id: stumsl.f,v 1.2 1993-12-03 21:18:59 kennison Exp $
C
      SUBROUTINE STUMSL(XCS,YCS,NCS,IAI,IAG,NAI)
C
      DIMENSION XCS(NCS),YCS(NCS),IAI(NAI),IAG(NAI)
C
C
C Example version of user masked drawing program
C
C XCS,YCS    - Arrays of X,Y coordinates
C NCS        - Number of points
C IAI        - Area identifier array
C IAG        - Area group array
C NAI        - number of area/group identifiers
C
C --------------------------------------------------------------------
C
      DO 101 I=1,NAI
         IF (IAI(I).LT.0) RETURN
 101  CONTINUE
C
C If drawing is turned on, draw the polyline.
C
      CALL CURVE(XCS,YCS,NCS)
C
C Done.
C
      RETURN
C
      END
