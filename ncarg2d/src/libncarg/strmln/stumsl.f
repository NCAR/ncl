C
C       $Id: stumsl.f,v 1.5 2008-07-27 00:17:28 haley Exp $
C                                                                      
C                Copyright (C)  2000
C        University Corporation for Atmospheric Research
C                All Rights Reserved
C
C The use of this Software is governed by a License Agreement.
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
