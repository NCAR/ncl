C
C	$Id: wmsect.f,v 1.4 2008-07-27 00:17:37 haley Exp $
C                                                                      
C                Copyright (C)  2000
C        University Corporation for Atmospheric Research
C                All Rights Reserved
C
C The use of this Software is governed by a License Agreement.
C
      SUBROUTINE WMSECT(RECT1,RECT2,ISECT,RECT3)
C
C  Given two rectangles RECT1 and RECT2 (stored as XL,YB,XR,YT),
C  calculate the intersection of the two rectangles.  If the 
C  intersection is not null, set ISECT to 1 and store the intersection
C  in RECT3.  If the intersection is null, set ISECT to 0 and return
C  all 0's in RECT3.
C
      DIMENSION RECT1(4),RECT2(4),RECT3(4)
C
      S2LLX = RECT2(1)
      S2LLY = RECT2(2)
      S2URX = RECT2(3)
      S2URY = RECT2(4)
      S1LLX = RECT1(1)
      S1LLY = RECT1(2)
      S1URX = RECT1(3)
      S1URY = RECT1(4)
C
      IF (S2LLX.LT.S1URX .AND. S1LLX.LT.S2URX .AND.
     +    S2LLY.LT.S1URY .AND. S1LLY.LT.S2URY)  THEN
        ISECT = 1
        RECT3(1) = MAX(S1LLX, S2LLX)
        RECT3(2) = MAX(S1LLY, S2LLY)
        RECT3(3) = MIN(S1URX, S2URX)
        RECT3(4) = MIN(S1URY, S2URY)
      ELSE
        ISECT = 0
        RECT3(1) = 0.
        RECT3(2) = 0.
        RECT3(3) = 0.
        RECT3(4) = 0.
      ENDIF
C
      RETURN
      END
