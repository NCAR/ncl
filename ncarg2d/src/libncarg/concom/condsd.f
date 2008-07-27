C
C	$Id: condsd.f,v 1.4 2008-07-27 00:16:55 haley Exp $
C                                                                      
C                Copyright (C)  2000
C        University Corporation for Atmospheric Research
C                All Rights Reserved
C
C The use of this Software is governed by a License Agreement.
C
      SUBROUTINE CONDSD
C
C  DRAW THE OUTLINE OF THE SHIELD ON THE PLOT
C
      COMMON /CONR13/XVS(50),YVS(50),ICOUNT,SPVAL,SHIELD,
     1               SLDPLT
      LOGICAL SHIELD,SLDPLT
C
        SAVE
C
C  GET THE START POINT
C
      XS = XVS(1)
      YS = YVS(1)
C
C  MOVE TO THE START OF THE OUTLINE
C
      CALL FL2INT(XS,YS,IX,IY)
      CALL PLOTIT(IX,IY,0)
C
C  LOOP FOR ALL SHIELD ELEMENTS
C
      DO 100 IC = 2,ICOUNT
C
C       DRAW THE OUTLINE OF THE SHIELD
C
        CALL FL2INT(XVS(IC),YVS(IC),IX,IY)
        CALL PLOTIT(IX,IY,1)
C
 100  CONTINUE
C
C  DRAW TO THE START
C
      CALL FL2INT(XS,YS,IX,IY)
      CALL PLOTIT(IX,IY,1)
C
C  FLUSH PLOTIT BUFFER
C
      CALL PLOTIT(0,0,0)
      RETURN
C
      END
