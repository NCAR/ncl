C
C	$Id: condsd.f,v 1.2 2000-07-12 16:22:21 haley Exp $
C                                                                      
C                Copyright (C)  2000
C        University Corporation for Atmospheric Research
C                All Rights Reserved
C
C This file is free software; you can redistribute it and/or modify
C it under the terms of the GNU Lesser General Public License as
C published by the Free Software Foundation; either version 2.1 of the
C License, or (at your option) any later version.
C
C This software is distributed in the hope that it will be useful, but
C WITHOUT ANY WARRANTY; without even the implied warranty of
C MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
C Lesser General Public License for more details.
C
C You should have received a copy of the GNU Lesser General Public
C License along with this software; if not, write to the Free Software
C Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA 02111-1307
C USA.
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
