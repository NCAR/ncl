C
C $Id: fl2int.f,v 1.5 2000-08-22 15:06:09 haley Exp $
C                                                                      
C                Copyright (C)  2000
C        University Corporation for Atmospheric Research
C                All Rights Reserved
C
C This file is free software; you can redistribute it and/or modify
C it under the terms of the GNU General Public License as published
C by the Free Software Foundation; either version 2 of the License, or
C (at your option) any later version.
C
C This software is distributed in the hope that it will be useful, but
C WITHOUT ANY WARRANTY; without even the implied warranty of
C MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
C General Public License for more details.
C
C You should have received a copy of the GNU General Public License
C along with this software; if not, write to the Free Software
C Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA 02111-1307
C USA.
C
      SUBROUTINE FL2INT (PX,PY,IX,IY)
C
C Given the user coordinates PX and PY of a point, FL2INT returns the
C metacode coordinates IX and IY of that point.
C
C Declare the common block containing the user state variables LL, MI,
C MX, and MY.
C
      COMMON /IUTLCM/ LL,MI,MX,MY,IU(96)
      SAVE /IUTLCM/
C
C Declare arrays in which to retrieve the variables defining the current
C window and viewport.
C
      DIMENSION WD(4),VP(4)
C
C Get the variables defining the current window and viewport.
C
      CALL GQCNTN (IE,NT)
      IF (IE.NE.0) THEN
        CALL SETER ('FL2INT - ERROR EXIT FROM GQCNTN',1,1)
        RETURN
      END IF
      CALL GQNT (NT,IE,WD,VP)
      IF (IE.NE.0) THEN
        CALL SETER ('FL2INT - ERROR EXIT FROM GQNT',2,1)
        RETURN
      END IF
C
C Compute IX.
C
      I=1
      IF (MI.GE.3) I=2
      IF (LL.LE.2) THEN
        IX=IFIX(((PX-WD(I))/(WD(3-I)-WD(I))*(VP(2)-VP(1))+VP(1))*32767.)
      ELSE
        IX=IFIX(((ALOG10(PX)-WD(I))/(WD(3-I)-WD(I))*
     +                              (VP(2)-VP(1))+VP(1))*32767.)
      ENDIF
C
C Compute IY.
C
      I=3
      IF (MI.EQ.2.OR.MI.GE.4) I=4
      IF (LL.LE.1.OR.LL.EQ.3) THEN
        IY=IFIX(((PY-WD(I))/(WD(7-I)-WD(I))*(VP(4)-VP(3))+VP(3))*32767.)
      ELSE
        IY=IFIX(((ALOG10(PY)-WD(I))/(WD(7-I)-WD(I))*
     +                              (VP(4)-VP(3))+VP(3))*32767.)
      ENDIF
C
C Done.
C
      RETURN
C
      END
