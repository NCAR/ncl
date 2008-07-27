C
C $Id: fl2int.f,v 1.7 2008-07-27 00:17:24 haley Exp $
C
C                Copyright (C)  2000
C        University Corporation for Atmospheric Research
C                All Rights Reserved
C
C The use of this Software is governed by a License Agreement.
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
        IX=INT(((PX-WD(I))/(WD(3-I)-WD(I))*(VP(2)-VP(1))+VP(1))*32767.)
      ELSE
        IX=INT(((ALOG10(PX)-WD(I))/(WD(3-I)-WD(I))*
     +                             (VP(2)-VP(1))+VP(1))*32767.)
      ENDIF
C
C Compute IY.
C
      I=3
      IF (MI.EQ.2.OR.MI.GE.4) I=4
      IF (LL.LE.1.OR.LL.EQ.3) THEN
        IY=INT(((PY-WD(I))/(WD(7-I)-WD(I))*(VP(4)-VP(3))+VP(3))*32767.)
      ELSE
        IY=INT(((ALOG10(PY)-WD(I))/(WD(7-I)-WD(I))*
     +                             (VP(4)-VP(3))+VP(3))*32767.)
      ENDIF
C
C Done.
C
      RETURN
C
      END
