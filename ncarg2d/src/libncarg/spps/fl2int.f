C
C	$Id: fl2int.f,v 1.1.1.1 1992-04-17 22:32:29 ncargd Exp $
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
      CALL GQNT (NT,IE,WD,VP)
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
