C
C	$Id: cufy.f,v 1.1.1.1 1992-04-17 22:32:28 ncargd Exp $
C
      FUNCTION CUFY (RY)
C
C Given a y coordinate RY in the user system, CUFY(RY) is a y
C coordinate in the fractional system.
C
      COMMON /IUTLCM/ LL,MI,MX,MY,IU(96)
      SAVE /IUTLCM/
      DIMENSION WD(4),VP(4)
      CALL GQCNTN (IE,NT)
      CALL GQNT (NT,IE,WD,VP)
      I=3
      IF (MI.EQ.2.OR.MI.GE.4) I=4
      IF (LL.LE.1.OR.LL.EQ.3) THEN
        CUFY=(RY-WD(I))/(WD(7-I)-WD(I))*(VP(4)-VP(3))+VP(3)
      ELSE
        CUFY=(ALOG10(RY)-WD(I))/(WD(7-I)-WD(I))*(VP(4)-VP(3))+VP(3)
      ENDIF
      RETURN
      END
