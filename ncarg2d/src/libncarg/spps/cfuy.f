C
C	$Id: cfuy.f,v 1.1.1.1 1992-04-17 22:32:26 ncargd Exp $
C
      FUNCTION CFUY (RY)
C
C Given a y coordinate RY in the fractional system, CFUY(RY) is a y
C coordinate in the user system.
C
      COMMON /IUTLCM/ LL,MI,MX,MY,IU(96)
      SAVE /IUTLCM/
      DIMENSION WD(4),VP(4)
      CALL GQCNTN (IE,NT)
      CALL GQNT (NT,IE,WD,VP)
      I=3
      IF (MI.EQ.2.OR.MI.GE.4) I=4
      CFUY=WD(I)+(RY-VP(3))/(VP(4)-VP(3))*(WD(7-I)-WD(I))
      IF (LL.EQ.2.OR.LL.GE.4) CFUY=10.**CFUY
      RETURN
      END
