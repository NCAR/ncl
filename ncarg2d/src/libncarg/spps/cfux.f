C
C	$Id: cfux.f,v 1.1.1.1 1992-04-17 22:32:26 ncargd Exp $
C
      FUNCTION CFUX (RX)
C
C Given an x coordinate RX in the fractional system, CFUX(RX) is an x
C coordinate in the user system.
C
      COMMON /IUTLCM/ LL,MI,MX,MY,IU(96)
      SAVE /IUTLCM/
      DIMENSION WD(4),VP(4)
      CALL GQCNTN (IE,NT)
      CALL GQNT (NT,IE,WD,VP)
      I=1
      IF (MI.GE.3) I=2
      CFUX=WD(I)+(RX-VP(1))/(VP(2)-VP(1))*(WD(3-I)-WD(I))
      IF (LL.GE.3) CFUX=10.**CFUX
      RETURN
      END
