C
C	$Id: kupx.f,v 1.1.1.1 1992-04-17 22:32:29 ncargd Exp $
C
      FUNCTION KUPX (RX)
C
C Given an x coordinate RX in the user system, KUPX(RX) is an x
C coordinate in the plotter system.
C
      COMMON /IUTLCM/ LL,MI,MX,MY,IU(96)
      SAVE /IUTLCM/
      DIMENSION WD(4),VP(4)
      CALL GQCNTN (IE,NT)
      CALL GQNT (NT,IE,WD,VP)
      I=1
      IF (MI.GE.3) I=2
      IF (LL.LE.2) THEN
        KUPX=1+IFIX(((RX-WD(I))/(WD(3-I)-WD(I))*(VP(2)-VP(1))+VP(1))*
     +              (2.**MX-1.))
      ELSE
        KUPX=1+IFIX(((ALOG10(RX)-WD(I))/(WD(3-I)-WD(I))*(VP(2)-VP(1))+
     +              VP(1))*(2.**MX-1.))
      ENDIF
      RETURN
      END
