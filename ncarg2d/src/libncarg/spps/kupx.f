C
C $Id: kupx.f,v 1.3 1994-03-17 01:43:57 kennison Exp $
C
      FUNCTION KUPX (RX)
C
C Given an X coordinate RX in the user system, KUPX(RX) is an X
C coordinate in the plotter system.
C
      COMMON /IUTLCM/ LL,MI,MX,MY,IU(96)
      SAVE /IUTLCM/
      DIMENSION WD(4),VP(4)
      CALL GQCNTN (IE,NT)
      IF (IE.NE.0) THEN
        CALL SETER ('KUPX - ERROR EXIT FROM GQCNTN',1,1)
        KUPX=0
        RETURN
      END IF
      CALL GQNT (NT,IE,WD,VP)
      IF (IE.NE.0) THEN
        CALL SETER ('KUPX - ERROR EXIT FROM GQNT',2,1)
        KUPX=0
        RETURN
      END IF
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
