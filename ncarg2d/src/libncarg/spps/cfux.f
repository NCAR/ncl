C
C $Id: cfux.f,v 1.3 1994-03-17 01:43:02 kennison Exp $
C
      FUNCTION CFUX (RX)
C
C Given an X coordinate RX in the fractional system, CFUX(RX) is an X
C coordinate in the user system.
C
      COMMON /IUTLCM/ LL,MI,MX,MY,IU(96)
      SAVE /IUTLCM/
      DIMENSION WD(4),VP(4)
      CALL GQCNTN (IE,NT)
      IF (IE.NE.0) THEN
        CALL SETER ('CFUX - ERROR EXIT FROM GQCNTN',1,1)
        CFUX=0.
        RETURN
      END IF
      CALL GQNT (NT,IE,WD,VP)
      IF (IE.NE.0) THEN
        CALL SETER ('CFUX - ERROR EXIT FROM GQNT',2,1)
        CFUX=0.
        RETURN
      END IF
      I=1
      IF (MI.GE.3) I=2
      CFUX=WD(I)+(RX-VP(1))/(VP(2)-VP(1))*(WD(3-I)-WD(I))
      IF (LL.GE.3) CFUX=10.**CFUX
      RETURN
      END
