C
C $Id: cufx.f,v 1.2 1993-12-12 20:55:10 kennison Exp $
C
      FUNCTION CUFX (RX)
C
C Given an X coordinate RX in the user system, CUFX(RX) is an X
C coordinate in the fractional system.
C
      COMMON /IUTLCM/ LL,MI,MX,MY,IU(96)
      SAVE /IUTLCM/
      DIMENSION WD(4),VP(4)
      CALL GQCNTN (IE,NT)
      IF (IE.NE.0) THEN
        CALL SETER ('CUFX - ERROR EXIT FROM GQCNTN',1,1)
        CUFX=0.
        RETURN
      END IF
      CALL GQNT (NT,IE,WD,VP)
      IF (IE.NE.0) THEN
        CALL SETER ('CUFX - ERROR EXIT FROM GQNT',2,1)
        CUFX=0.
        RETURN
      END IF
      I=1
      IF (MI.GE.3) I=2
      IF (LL.LE.2) THEN
        CUFX=(RX-WD(I))/(WD(3-I)-WD(I))*(VP(2)-VP(1))+VP(1)
      ELSE
        CUFX=(ALOG10(RX)-WD(I))/(WD(3-I)-WD(I))*(VP(2)-VP(1))+VP(1)
      ENDIF
      RETURN
      END
