C
C $Id: kumx.f,v 1.3 1994-03-17 01:43:53 kennison Exp $
C
      FUNCTION KUMX (RX)
C
C Given an X coordinate RX in the user system, KUMX(RX) is an X
C coordinate in the metacode system.
C
      COMMON /IUTLCM/ LL,MI,MX,MY,IU(96)
      SAVE /IUTLCM/
      DIMENSION WD(4),VP(4)
      CALL GQCNTN (IE,NT)
      IF (IE.NE.0) THEN
        CALL SETER ('KUMX - ERROR EXIT FROM GQCNTN',1,1)
        KUMX=0
        RETURN
      END IF
      CALL GQNT (NT,IE,WD,VP)
      IF (IE.NE.0) THEN
        CALL SETER ('KUMX - ERROR EXIT FROM GQNT',2,1)
        KUMX=0
        RETURN
      END IF
      I=1
      IF (MI.GE.3) I=2
      IF (LL.LE.2) THEN
        KUMX=IFIX(((RX-WD(I))/(WD(3-I)-WD(I))*(VP(2)-VP(1))+VP(1))*
     +              32767.)
      ELSE
        KUMX=IFIX(((ALOG10(RX)-WD(I))/(WD(3-I)-WD(I))*(VP(2)-VP(1))+
     +              VP(1))*32767.)
      ENDIF
      RETURN
      END
