C
C $Id: cmux.f,v 1.2 1993-12-12 20:54:57 kennison Exp $
C
      FUNCTION CMUX (IX)
C
C Given an X coordinate IX in the metacode system, CMUX(IX) is an X
C coordinate in the user system.
C
      COMMON /IUTLCM/ LL,MI,MX,MY,IU(96)
      SAVE /IUTLCM/
      DIMENSION WD(4),VP(4)
      CALL GQCNTN (IE,NT)
      IF (IE.NE.0) THEN
        CALL SETER ('CMUX - ERROR EXIT FROM GQCNTN',1,1)
        CMUX=0.
        RETURN
      END IF
      CALL GQNT (NT,IE,WD,VP)
      IF (IE.NE.0) THEN
        CALL SETER ('CMUX - ERROR EXIT FROM GQNT',2,1)
        CMUX=0.
        RETURN
      END IF
      I=1
      IF (MI.GE.3) I=2
      CMUX=WD(I)+(FLOAT(IX)/32767.-VP(1))/(VP(2)-VP(1))*(WD(3-I)-WD(I))
      IF (LL.GE.3) CMUX=10.**CMUX
      RETURN
      END
