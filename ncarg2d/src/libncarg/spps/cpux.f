C
C $Id: cpux.f,v 1.2 1993-12-12 20:55:06 kennison Exp $
C
      FUNCTION CPUX (IX)
C
C Given an x coordinate IX in the plotter system, CPUX(IX) is an x
C coordinate in the user system.
C
      COMMON /IUTLCM/ LL,MI,MX,MY,IU(96)
      SAVE /IUTLCM/
      DIMENSION WD(4),VP(4)
      CALL GQCNTN (IE,NT)
      IF (IE.NE.0) THEN
        CALL SETER ('CPUX - ERROR EXIT FROM GQCNTN',1,1)
        CPUX=0.
        RETURN
      END IF
      CALL GQNT (NT,IE,WD,VP)
      IF (IE.NE.0) THEN
        CALL SETER ('CPUX - ERROR EXIT FROM GQNT',2,1)
        CPUX=0.
        RETURN
      END IF
      I=1
      IF (MI.GE.3) I=2
      CPUX=WD(I)+(FLOAT(IX-1)/(2.**MX-1.)-VP(1))/(VP(2)-VP(1))*
     +           (WD(3-I)-WD(I))
      IF (LL.GE.3) CPUX=10.**CPUX
      RETURN
      END
