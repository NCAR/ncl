C
C $Id: kupy.f,v 1.2 1993-12-12 20:55:56 kennison Exp $
C
      FUNCTION KUPY (RY)
C
C Given a Y coordinate RY in the user system, KUPY(RY) is a Y
C coordinate in the plotter system.
C
      COMMON /IUTLCM/ LL,MI,MX,MY,IU(96)
      SAVE /IUTLCM/
      DIMENSION WD(4),VP(4)
      CALL GQCNTN (IE,NT)
      IF (IE.NE.0) THEN
        CALL SETER ('KUPY - ERROR EXIT FROM GQCNTN',1,1)
        KUPY=0
        RETURN
      END IF
      CALL GQNT (NT,IE,WD,VP)
      IF (IE.NE.0) THEN
        CALL SETER ('KUPY - ERROR EXIT FROM GQNT',2,1)
        KUPY=0
        RETURN
      END IF
      I=3
      IF (MI.EQ.2.OR.MI.GE.4) I=4
      IF (LL.LE.1.OR.LL.EQ.3) THEN
        KUPY=1+IFIX(((RY-WD(I))/(WD(7-I)-WD(I))*(VP(4)-VP(3))+VP(3))*
     +              (2.**MY-1.))
      ELSE
        KUPY=1+IFIX(((ALOG10(RY)-WD(I))/(WD(7-I)-WD(I))*(VP(4)-VP(3))+
     +              VP(3))*(2.**MY-1.))
      ENDIF
      RETURN
      END
