C
C	$Id: cpux.f,v 1.1.1.1 1992-04-17 22:32:27 ncargd Exp $
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
      CALL GQNT (NT,IE,WD,VP)
      I=1
      IF (MI.GE.3) I=2
      CPUX=WD(I)+(FLOAT(IX-1)/(2.**MX-1.)-VP(1))/(VP(2)-VP(1))*
     +           (WD(3-I)-WD(I))
      IF (LL.GE.3) CPUX=10.**CPUX
      RETURN
      END
