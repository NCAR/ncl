C
C	$Id: cpuy.f,v 1.1.1.1 1992-04-17 22:32:27 ncargd Exp $
C
      FUNCTION CPUY (IY)
C
C Given a y coordinate IY in the plotter system, CPUY(IY) is a y
C coordinate in the user system.
C
      COMMON /IUTLCM/ LL,MI,MX,MY,IU(96)
      SAVE /IUTLCM/
      DIMENSION WD(4),VP(4)
      CALL GQCNTN (IE,NT)
      CALL GQNT (NT,IE,WD,VP)
      I=3
      IF (MI.EQ.2.OR.MI.GE.4) I=4
      CPUY=WD(I)+(FLOAT(IY-1)/(2.**MY-1.)-VP(3))/(VP(4)-VP(3))*
     +     (WD(7-I)-WD(I))
      IF (LL.EQ.2.OR.LL.GE.4) CPUY=10.**CPUY
      RETURN
      END
