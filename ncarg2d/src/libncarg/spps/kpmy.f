C
C	$Id: kpmy.f,v 1.1.1.1 1992-04-17 22:32:28 ncargd Exp $
C
      FUNCTION KPMY (IY)
C
C Given a y coordinate IY in the plotter system, KPMY(IY) is a y
C coordinate in the metacode system.
C
      COMMON /IUTLCM/ LL,MI,MX,MY,IU(96)
      SAVE /IUTLCM/
      KPMY=IFIX(32767.*FLOAT(IY-1)/(2.**MY-1.))
      RETURN
      END
