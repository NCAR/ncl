C
C $Id: kpmy.f,v 1.3 1994-03-17 01:43:52 kennison Exp $
C
      FUNCTION KPMY (IY)
C
C Given a Y coordinate IY in the plotter system, KPMY(IY) is a Y
C coordinate in the metacode system.
C
      COMMON /IUTLCM/ LL,MI,MX,MY,IU(96)
      SAVE /IUTLCM/
      KPMY=IFIX(32767.*FLOAT(IY-1)/(2.**MY-1.))
      RETURN
      END
