C
C $Id: kpmy.f,v 1.2 1993-12-12 20:55:47 kennison Exp $
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
