C
C $Id: kfpy.f,v 1.3 1994-03-17 01:43:43 kennison Exp $
C
      FUNCTION KFPY (RY)
C
C Given a Y coordinate RY in the fractional system, KFPY(RY) is a Y
C coordinate in the plotter system.
C
      COMMON /IUTLCM/ LL,MI,MX,MY,IU(96)
      SAVE /IUTLCM/
      KFPY=1+IFIX(RY*(2.**MX-1.))
      RETURN
      END
