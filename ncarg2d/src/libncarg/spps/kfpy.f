C
C $Id: kfpy.f,v 1.2 1993-12-12 20:55:37 kennison Exp $
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
