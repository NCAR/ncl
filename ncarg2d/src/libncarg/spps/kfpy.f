C
C	$Id: kfpy.f,v 1.1.1.1 1992-04-17 22:32:28 ncargd Exp $
C
      FUNCTION KFPY (RY)
C
C Given a y coordinate RY in the fractional system, KFPY(RY) is a y
C coordinate in the plotter system.
C
      COMMON /IUTLCM/ LL,MI,MX,MY,IU(96)
      SAVE /IUTLCM/
      KFPY=1+IFIX(RY*(2.**MX-1.))
      RETURN
      END
