C
C $Id: kfpx.f,v 1.2 1993-12-12 20:55:35 kennison Exp $
C
      FUNCTION KFPX (RX)
C
C Given an X coordinate RX in the fractional system, KFPX(RX) is an X
C coordinate in the plotter system.
C
      COMMON /IUTLCM/ LL,MI,MX,MY,IU(96)
      SAVE /IUTLCM/
      KFPX=1+IFIX(RX*(2.**MX-1.))
      RETURN
      END
