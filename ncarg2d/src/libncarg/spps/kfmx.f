C
C $Id: kfmx.f,v 1.3 1994-03-17 01:43:37 kennison Exp $
C
      FUNCTION KFMX (RX)
C
C Given an X coordinate RX in the fractional system, KFMX(RX) is an X
C coordinate in the metacode system.
C
      KFMX=IFIX(RX*32767.)
      RETURN
      END
