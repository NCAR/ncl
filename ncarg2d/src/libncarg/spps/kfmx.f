C
C $Id: kfmx.f,v 1.2 1993-12-12 20:55:31 kennison Exp $
C
      FUNCTION KFMX (RX)
C
C Given an X coordinate RX in the fractional system, KFMX(RX) is an X
C coordinate in the metacode system.
C
      KFMX=IFIX(RX*32767.)
      RETURN
      END
