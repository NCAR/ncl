C
C	$Id: kfmx.f,v 1.1.1.1 1992-04-17 22:32:28 ncargd Exp $
C
      FUNCTION KFMX (RX)
C
C Given an x coordinate RX in the fractional system, KFMX(RX) is an x
C coordinate in the metacode system.
C
      KFMX=IFIX(RX*32767.)
      RETURN
      END
