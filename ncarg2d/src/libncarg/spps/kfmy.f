C
C $Id: kfmy.f,v 1.3 1994-03-17 01:43:39 kennison Exp $
C
      FUNCTION KFMY (RY)
C
C Given a Y coordinate RY in the fractional system, KFMY(RY) is a Y
C coordinate in the metacode system.
C
      KFMY=IFIX(RY*32767.)
      RETURN
      END
