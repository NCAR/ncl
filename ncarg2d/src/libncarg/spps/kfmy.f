C
C $Id: kfmy.f,v 1.2 1993-12-12 20:55:33 kennison Exp $
C
      FUNCTION KFMY (RY)
C
C Given a Y coordinate RY in the fractional system, KFMY(RY) is a Y
C coordinate in the metacode system.
C
      KFMY=IFIX(RY*32767.)
      RETURN
      END
