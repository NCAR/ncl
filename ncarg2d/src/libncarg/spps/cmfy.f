C
C $Id: cmfy.f,v 1.2 1993-12-12 20:54:54 kennison Exp $
C
      FUNCTION CMFY (IY)
C
C Given a Y coordinate IY in the metacode system, CMFY(IY) is a Y
C coordinate in the fractional system.
C
      CMFY=FLOAT(IY)/32767.
      RETURN
      END
