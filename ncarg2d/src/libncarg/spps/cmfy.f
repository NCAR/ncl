C
C $Id: cmfy.f,v 1.3 1994-03-17 01:43:10 kennison Exp $
C
      FUNCTION CMFY (IY)
C
C Given a Y coordinate IY in the metacode system, CMFY(IY) is a Y
C coordinate in the fractional system.
C
      CMFY=FLOAT(IY)/32767.
      RETURN
      END
