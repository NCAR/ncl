C
C $Id: cmfx.f,v 1.3 1994-03-17 01:43:08 kennison Exp $
C
      FUNCTION CMFX (IX)
C
C Given an X coordinate IX in the metacode system, CMFX(IX) is an X
C coordinate in the fractional system.
C
      CMFX=FLOAT(IX)/32767.
      RETURN
      END
