C
C $Id: cmfx.f,v 1.2 1993-12-12 20:54:52 kennison Exp $
C
      FUNCTION CMFX (IX)
C
C Given an X coordinate IX in the metacode system, CMFX(IX) is an X
C coordinate in the fractional system.
C
      CMFX=FLOAT(IX)/32767.
      RETURN
      END
