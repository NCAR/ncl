C
C $Id: cpfx.f,v 1.3 1994-03-17 01:43:15 kennison Exp $
C
      FUNCTION CPFX (IX)
C
C Given an X coordinate IX in the plotter system, CPFX(IX) is an X
C coordinate in the fractional system.
C
      COMMON /IUTLCM/ LL,MI,MX,MY,IU(96)
      SAVE /IUTLCM/
      CPFX=FLOAT(IX-1)/(2.**MX-1.)
      RETURN
      END
