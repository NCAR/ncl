C
C $Id: kpmx.f,v 1.2 1993-12-12 20:55:44 kennison Exp $
C
      FUNCTION KPMX (IX)
C
C Given an X coordinate IX in the plotter system, KPMX(IX) is an X
C coordinate in the metacode system.
C
      COMMON /IUTLCM/ LL,MI,MX,MY,IU(96)
      SAVE /IUTLCM/
      KPMX=IFIX(32767.*FLOAT(IX-1)/(2.**MX-1.))
      RETURN
      END
