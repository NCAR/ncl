C
C $Id: kmpy.f,v 1.3 1994-03-17 01:43:48 kennison Exp $
C
      FUNCTION KMPY (IY)
C
C Given a Y coordinate IY in the metacode system, KMPY(IY) is a Y
C coordinate in the plotter system.
C
      COMMON /IUTLCM/ LL,MI,MX,MY,IU(96)
      SAVE /IUTLCM/
      KMPY=1+IFIX((2.**MY-1.)*FLOAT(IY)/32767.)
      RETURN
      END
