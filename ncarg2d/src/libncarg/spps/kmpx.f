C
C $Id: kmpx.f,v 1.2 1993-12-12 20:55:40 kennison Exp $
C
      FUNCTION KMPX (IX)
C
C Given an X coordinate IX in the metacode system, KMPX(IX) is an X
C coordinate in the plotter system.
C
      COMMON /IUTLCM/ LL,MI,MX,MY,IU(96)
      SAVE /IUTLCM/
      KMPX=1+IFIX((2.**MX-1.)*FLOAT(IX)/32767.)
      RETURN
      END
