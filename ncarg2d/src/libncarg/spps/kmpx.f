C
C	$Id: kmpx.f,v 1.1.1.1 1992-04-17 22:32:28 ncargd Exp $
C
      FUNCTION KMPX (IX)
C
C Given an x coordinate IX in the metacode system, KMPX(IX) is an x
C coordinate in the plotter system.
C
      COMMON /IUTLCM/ LL,MI,MX,MY,IU(96)
      SAVE /IUTLCM/
      KMPX=1+IFIX((2.**MX-1.)*FLOAT(IX)/32767.)
      RETURN
      END
