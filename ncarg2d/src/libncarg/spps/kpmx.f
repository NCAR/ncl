C
C	$Id: kpmx.f,v 1.1.1.1 1992-04-17 22:32:28 ncargd Exp $
C
      FUNCTION KPMX (IX)
C
C Given an x coordinate IX in the plotter system, KPMX(IX) is an x
C coordinate in the metacode system.
C
      COMMON /IUTLCM/ LL,MI,MX,MY,IU(96)
      SAVE /IUTLCM/
      KPMX=IFIX(32767.*FLOAT(IX-1)/(2.**MX-1.))
      RETURN
      END
