C
C	$Id: cpfy.f,v 1.1.1.1 1992-04-17 22:32:27 ncargd Exp $
C
      FUNCTION CPFY (IY)
C
C Given a y coordinate IY in the plotter system, CPFY(IY) is a y
C coordinate in the fractional system.
C
      COMMON /IUTLCM/ LL,MI,MX,MY,IU(96)
      SAVE /IUTLCM/
      CPFY=FLOAT(IY-1)/(2.**MY-1.)
      RETURN
      END
