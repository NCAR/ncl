C
C	$Id: mxmy.f,v 1.1.1.1 1992-04-17 22:32:31 ncargd Exp $
C
      SUBROUTINE MXMY (IX,IY)
C
C Return to the user the coordinates of the current pen position, in the
C plotter coordinate system.
C
C In the common block PLTCM are recorded the coordinates of the last
C pen position, in the metacode coordinate system.
C
      COMMON /PLTCM/ JX,JY
      SAVE /PLTCM/
C
C Declare the common block containing the user state variables LL, MI,
C MX, and MY.
C
      COMMON /IUTLCM/ LL,MI,MX,MY,IU(96)
      SAVE /IUTLCM/
C
C Return to the user the plotter-system equivalents of the values in
C the metacode system.
C
      IX=1+IFIX((2.**MX-1.)*FLOAT(JX)/32767.)
      IY=1+IFIX((2.**MY-1.)*FLOAT(JY)/32767.)
C
C Done.
C
      RETURN
C
      END
