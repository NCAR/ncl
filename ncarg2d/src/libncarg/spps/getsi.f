C
C $Id: getsi.f,v 1.2 1993-12-12 20:55:26 kennison Exp $
C
      SUBROUTINE GETSI (IX,IY)
C
C Return to the user the parameters which determine the assumed size of
C the target plotter and therefore determine how user coordinates are
C to be mapped into plotter coordinates.
C
C Declare the common block containing the scaling information.
C
      COMMON /IUTLCM/ LL,MI,MX,MY,IU(96)
      SAVE /IUTLCM/
C
C Set the user variables.

      IX=MX
      IY=MY
C
      RETURN
C
      END
