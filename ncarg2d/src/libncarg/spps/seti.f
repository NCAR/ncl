C
C $Id: seti.f,v 1.3 1994-03-17 01:44:17 kennison Exp $
C
      SUBROUTINE SETI (IX,IY)
C
C Allows the user to set the parameters which determine the assumed size
C of the target plotter and therefore determine how user coordinates are
C to be mapped into plotter coordinates.
C
C Declare the common block containing the scaling information.
C
      COMMON /IUTLCM/ LL,MI,MX,MY,IU(96)
      SAVE /IUTLCM/
C
C Transfer the user's values into the common block.
C
      MX=MAX0(1,MIN0(15,IX))
      MY=MAX0(1,MIN0(15,IY))
C
      RETURN
C
      END
