C
C $Id: seti.f,v 1.4 1994-08-24 16:34:24 kennison Exp $
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
C Check for an uncleared prior error.
C
      IF (ICFELL('SETI - UNCLEARED PRIOR ERROR',1).NE.0) RETURN
C
C Transfer the user's values into the common block.
C
      MX=MAX0(1,MIN0(15,IX))
      MY=MAX0(1,MIN0(15,IY))
C
      RETURN
C
      END
