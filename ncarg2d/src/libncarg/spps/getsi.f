C
C $Id: getsi.f,v 1.4 1994-08-24 16:34:23 kennison Exp $
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
C Check for an uncleared prior error.
C
      IF (ICFELL('GETSI - UNCLEARED PRIOR ERROR',1).NE.0) RETURN
C
C Set the user variables.
C
      IX=MX
      IY=MY
C
      RETURN
C
      END
