C
C $Id: seti.f,v 1.8 2008-07-27 00:17:25 haley Exp $
C
C                Copyright (C)  2000
C        University Corporation for Atmospheric Research
C                All Rights Reserved
C
C The use of this Software is governed by a License Agreement.
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
      MX=MAX(1,MIN(15,IX))
      MY=MAX(1,MIN(15,IY))
C
      RETURN
C
      END
