C
C $Id: getsi.f,v 1.8 2008-07-27 00:17:24 haley Exp $
C
C                Copyright (C)  2000
C        University Corporation for Atmospheric Research
C                All Rights Reserved
C
C The use of this Software is governed by a License Agreement.
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
