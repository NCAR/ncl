C
C $Id: mxmy.f,v 1.7 2008-07-27 00:17:24 haley Exp $
C
C                Copyright (C)  2000
C        University Corporation for Atmospheric Research
C                All Rights Reserved
C
C The use of this Software is governed by a License Agreement.
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
      IX=1+INT((2.**MX-1.)*REAL(JX)/32767.)
      IY=1+INT((2.**MY-1.)*REAL(JY)/32767.)
C
C Done.
C
      RETURN
C
      END
