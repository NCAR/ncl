C
C $Id: cpfx.f,v 1.7 2008-07-27 00:17:23 haley Exp $
C
C                Copyright (C)  2000
C        University Corporation for Atmospheric Research
C                All Rights Reserved
C
C The use of this Software is governed by a License Agreement.
C
      FUNCTION CPFX (IX)
C
C Given an X coordinate IX in the plotter system, CPFX(IX) is an X
C coordinate in the fractional system.
C
      COMMON /IUTLCM/ LL,MI,MX,MY,IU(96)
      SAVE /IUTLCM/
      CPFX=REAL(IX-1)/(2.**MX-1.)
      RETURN
      END
