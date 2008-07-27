C
C $Id: kmpy.f,v 1.7 2008-07-27 00:17:24 haley Exp $
C
C                Copyright (C)  2000
C        University Corporation for Atmospheric Research
C                All Rights Reserved
C
C The use of this Software is governed by a License Agreement.
C
      FUNCTION KMPY (IY)
C
C Given a Y coordinate IY in the metacode system, KMPY(IY) is a Y
C coordinate in the plotter system.
C
      COMMON /IUTLCM/ LL,MI,MX,MY,IU(96)
      SAVE /IUTLCM/
      KMPY=1+INT((2.**MY-1.)*REAL(IY)/32767.)
      RETURN
      END
