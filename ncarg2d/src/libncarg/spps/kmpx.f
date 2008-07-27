C
C $Id: kmpx.f,v 1.7 2008-07-27 00:17:24 haley Exp $
C
C                Copyright (C)  2000
C        University Corporation for Atmospheric Research
C                All Rights Reserved
C
C The use of this Software is governed by a License Agreement.
C
      FUNCTION KMPX (IX)
C
C Given an X coordinate IX in the metacode system, KMPX(IX) is an X
C coordinate in the plotter system.
C
      COMMON /IUTLCM/ LL,MI,MX,MY,IU(96)
      SAVE /IUTLCM/
      KMPX=1+INT((2.**MX-1.)*REAL(IX)/32767.)
      RETURN
      END
