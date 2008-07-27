C
C $Id: kpmx.f,v 1.7 2008-07-27 00:17:24 haley Exp $
C
C                Copyright (C)  2000
C        University Corporation for Atmospheric Research
C                All Rights Reserved
C
C The use of this Software is governed by a License Agreement.
C
      FUNCTION KPMX (IX)
C
C Given an X coordinate IX in the plotter system, KPMX(IX) is an X
C coordinate in the metacode system.
C
      COMMON /IUTLCM/ LL,MI,MX,MY,IU(96)
      SAVE /IUTLCM/
      KPMX=INT(32767.*REAL(IX-1)/(2.**MX-1.))
      RETURN
      END
