C
C $Id: psym3.f,v 1.6 2008-07-27 00:17:34 haley Exp $
C
C                Copyright (C)  2000
C        University Corporation for Atmospheric Research
C                All Rights Reserved
C
C The use of this Software is governed by a License Agreement.
C
      SUBROUTINE PSYM3 (U,V,W,ICHAR,SIZE,IDIR,ITOP,IUP)
      IF (IUP .EQ. 2) CALL VECT3 (U,V,W)
      CALL PWRZ (U,V,W,ICHAR,1,SIZE,IDIR,ITOP,0)
      RETURN
      END
