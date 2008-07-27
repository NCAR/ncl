C
C $Id: drawi.f,v 1.5 2008-07-27 00:17:15 haley Exp $
C
C                Copyright (C)  2000
C        University Corporation for Atmospheric Research
C                All Rights Reserved
C
C The use of this Software is governed by a License Agreement.
C
      SUBROUTINE DRAWI (IXA,IYA,IXB,IYB)
C
C This routine is included for use by PWRZI.
C
      CALL ISPLTF (REAL(IXA)/32767.,REAL(IYA)/32767.,1)
      CALL ISPLTF (REAL(IXB)/32767.,REAL(IYB)/32767.,2)
      RETURN
      END
