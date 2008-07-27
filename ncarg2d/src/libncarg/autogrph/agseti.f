C
C $Id: agseti.f,v 1.6 2008-07-27 00:14:35 haley Exp $
C
C                Copyright (C)  2000
C        University Corporation for Atmospheric Research
C                All Rights Reserved
C
C The use of this Software is governed by a License Agreement.
C
      SUBROUTINE AGSETI (TPID,IUSR)
C
      CHARACTER*(*) TPID
      DIMENSION FURA(1)
C
C The routine AGSETI may be used to set the integer-equivalent value of
C any single AUTOGRAPH control parameter.
C
      FURA(1)=REAL(IUSR)
      CALL AGSETP (TPID,FURA,1)
      RETURN
C
      END
