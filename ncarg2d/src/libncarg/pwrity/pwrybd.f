C
C	$Id: pwrybd.f,v 1.5 2008-07-27 00:17:21 haley Exp $
C                                                                      
C                Copyright (C)  2000
C        University Corporation for Atmospheric Research
C                All Rights Reserved
C
C The use of this Software is governed by a License Agreement.
C
      SUBROUTINE PWRYBD
C
C Calling this do-nothing subroutine forces "ld" to load the following
C block data routine (but only if they are in the same ".f" file).
C
        RETURN
C
      END
CNOSPLIT
      BLOCKDATA PWRYBDX
      COMMON /PWRCOM/ USABLE
      LOGICAL         USABLE
      DATA USABLE/.FALSE./
C REVISION HISTORY------
C FEBURARY 1979    CREATED NEW ALGORITHM PWRITY TO REPLACE PWRY
C                  ADDED REVISION HISTORY
C JUNE 1979        CHANGE ARGUMENT THETA IN PWRITY FROM FLOATING TO
C                  INTEGER, USING ITHETA AS THE NEW NAME.  ITS
C                  MEANING IS NOW DEGREES INSTEAD OF RADIANS.
C JULY 1984        CONVERTED TO FORTRAN 77 AND GKS
C-----------------------------------------------------------------------
      END
