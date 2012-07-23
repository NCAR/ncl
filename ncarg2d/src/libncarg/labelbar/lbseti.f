C
C $Id: lbseti.f,v 1.5.8.1 2010-03-17 20:51:57 brownrig Exp $
C                                                                      
C                Copyright (C)  2000
C        University Corporation for Atmospheric Research
C                All Rights Reserved
C
C The use of this Software is governed by a License Agreement.
C
      SUBROUTINE LBSETI (WHCH,IVAL)
C
        CHARACTER*(*) WHCH
C
C This subroutine is called to give a specified integer value to a
C specified parameter.
C
C WHCH is the name of the parameter whose value is to be set.
C
C IVAL is an integer variable containing the new value of the parameter.
C
C Declare the common block where internal parameters are stored.
C
        COMMON /LBCOMN/ ICBL,ICFL,ICLB,WOBL,WOFL,WOLB
        SAVE   /LBCOMN/
C
C Define a character temporary for use in forming error messages.
C
        CHARACTER*39 CTMP
C --------------------------------------------------------
C RLB 3/2010: Previously the integer parameter was converted to a float
C   and the work was delegated off to LBSETR. This provided a sort
C   of "automatic type conversion", allowing the user to set a real
C   parameter using either lbseti() or lbsetr(), as in:
C        CALL LBSETI ('xxx',-9999)
C     or
C        CALL LBSETR ('xxx',-9999.0)
C
C   Color-indices are now either encoded RGBa values, or indices as
C   before. RGBa values are typically large integer values, beyond the
C   precision of floats, and thus this delegation scheme no longer
C   works correctly. The code has been refactored such that the integer
C   cases are now handled directly herein. If no action is found for
C   the WHCH, then we delegate over to LBSETR.
C -------------------------------------------------------
C
C Do a call forcing a BLOCKDATA to be loaded from a binary library.
C
        CALL LBBLDA
C
C Check for an uncleared prior error.
C
        IF (ICFELL('LBSETI - UNCLEARED PRIOR ERROR',1).NE.0) RETURN
C
C Check for a parameter name that is too short.
C
        IF (LEN(WHCH).LT.3) THEN
          CTMP(1:36)='LBSETI - PARAMETER NAME TOO SHORT - '
          CTMP(37:36+LEN(WHCH))=WHCH
          CALL SETER (CTMP(1:36+LEN(WHCH)),2,1)
          RETURN
        END IF
C
C Set the appropriate parameter value.
C
        IF      (WHCH(1:3).EQ.'CBL'.OR.WHCH(1:3).EQ.'cbl') THEN
          ICBL=MAX(-1,IVAL)
        ELSE IF (WHCH(1:3).EQ.'CFL'.OR.WHCH(1:3).EQ.'cfl') THEN
          ICFL=MAX(-1,IVAL)
        ELSE IF (WHCH(1:3).EQ.'CLB'.OR.WHCH(1:3).EQ.'clb') THEN
          ICLB=MAX(-1,IVAL)
        ELSE
C         Float the integer value and pass it on to LBSETR.
          CALL LBSETR (WHCH,REAL(IVAL))
          IF (ICFELL('LBSETI',2).NE.0) RETURN
        END IF
C
C Done.
C
        RETURN
C
      END
