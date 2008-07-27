C
C $Id: lbseti.f,v 1.5 2008-07-27 00:17:17 haley Exp $
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
C
C Check for an uncleared prior error.
C
        IF (ICFELL('LBSETI - UNCLEARED PRIOR ERROR',1).NE.0) RETURN
C
C Float the integer value and pass it on to LBSETR.
C
          CALL LBSETR (WHCH,REAL(IVAL))
          IF (ICFELL('LBSETI',2).NE.0) RETURN
C
C Done.
C
        RETURN
C
      END
