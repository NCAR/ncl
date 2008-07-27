C
C $Id: dpseti.f,v 1.6 2008-07-27 00:16:59 haley Exp $
C                                                                      
C                Copyright (C)  2000
C        University Corporation for Atmospheric Research
C                All Rights Reserved
C
C The use of this Software is governed by a License Agreement.
C
      SUBROUTINE DPSETI (PNAM,IVAL)
C
C This routine, given an integer value, sets the value of an internal
C parameter of type INTEGER or REAL to that value.
C
        CHARACTER*(*) PNAM
C
C Check for an uncleared prior error.
C
        IF (ICFELL('DPSETI - UNCLEARED PRIOR ERROR',1).NE.0) RETURN
C
C Pass the buck to DPSETR.
C
        CALL DPSETR (PNAM,REAL(IVAL))
        IF (ICFELL('DPSETI',2).NE.0) RETURN
C
C Done.
C
        RETURN
C
      END
