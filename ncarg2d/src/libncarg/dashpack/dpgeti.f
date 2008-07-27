C
C $Id: dpgeti.f,v 1.6 2008-07-27 00:16:59 haley Exp $
C                                                                      
C                Copyright (C)  2000
C        University Corporation for Atmospheric Research
C                All Rights Reserved
C
C The use of this Software is governed by a License Agreement.
C
      SUBROUTINE DPGETI (PNAM,IVAL)
C
C This routine is used to get the integral value of an internal
C parameter of type INTEGER or REAL.
C
        CHARACTER*(*) PNAM
C
C Check for an uncleared prior error.
C
        IF (ICFELL('DPGETI - UNCLEARED PRIOR ERROR',1).NE.0) RETURN
C
C Pass the buck to DPGETR.
C
        CALL DPGETR (PNAM,RVAL)
        IF (ICFELL('DPGETI',2).NE.0) RETURN
C
        IVAL=INT(RVAL)
C
C Done.
C
        RETURN
C
      END
