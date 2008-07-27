C
C $Id: gaseti.f,v 1.6 2008-07-27 00:17:14 haley Exp $
C                                                                      
C                Copyright (C)  2000
C        University Corporation for Atmospheric Research
C                All Rights Reserved
C
C The use of this Software is governed by a License Agreement.
C
      SUBROUTINE GASETI (PNAM,IVAL)
C
        CHARACTER*(*) PNAM
C
C The subroutine GASETI may be used to set GRIDAL parameters which have
C values of type INTEGER.
C
C Check for an uncleared prior error.
C
        IF (ICFELL('GASETI - UNCLEARED PRIOR ERROR',1).NE.0) RETURN
C
C Just convert the call into a call to the routine GASETR.
C
        CALL GASETR (PNAM,REAL(IVAL))
        IF (ICFELL('GASETI',2).NE.0) RETURN
C
C Done.
C
        RETURN
C
      END
