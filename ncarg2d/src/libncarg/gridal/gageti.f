C
C $Id: gageti.f,v 1.6 2008-07-27 00:17:13 haley Exp $
C                                                                      
C                Copyright (C)  2000
C        University Corporation for Atmospheric Research
C                All Rights Reserved
C
C The use of this Software is governed by a License Agreement.
C
      SUBROUTINE GAGETI (PNAM,IVAL)
C
        CHARACTER*(*) PNAM
C
C The subroutine GAGETI may be used to get GRIDAL parameters which have
C values of type INTEGER.
C
C Check for an uncleared prior error.
C
        IF (ICFELL('GAGETI - UNCLEARED PRIOR ERROR',1).NE.0) RETURN
C
C Just convert the call into a call to the routine GAGETR.
C
        CALL GAGETR (PNAM,RVAL)
        IF (ICFELL('GAGETI',2).NE.0) RETURN
        IVAL=INT(RVAL)
C
C Done.
C
        RETURN
C
      END
