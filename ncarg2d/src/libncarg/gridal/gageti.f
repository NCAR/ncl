C
C $Id: gageti.f,v 1.3 1994-03-17 17:27:35 kennison Exp $
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
