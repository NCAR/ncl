C
C $Id: gaseti.f,v 1.3 1994-03-17 17:27:42 kennison Exp $
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
