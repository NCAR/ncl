C
C $Id: tdgeti.f,v 1.1 1997-06-18 22:08:26 kennison Exp $
C
      SUBROUTINE TDGETI (PNAM,IVAL)
C
      CHARACTER*(*) PNAM
C
C The subroutine TDGETI may be used to get TDPACK parameters which have
C values of type INTEGER.
C
C Check for an uncleared prior error.
C
        IF (ICFELL('TDGETI - UNCLEARED PRIOR ERROR',1).NE.0) RETURN
C
C Just convert it into a call to the routine TDGETR.
C
        CALL TDGETR (PNAM,RVAL)
        IF (ICFELL('TDGETI',2).NE.0) RETURN
        IVAL=INT(RVAL)
C
C Done.
C
        RETURN
C
      END
