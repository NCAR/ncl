C
C $Id: tdseti.f,v 1.1 1997-06-18 22:08:58 kennison Exp $
C
      SUBROUTINE TDSETI (PNAM,IVAL)
C
      CHARACTER*(*) PNAM
C
C The subroutine TDSETI may be used to set TDPACK parameters which have
C values of type INTEGER.
C
C Check for an uncleared prior error.
C
        IF (ICFELL('TDSETI - UNCLEARED PRIOR ERROR',1).NE.0) RETURN
C
C Just convert it into a call to the routine TDSETR.
C
        CALL TDSETR (PNAM,REAL(IVAL))
        IF (ICFELL('TDSETI',2).NE.0) RETURN
C
C Done.
C
        RETURN
C
      END
