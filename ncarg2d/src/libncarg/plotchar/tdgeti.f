C
C $Id: tdgeti.f,v 1.2 1994-03-09 23:24:13 kennison Exp $
C
      SUBROUTINE TDGETI (PNAM,IVAL)
C
      CHARACTER*(*) PNAM
C
C The subroutine TDGETI may be used to get TDPACK parameters which have
C values of type INTEGER.
C
C Just convert it into a call to the routine TDGETR.
C
      CALL TDGETR (PNAM,RVAL)
      IF (ICFELL('TDGETI',1).NE.0) THEN
        IVAL=0
        RETURN
      END IF
      IVAL=INT(RVAL)
C
C Done.
C
      RETURN
C
      END
