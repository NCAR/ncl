C
C $Id: tdgeti.f,v 1.1 1992-11-17 18:47:14 kennison Exp $
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
      IVAL=INT(RVAL)
C
C Done.
C
      RETURN
C
      END
