C
C $Id: tdseti.f,v 1.1 1992-11-17 18:47:28 kennison Exp $
C
      SUBROUTINE TDSETI (PNAM,IVAL)
C
      CHARACTER*(*) PNAM
C
C The subroutine TDSETI may be used to set TDPACK parameters which have
C values of type INTEGER.
C
C Just convert it into a call to the routine TDSETR.
C
      CALL TDSETR (PNAM,REAL(IVAL))
C
C Done.
C
      RETURN
C
      END
