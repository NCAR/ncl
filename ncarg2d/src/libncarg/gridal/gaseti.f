C
C	$Id: gaseti.f,v 1.2 1992-09-04 20:40:46 ncargd Exp $
C

      SUBROUTINE GASETI (PNAM,IVAL)
C
        CHARACTER*(*) PNAM
C
C The subroutine GASETI may be used to set GRIDAL parameters which have
C values of type INTEGER.
C
C Just convert it into a call to the routine GASETR.
C
        CALL GASETR (PNAM,REAL(IVAL))
C
C Done.
C
        RETURN
C
      END
