C
C	$Id: gageti.f,v 1.2 1992-09-04 20:40:39 ncargd Exp $
C

      SUBROUTINE GAGETI (PNAM,IVAL)
C
        CHARACTER*(*) PNAM
C
C The subroutine GAGETI may be used to get GRIDAL parameters which have
C values of type INTEGER.
C
C Just convert it into a call to the routine GAGETR.
C
        CALL GAGETR (PNAM,RVAL)
        IVAL=INT(RVAL)
C
C Done.
C
        RETURN
C
      END
