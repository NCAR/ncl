
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
