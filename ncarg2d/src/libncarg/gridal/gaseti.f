
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
