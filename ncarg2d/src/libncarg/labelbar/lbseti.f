C
C	$Id: lbseti.f,v 1.1.1.1 1992-04-17 22:32:58 ncargd Exp $
C
C
C-----------------------------------------------------------------------
C
      SUBROUTINE LBSETI (WHCH,IVAL)
C
        CHARACTER*(*) WHCH
C
C This subroutine is called to give a specified integer value to a
C specified parameter.
C
C WHCH is the name of the parameter whose value is to be set.
C
C IVAL is an integer variable containing the new value of the parameter.
C
C
C Float the integer value and pass it on to LBSETR.
C
          CALL LBSETR (WHCH,REAL(IVAL))
C
C Done.
C
        RETURN
C
      END
