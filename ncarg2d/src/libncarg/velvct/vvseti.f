C
C	$Id: vvseti.f,v 1.1 1992-10-12 15:32:31 dbrown Exp $
C
C
C-----------------------------------------------------------------------
C
      SUBROUTINE VVSETI (WHCH,IVAL)
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
C The real work is done by VVSETR
C
C Float the integer value and pass it on to VVSETR.
C
      RVAL=REAL(IVAL)
      CALL VVSETR (WHCH,RVAL)
C
C Done.
C
      RETURN
C
      END
