C
C	$Id: stseti.f,v 1.1 1993-01-15 23:53:45 dbrown Exp $
C
C
C-----------------------------------------------------------------------
C
      SUBROUTINE STSETI (WHCH,IVAL)
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
C The real work is done by STSETR
C
C Float the integer value and pass it on to STSETR.
C
      RVAL=REAL(IVAL)
      CALL STSETR (WHCH,RVAL)
C
C Done.
C
      RETURN
C
      END
