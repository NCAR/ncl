C
C	$Id: stgeti.f,v 1.1 1993-01-15 23:53:27 dbrown Exp $
C
C
C-----------------------------------------------------------------------
C
      SUBROUTINE STGETI (WHCH,IVAL)
C
      CHARACTER*(*) WHCH
C
C This subroutine is called to retrieve the integer value of a specified
C parameter.
C
C WHCH is the name of the parameter whose value is to be retrieved.
C
C IVAL is an integer variable in which the desired value is to be
C returned by STGETI.
C
C
C Use STGETR to retrieve the real value, fix it, and return it to the
C user.
C
      CALL STGETR (WHCH,RVAL)
      IVAL=INT(RVAL)
C
C Done.
C
      RETURN
C
      END
