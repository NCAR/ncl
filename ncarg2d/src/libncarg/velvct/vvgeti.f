C
C	$Id: vvgeti.f,v 1.1 1992-10-12 15:31:26 dbrown Exp $
C
C
C-----------------------------------------------------------------------
C
      SUBROUTINE VVGETI (WHCH,IVAL)
C
      CHARACTER*(*) WHCH
C
C This subroutine is called to retrieve the integer value of a specified
C parameter.
C
C WHCH is the name of the parameter whose value is to be retrieved.
C
C IVAL is an integer variable in which the desired value is to be
C returned by VVGETI.
C
C
C Use VVGETR to retrieve the real value, fix it, and return it to the
C user.
C
      CALL VVGETR (WHCH,RVAL)
      IVAL=INT(RVAL)
C
C Done.
C
      RETURN
C
      END
