C
C	$Id: lbgeti.f,v 1.1.1.1 1992-04-17 22:32:57 ncargd Exp $
C
C
C-----------------------------------------------------------------------
C
      SUBROUTINE LBGETI (WHCH,IVAL)
C
        CHARACTER*(*) WHCH
C
C This subroutine is called to retrieve the integer value of a specified
C parameter.
C
C WHCH is the name of the parameter whose value is to be retrieved.
C
C IVAL is an integer variable in which the desired value is to be
C returned by LBGETI.
C
C
C Use LBGETR to retrieve the real value, fix it, and return it to the
C user.
C
        CALL LBGETR (WHCH,RVAL)
        IVAL=INT(RVAL)
C
C Done.
C
        RETURN
C
      END
