C
C	$Id: cpgeti.f,v 1.1.1.1 1992-04-17 22:32:44 ncargd Exp $
C
C
C-----------------------------------------------------------------------
C
      SUBROUTINE CPGETI (WHCH,IVAL)
C
      CHARACTER*(*) WHCH
C
C This subroutine is called to retrieve the integer value of a specified
C parameter.
C
C WHCH is the name of the parameter whose value is to be retrieved.
C
C IVAL is an integer variable in which the desired value is to be
C returned by CPGETI.
C
C
C Use CPGETR to retrieve the real value, fix it, and return it to the
C user.
C
      CALL CPGETR (WHCH,RVAL)
      IVAL=INT(RVAL)
C
C Done.
C
      RETURN
C
      END
