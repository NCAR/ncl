C
C	$Id: sfgeti.f,v 1.1.1.1 1992-04-17 22:32:55 ncargd Exp $
C
C
C-----------------------------------------------------------------------
C
      SUBROUTINE SFGETI (CNP,IVP)
C
      CHARACTER*(*) CNP
C
C This subroutine is called to retrieve the integer value of a specified
C parameter.
C
C CNP is the name of the parameter whose value is to be retrieved.
C
C IVP is an integer variable in which the desired value is to be
C returned by SFGETI.
C
C
C Use SFGETR to retrieve the real value, fix it, and return it to the
C user.
C
      CALL SFGETR (CNP,RVP)
      IVP=INT(RVP)
C
C Done.
C
      RETURN
C
      END
