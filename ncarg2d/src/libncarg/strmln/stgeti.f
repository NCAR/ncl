C
C	$Id: stgeti.f,v 1.3 1993-03-31 00:31:15 dbrown Exp $
C
C
C-----------------------------------------------------------------------
C
      SUBROUTINE STGETI (CNM,IVL)
C
      CHARACTER*(*) CNM
C
C This subroutine is called to retrieve the integer value of a specified
C parameter.
C
C CNM is the name of the parameter whose value is to be retrieved.
C
C IVL is an integer variable in which the desired value is to be
C returned by STGETI.
C
C ---------------------------------------------------------------------
C
C Use STGETR to retrieve the real value, fix it, and return it to the
C user.
C
      CALL STGETR (CNM,RVL)
      IVL=INT(RVL)
C
C Done.
C
      RETURN
C
      END
