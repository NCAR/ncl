C
C       $Id: vvgeti.f,v 1.3 1993-12-03 21:27:38 kennison Exp $
C
C
C-----------------------------------------------------------------------
C
      SUBROUTINE VVGETI (CNM,IVL)
C
      CHARACTER*(*) CNM
C
C This subroutine is called to retrieve the integer value of a specified
C parameter.
C
C CNM is the name of the parameter whose value is to be retrieved.
C
C IVL is an integer variable in which the desired value is to be
C returned by VVGETI.
C
C
C Use VVGETR to retrieve the real value, fix it, and return it to the
C user.
C
      CALL VVGETR (CNM,RVL)
      IVL=INT(RVL)
C
C Done.
C
      RETURN
C
      END
