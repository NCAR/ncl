C
C	$Id: vvseti.f,v 1.2 1993-01-27 20:59:57 dbrown Exp $
C
C
C-----------------------------------------------------------------------
C
      SUBROUTINE VVSETI (CNM,IVL)
C
      CHARACTER*(*) CNM
C
C This subroutine is called to give a specified integer value to a
C specified parameter.
C
C CNM is the name of the parameter whose value is to be set.
C
C IVL is an integer variable containing the new value of the parameter.
C
C The real work is done by VVSETR
C
C Float the integer value and pass it on to VVSETR.
C
      RVL=REAL(IVL)
      CALL VVSETR (CNM,RVL)
C
C Done.
C
      RETURN
C
      END
